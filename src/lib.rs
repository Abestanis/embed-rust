#![feature(proc_macro_span)]

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    env,
    fs::{self, File},
    io::{Read, Write},
    path::PathBuf,
    process::Command,
};

use path_slash::PathBufExt;
use proc_macro2::{Group, Span};
use quote::quote;
use serde::Deserialize;
use syn::{
    braced,
    token::{Brace, Comma},
    Error, Ident, LitStr,
};

const CARGO_DEPENDENCIES_SECTION: &str = "[dependencies]";
const COMPILER_ARTIFACT_MESSAGE_TYPE: &str = "compiler-artifact";

#[derive(Deserialize)]
struct CompilerArtifactMessage {
    reason: String,
    filenames: Vec<String>,
}

macro_rules! parse_options {
    ($input: expr, $key: ident, $args: ident, $argument_parser: block) => {
        parse_options!($input, $key, $args, $argument_parser, {}, false);
    };
    ($input: expr, $key: ident, $args: ident, $argument_parser: block, $string_argument_parser: block) => {
        parse_options!($input, $key, $args, $argument_parser, $string_argument_parser, true);
    };
    ($input: expr, $key: ident, $args: ident, $argument_parser: block, $string_argument_parser: block, $allow_string_arguments: expr) => {
        let mut seen_arguments = HashSet::new();
        let $args;
        let _: Brace = braced!($args in $input);
        while !$args.is_empty() {
            let lookahead = $args.lookahead1();
            if lookahead.peek(Ident) {
                let $key: Ident = $args.parse()?;
                let _colon: syn::token::Colon = $args.parse()?;
                if !seen_arguments.insert($key.to_string()) {
                    return Err(Error::new(
                        $key.span(),
                        format!("Duplicated parameter `{}`", $key),
                    ));
                }
                $argument_parser;
            } else if $allow_string_arguments && lookahead.peek(LitStr) {
                #[allow(unused_variables)]
                let $key: LitStr = $args.parse()?;
                let _colon: syn::token::Colon = $args.parse()?;
                $string_argument_parser;
            } else {
                return Err(lookahead.error());
            }
            let lookahead = $args.lookahead1();
            if lookahead.peek(Comma) {
                let _: Comma = $args.parse()?;
            } else if !$args.is_empty() {
                return Err(lookahead.error());
            }
        }
    };
}

#[derive(Debug)]
struct GitSource {
    url: String,
    branch: Option<String>,
    path: Option<PathBuf>,
}

#[derive(Debug)]
enum Source {
    Inline(Group),
    Git(GitSource),
}

/// Arguments for the [embed_rust] macro.
struct MatchTelecommandArgs {
    source: Source,
    extra_files: HashMap<PathBuf, String>,
    dependencies: String,
}

impl syn::parse::Parse for MatchTelecommandArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut source = None;
        let mut extra_files = HashMap::new();
        let mut dependencies = String::new();
        parse_options!(
            input,
            key,
            args,
            {
                match key.to_string().as_str() {
                    "source" => {
                        if source.is_some() {
                            return Err(Error::new(
                                key.span(),
                                format!(
                                    "Can only have one of `{key}`, `git` and `\"src/main.rs\"`"
                                ),
                            ));
                        }
                        source = Some(Source::Inline(args.parse()?))
                    }
                    "dependencies" => {
                        let dependencies_string: LitStr = args.parse()?;
                        dependencies = dependencies_string.value();
                    }
                    "git" => {
                        if source.is_some() {
                            return Err(Error::new(
                                key.span(),
                                format!(
                                    "Can only have one of source`, `{key}` and `\"src/main.rs\"`"
                                ),
                            ));
                        }
                        let lookahead = args.lookahead1();
                        source = Some(Source::Git(if lookahead.peek(LitStr) {
                            let url: LitStr = args.parse()?;
                            GitSource {
                                url: url.value(),
                                branch: None,
                                path: None,
                            }
                        } else if lookahead.peek(Brace) {
                            let mut url = None;
                            let mut branch = None;
                            let mut path = None;
                            parse_options!(args, key, git_args, {
                                match key.to_string().as_str() {
                                    "url" => {
                                        let url_literal: LitStr = git_args.parse()?;
                                        url = Some(url_literal.value());
                                    }
                                    "branch" => {
                                        let branch_literal: LitStr = git_args.parse()?;
                                        branch = Some(branch_literal.value());
                                    }
                                    "path" => {
                                        let path_literal: LitStr = git_args.parse()?;
                                        path = Some(PathBuf::from_slash(path_literal.value()));
                                    }
                                    _ => {
                                        return Err(Error::new(
                                            key.span(),
                                            format!("Invalid parameter `{key}`"),
                                        ))
                                    }
                                }
                            });
                            let Some(url) = url else {
                                return Err(Error::new(
                                    key.span(),
                                    format!("missing `url` key for `{key}` argument"),
                                ));
                            };
                            GitSource { url, branch, path }
                        } else {
                            return Err(lookahead.error());
                        }))
                    }
                    _ => return Err(Error::new(key.span(), format!("Invalid parameter `{key}`"))),
                }
            },
            {
                let key_value = key.value();
                let path = PathBuf::from_slash(key_value.clone());
                let extra_file_slot = match extra_files.entry(path) {
                    Entry::Vacant(entry) => entry,
                    Entry::Occupied(_) => {
                        return Err(Error::new(
                            key.span(),
                            format!("Duplicated file `{key_value}`"),
                        ))
                    }
                };
                match key_value.as_str() {
                    "src/main.rs" => {
                        if source.is_some() {
                            return Err(Error::new(
                                key.span(),
                                format!(
                                    "Can only have one of `source`, `git` and `\"{key_value}\"`"
                                ),
                            ));
                        }
                        source = Some(Source::Inline(args.parse()?))
                    }
                    _ => {
                        let content = if key_value.ends_with(".rs") {
                            let source: Group = args.parse()?;
                            let source = source.stream();
                            quote!(#source).to_string()
                        } else {
                            let lookahead = args.lookahead1();
                            if lookahead.peek(LitStr) {
                                let content: LitStr = args.parse()?;
                                content.value()
                            } else if lookahead.peek(Brace) {
                                let source: Group = args.parse()?;
                                let source = source.stream();
                                quote!(#source).to_string()
                            } else {
                                return Err(lookahead.error());
                            }
                        };
                        extra_file_slot.insert(content);
                    }
                }
            }
        );
        let Some(source) = source else {
            return Err(Error::new(Span::call_site(), "Missing `source` attribute"));
        };
        Ok(Self {
            source,
            extra_files,
            dependencies,
        })
    }
}

#[proc_macro]
pub fn embed_rust(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = syn::parse_macro_input!(tokens as MatchTelecommandArgs);
    let path = match compile_rust(args) {
        Ok(path) => path,
        Err(error) => return error.into_compile_error().into(),
    };
    let Some(path) = path.to_str() else {
        return Error::new(Span::call_site(), "Generated binary path contains invalid UTF-8").into_compile_error().into();
    };
    quote! {
        include_bytes!(#path)
    }
    .into()
}

fn compile_rust(args: MatchTelecommandArgs) -> syn::Result<PathBuf> {
    let call_site = Span::call_site().unwrap();
    let call_site_location = call_site.start();
    let source_file = call_site.source_file().path();
    let crate_dir = PathBuf::from(
        env::var("CARGO_MANIFEST_DIR")
            .expect("'CARGO_MANIFEST_DIR' environment variable is missing"),
    );
    let source_file = match source_file.strip_prefix(crate_dir) {
        Ok(path) => path.to_path_buf(),
        Err(_) => source_file,
    };
    let source_file_id =
        sanitize_filename::sanitize(source_file.to_string_lossy()).replace('.', "_");

    let line = call_site_location.line;
    let column = call_site_location.column;
    let id = format!("{source_file_id}_{line}_{column}");
    let mut generated_project_dir =
        PathBuf::from(env::var("OUT_DIR").expect("'OUT_DIR' environment variable is missing"))
            .join(id);
    let _ = fs::remove_dir_all(generated_project_dir.clone()); // Ignore errors about non-existent directories.
    if let Err(error) = fs::create_dir_all(generated_project_dir.clone()) {
        return Err(Error::new(
            Span::call_site(),
            format!("Failed to create embedded project directory: {error:?}"),
        ));
    }

    match args.source {
        Source::Inline(source) => {
            run_command(
                Command::new("cargo")
                    .current_dir(generated_project_dir.clone())
                    .arg("init"),
                "Failed to initialize embedded project crate",
            )?;
            let source_dir = generated_project_dir.join("src");
            let main_source_file = source_dir.join("main.rs");
            let main_source = source.stream();
            write_file(main_source_file, quote!(#main_source).to_string())?;
        }
        Source::Git(git_source) => {
            let mut clone_command = Command::new("git");
            let mut clone_command = clone_command.arg("clone").arg("--recurse-submodules");
            if let Some(branch) = git_source.branch {
                clone_command = clone_command.arg("--branch").arg(branch);
            }
            run_command(
                clone_command
                    .arg(git_source.url)
                    .arg(generated_project_dir.clone()),
                "Failed to clone embedded project",
            )?;
            if let Some(path) = git_source.path {
                generated_project_dir = generated_project_dir.join(path);
            }
        }
    }
    for (path, content) in args.extra_files.into_iter() {
        write_file(generated_project_dir.join(path), content)?;
    }
    if !args.dependencies.is_empty() {
        let cargo_file = generated_project_dir.join("Cargo.toml");
        let mut cargo_content = String::new();
        File::open(cargo_file.clone())
            .map_err(|error| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to open {cargo_file:?}: {error:?}"),
                )
            })?
            .read_to_string(&mut cargo_content)
            .map_err(|error| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to read {cargo_file:?}: {error:?}"),
                )
            })?;
        if !cargo_content.contains(CARGO_DEPENDENCIES_SECTION) {
            return Err(Error::new(
                Span::call_site(),
                "Generated Cargo.toml has no dependencies section",
            ));
        }
        cargo_content = cargo_content.replace(
            CARGO_DEPENDENCIES_SECTION,
            (CARGO_DEPENDENCIES_SECTION.to_owned() + "\n" + &args.dependencies).as_str(),
        );
        write_file(cargo_file, cargo_content)?;
    }

    let mut build_command = Command::new("cargo");
    let build_command = build_command
        .current_dir(generated_project_dir)
        .arg("build")
        .arg("--release");
    run_command(build_command, "Failed to build embedded project crate")?;

    // Find binary.
    let output = run_command(
        build_command.arg("--message-format").arg("json"),
        "Failed to build embedded project crate",
    )?;
    let Ok(output) = core::str::from_utf8(&output) else {
        return Err(Error::new(
            Span::call_site(),
            "Unable to parse cargo output: Invalid UTF-8",
        ));
    };
    let mut artifact_message = None;
    for line in output.lines() {
        if line.contains(COMPILER_ARTIFACT_MESSAGE_TYPE) {
            artifact_message = Some(line);
        }
    }
    let Some(artifact_message) = artifact_message else {
        return Err(Error::new(
            Span::call_site(),
            "Did not found an artifact message in cargo build output",
        ));
    };
    let artifact_message: CompilerArtifactMessage = serde_json::from_str(artifact_message)
        .map_err(|error| {
            Error::new(
                Span::call_site(),
                format!("Failed to parse artifact message from cargo: {error:?}"),
            )
        })?;
    if artifact_message.reason != COMPILER_ARTIFACT_MESSAGE_TYPE {
        return Err(Error::new(
            Span::call_site(),
            "Invalid cargo artifact message: Wrong reason",
        ));
    }
    let Some(artifact_path) = artifact_message.filenames.first() else {
        return Err(Error::new(
            Span::call_site(),
            "Invalid cargo artifact message: No artifact path",
        ));
    };

    Ok(PathBuf::from(artifact_path))
}

fn run_command(command: &mut Command, error_message: &str) -> syn::Result<Vec<u8>> {
    match command.output() {
        Ok(output) => {
            if !output.status.success() {
                Err(Error::new(
                    Span::call_site(),
                    format!(
                        "{error_message}: `{command:?}` failed with exit code {exit_code:?}\n# Stdout:\n{stdout}\n# Stderr:\n{stderr}",
                        exit_code = output.status.code(),
                        stdout = core::str::from_utf8(output.stdout.as_slice()).unwrap_or("<Invalid UTF-8>"),
                        stderr = core::str::from_utf8(output.stderr.as_slice()).unwrap_or("<Invalid UTF-8>")
                    ),
                ))
            } else {
                Ok(output.stdout)
            }
        }
        Err(error) => Err(Error::new(
            Span::call_site(),
            format!("{error_message}: `{command:?}` failed: {error:?}"),
        )),
    }
}

fn write_file(path: PathBuf, content: String) -> syn::Result<()> {
    if let Some(parent_dir) = path.parent() {
        if let Err(error) = fs::create_dir_all(parent_dir) {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to create parent directory for {path:?}: {error:?}"),
            ));
        }
    }
    let mut file = File::create(path.clone()).map_err(|error| {
        Error::new(
            Span::call_site(),
            format!("Failed to open {path:?}: {error:?}"),
        )
    })?;
    file.write_all(content.as_bytes()).map_err(|error| {
        Error::new(
            Span::call_site(),
            format!("Failed to write {path:?} to project: {error:?}"),
        )
    })?;
    Ok(())
}
