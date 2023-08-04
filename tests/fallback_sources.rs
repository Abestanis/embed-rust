use embed_rust::embed_rust;

mod utils;

#[test]
fn fallback_to_git_example() {
    const FALLBACK_GIT_EXAMPLE: &[u8] = embed_rust!({
        path: "non_existent",
        git: "https://github.com/Abestanis/embed-rust.git"
    });
    assert!(!FALLBACK_GIT_EXAMPLE.is_empty());
    assert!(utils::contains(FALLBACK_GIT_EXAMPLE, b"compiler-artifact"));
}

#[test]
fn fallback_to_path_example() {
    const FALLBACK_PATH_EXAMPLE: &[u8] = embed_rust!({
        git: "https://github.com/Abestanis/embed-rust-non-existent.git",
        path: "projects/relative-path"
    });
    assert!(!FALLBACK_PATH_EXAMPLE.is_empty());
    assert!(utils::contains(
        FALLBACK_PATH_EXAMPLE,
        b"Hello world from relative path project!"
    ));
}
