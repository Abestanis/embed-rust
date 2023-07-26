use embed_rust::embed_rust;

mod utils;

#[test]
fn executes_post_build() {
    const BASIC_EXAMPLE: &[u8] = embed_rust!({
        source: {
            fn main() {
                println!("Hello world!");
            }
        },
        post_build: [
            ["cp", input_path, output_path] // Just copy the generated binary to the output path.
        ]
    });
    assert!(!BASIC_EXAMPLE.is_empty());
    assert!(utils::contains(BASIC_EXAMPLE, b"Hello world!"));
}
