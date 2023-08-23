use embed_rust::embed_rust;

mod utils;

#[test]
fn git_example() {
    const BINARY_PATH_SIMPLE_EXAMPLE: &[u8] = embed_rust!({
        path: "projects/relative-path",
        binary_cache_path: "binaries/relative-path.bin",
    });
    assert!(!BINARY_PATH_SIMPLE_EXAMPLE.is_empty());
    assert!(utils::contains(
        BINARY_PATH_SIMPLE_EXAMPLE,
        b"Hello world from relative path project!"
    ));
}
