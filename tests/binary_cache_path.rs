use embed_rust::embed_rust;

mod utils;

#[test]
fn binary_cache_example() {
    const BINARY_PATH_SIMPLE_EXAMPLE: &[u8] = embed_rust!({
        path: "projects/relative-path",
        binary_cache_path: "binaries/relative-path.bin",
    });
    assert!(!BINARY_PATH_SIMPLE_EXAMPLE.is_empty());
    assert!(utils::contains(
        BINARY_PATH_SIMPLE_EXAMPLE,
        b"Hello world from relative path project!"
    ));
    const BINARY_PATH_FALLBACK_EXAMPLE: &[u8] = embed_rust!({
        path: "projects/non-existent-path",
        binary_cache_path: "binaries/relative-path.bin",
    });
    assert_eq!(
        BINARY_PATH_SIMPLE_EXAMPLE, BINARY_PATH_FALLBACK_EXAMPLE,
        "Expected to fall back to the binary_cache_path"
    );
}
