use embed_rust::embed_rust;

mod utils;

#[test]
fn git_example() {
    const RELATIVE_PATH_SIMPLE_EXAMPLE: &[u8] = embed_rust!({
        path: "projects/relative-path"
    });
    assert!(!RELATIVE_PATH_SIMPLE_EXAMPLE.is_empty());
    assert!(utils::contains(
        RELATIVE_PATH_SIMPLE_EXAMPLE,
        b"Hello world from relative path project!"
    ));
}
