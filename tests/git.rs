use embed_rust::embed_rust;

mod utils;

#[test]
fn git_example() {
    const GIT_SIMPLE_EXAMPLE: &[u8] = embed_rust!({
        git: "https://github.com/Abestanis/embed-rust.git"
    });
    assert!(!GIT_SIMPLE_EXAMPLE.is_empty());
    assert!(utils::contains(GIT_SIMPLE_EXAMPLE, b"compiler-artifact"));

    const GIT_COMPLEX_EXAMPLE: &[u8] = embed_rust!({
        git: { url: "https://github.com/Abestanis/embed-rust.git", path: "tests/git/project", branch: "feature/git" }
    });
    assert_ne!(GIT_SIMPLE_EXAMPLE, GIT_COMPLEX_EXAMPLE);
    assert!(!GIT_COMPLEX_EXAMPLE.is_empty());
    assert!(utils::contains(
        GIT_COMPLEX_EXAMPLE,
        b"Hello world from git project!"
    ));
}
