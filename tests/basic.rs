use embed_rust::embed_rust;

mod utils;

#[test]
fn embeds_basic_example() {
    const BASIC_EXAMPLE: &[u8] = embed_rust!({
        source: {
            fn main() {
                println!("Hello world!");
            }
        },
    });
    assert!(!BASIC_EXAMPLE.is_empty());
    assert!(utils::contains(BASIC_EXAMPLE, b"Hello world!"));

    const BASIC_EXAMPLE_WITH_DIFFERENT_MESSAGE: &[u8] = embed_rust!({
        source: {
            fn main() {
                println!("Hello worlds!");
            }
        }
    });
    assert_ne!(BASIC_EXAMPLE, BASIC_EXAMPLE_WITH_DIFFERENT_MESSAGE);
    assert!(!BASIC_EXAMPLE_WITH_DIFFERENT_MESSAGE.is_empty());
    assert!(!utils::contains(
        BASIC_EXAMPLE_WITH_DIFFERENT_MESSAGE,
        b"Hello world!"
    ));
    assert!(utils::contains(
        BASIC_EXAMPLE_WITH_DIFFERENT_MESSAGE,
        b"Hello worlds!"
    ));
}
