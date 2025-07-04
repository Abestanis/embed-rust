# Embed Rust

A macro that allows to embed a Rust executable in another Rust program.
This can be useful for building small test payloads for other (e.g. embedded) targets.

Requires Rust `1.88` or later.

## Example

```rust
const BASIC_EXAMPLE: &[u8] = embed_rust!({
    source: {
        fn main() {
            println!("Hello world!");
        }
    },
});
```

For more examples see the [`tests`](tests) directory.
