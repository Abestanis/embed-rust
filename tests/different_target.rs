use embed_rust::embed_rust;

mod utils;

#[test]
fn embeds_embedded_example() {
    const EMBEDDED_TARGET_EXECUTABLE: &[u8] = embed_rust!({
        source: {
            #![no_std]
            #![no_main]

            use cortex_m_rt::entry;

            #[entry]
            fn main() -> ! {
                const MESSAGE: &str = concat!("This is the target: |", env!("TARGET"), "|");
                core::hint::black_box(MESSAGE);
                loop {
                    cortex_m::asm::nop();
                }
            }

            #[inline(never)]
            #[panic_handler]
            fn panic(_: &core::panic::PanicInfo) -> ! {
                for _ in 0..10000 {
                    cortex_m::asm::nop();
                }
                cortex_m::peripheral::SCB::sys_reset();

            }
        },
        "build.rs": {
            fn main() {
                println!("cargo:rustc-link-arg=-Tlink.x");
                println!(
                    "cargo:rustc-env=TARGET={}",
                    std::env::var("TARGET").unwrap()
                );
            }
        },
        "rust-toolchain.toml": r#"
            [toolchain]
            targets = [ "thumbv7em-none-eabihf" ]
        "#,
        ".cargo/config.toml": r#"
            [build]
            target = "thumbv7em-none-eabihf"
        "#,
        "memory.x": r#"
            MEMORY
            {
                FLASH : ORIGIN = 0x24000000, LENGTH = 128K
                RAM : ORIGIN = 0x24020000, LENGTH = 384K
            }

            _stack_start = ORIGIN(RAM) + LENGTH(RAM);
        "#,
        dependencies: r#"
            cortex-m = "0.7.5"
            cortex-m-rt = "0.7.1"
        "#
    });
    const _: () = assert!(!EMBEDDED_TARGET_EXECUTABLE.is_empty());
    assert!(EMBEDDED_TARGET_EXECUTABLE.starts_with(b"\x7FELF"));
    assert!(utils::contains(
        EMBEDDED_TARGET_EXECUTABLE,
        b"This is the target: |thumbv7em-none-eabihf|"
    ));
}
