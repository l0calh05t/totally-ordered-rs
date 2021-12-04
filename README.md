# Totally Ordered IEEE Float Comparison for Rust

[![crates.io](https://img.shields.io/crates/v/totally-ordered)](https://crates.io/crates/totally-ordered)
[![docs.rs](https://docs.rs/totally-ordered/badge.svg)](https://docs.rs/totally-ordered)
[![Rust](https://github.com/l0calh05t/totally-ordered-rs/actions/workflows/rust.yml/badge.svg)](https://github.com/l0calh05t/totally-ordered-rs/actions/workflows/rust.yml)

This crate adds the `TotallyOrderable` trait for `f32` and `f64` values as well as the ABI-transparent `TotallyOrdered` type which adds `Ord + Eq + Hash` to wrapped floating point values.
Main use case: sorting of floating-point arrays which may or may not contain not-a-numbers, infinities, and positive or negative zeros.

```rust
use totally_ordered::TotallyOrdered;
let mut values : [f64; 4] = [-0.0, 0.0, -1.0, 1.0];
TotallyOrdered::new_slice_mut(&mut values).sort();
```
