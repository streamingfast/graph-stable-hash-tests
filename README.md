## `graph-node` Stable Hash Tests

This repository was used while porting The Graph `stable-hash` Rust crates to Golang. It contains exclusively test cases that showcase the behavior each components involved must have.

We test extensively

- [stable-hash](https://github.com/graphprotocol/stable-hash) Rust crate
- [BigDecimal](https://github.com/graphprotocol/graph-node/blob/master/graph/src/data/store/scalar.rs#L34) `graph-node` type wrapper (around [BigDecimal](https://github.com/ruby/bigdecimal) crate).

Technically, all those tests should go in their respective project. We should upstream them even if they overlap a little bit. At the same time, it was easier to have cross-links between here and tests within the Golang project since we control this repository. So, maybe it's better to keep them here without upstreaming.