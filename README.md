# nfa_regex

Simple non finite automata regular expression (NFA regex) engine.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
nfa_regex = "1.0.0"
```

Add this to your crate root:

```rust
extern crate nfa_regex;
```

E.g. Create regex variable to match numeric type:

```
let mut r = nfa_regex::Regex::new("[\\+\\-]?[0-9]+(\\.[0-9]+)?([eE][\\+\\-]?[0-9]+)?").unwrap();
assert_eq!(r.match_pattern("-2.6e-6"), true);
assert_eq!(r.search_pattern_and_get_slice("pi = 3.141593, etc."), ("pi = ", "3.141593", ", etc."));
```

## Features

Supported regex operators:

```
Repeatition: +, *, ?, {m,n}
Or:          |
Not:         ^ (only used inside '[]')
Escape:      \
Bracket:     []
Grouping:    ()
```

Functions:
- Match pattern to input string.
- Search pattern into input string.
- Replace pattern found into input string.

## Releases

See [RELEASES.md](RELEASES.md)

## Compatibility

The `nfa_regex` crate is created and tested with rustc 1.51.0 and greater.

## License

Licensed under
 * [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed under Apache-2.0, without any additional terms or conditions.
