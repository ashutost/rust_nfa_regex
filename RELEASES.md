# Release 1.0.0 (2021-06-12)

- NFA regex engine.
- Basic regex features: Match, Search, Replace.
- See features under README.md

Known Issues:
- Implementation does not work for multibyte UTF-8 characters but only ASCII characters. In rust, string's character iterator does not return the index of char
  respecting its character boundry, which gives invalid index into str or String.
    E.g. "π = 3.141593, अ = -2.12e-10", The variable names are multibyte characters, the index returned by string's char iterator and actual index of char
    in string differs.
- The repeat counter requires regex variable creation to be mutable since the counter is part of regex engine.

**Author**: @ashgamer10
