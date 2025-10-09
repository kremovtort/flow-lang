## Lexical structure

This section defines the tokenization rules: identifiers, keywords, literals, comments, operators, and separators.

### Character set and case
- Source files are UTF‑8 encoded.
- Identifiers and keywords are case-sensitive.

### Identifiers
- Term/type identifiers: `[A-Za-z_][A-Za-z0-9_]*`
- Module/type/trait/effect names may start with an uppercase letter (Rust-like convention), but casing is not semantically enforced.
- RefScope identifiers are prefixed with a single quote: `'[A-Za-z_][A-Za-z0-9_]*` (e.g., `'s`, `'global`, `'const`).

### Keywords (reserved)
The following words are reserved and cannot be used as identifiers:

`mod`, `use`, `pub`, `fn`, `struct`, `enum`, `type`, `trait`, `impl`, `effect`, `op`, `returning`, `handle`, `in`, `with`, `where`, `match`, `if`, `else`, `for`, `while`, `loop`, `break`, `continue`, `let`, `mut`, `true`, `false`, `as`.

Notes:
- Module declaration uses `mod` (not `module`).
- `as` is reserved for potential aliasing in `use` (see Modules), even if not all forms are used yet.

### Punctuators and operators (token inventory)
Common multi/uni-character tokens recognized by the lexer:

- Grouping: `(` `)` `[` `]` `{` `}`
- Separators: `,` `;` `:` `::` `.` `=>`
- Arrows and at: `->` `@` `@[`
- Assignment/comparison: `=` `==` `!=` `<` `<=` `>` `>=`
- Arithmetic: `+` `-` `*` `/` `%`
- Bitwise: `&` `|` `^` `<<` `>>`
- Boolean: `&&` `||` `!`
- Ranges: `..` `..=`
- Misc value-level: `++` (monoid-like concatenation, see Precedence)

Exact precedence and associativity: see [Precedence](./Precedence.md).

Special postfix operator:
- The sequence `.do` is parsed as a dedicated postfix execution operator (see Effects). Outside of the postfix form, `do` is not a keyword and may be used as an identifier.

### Literals
- Integer: decimal (`0`, `42`), hexadecimal (`0xFF`), binary (`0b1010`), octal (`0o755`). Optional `_` separators: `1_000_000`.
- Float: `3.14`, `1e9`, `1.0e-3`, with optional `_` separators.
- Boolean: `true`, `false`.
- Char: `'a'`, `'\n'` (single quotes, single Unicode scalar value).
- String: `"hello"`, supports common escapes (e.g. `\n`, `\t`, `\\`, `\"`).
- Unit: `()`.

### Comments and whitespace
- Line comments: `// ...` to end of line.
- Block comments: `/* ... */`, nestable.
- Whitespace is insignificant except as token separator.

### Semicolons and statements
- Statements are separated by `;`.
- In a block `{ ... }`, if the last item is not terminated by `;`, it is treated as an expression and becomes the block's value.


