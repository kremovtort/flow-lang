## Operators: precedence and associativity

Precedence is broadly aligned with Rust, with Flow additions noted. Higher rows bind tighter.

1. Module access: `::` (left-to-right)
1. Postfix member access: `.` (left-to-right)
2. Call, indexing, tuple projection: `f(x)`, `a[b]`, `t.1` (left-to-right)
3. Unary: `!` `-` `*` `&` (right-to-left where applicable)
4. Multiplicative: `*` `/` `%` (left)
5. Additive: `+` `-` (left)
6. Concatenation/Monoid: `++` (left)
7. Shifts: `<<` `>>` (left)
8. Bitwise AND/XOR/OR: `&` `^` `|` (left)
9. Comparisons: `==` `!=` `<` `<=` `>` `>=` (non-associative)
10. Ranges: `..` `..=` (non-associative)
11. Boolean AND: `&&` (left)
12. Boolean OR: `||` (left)
13. Assignment and type ascription: `=` `: T` (right)
14. Arrows and effects: `->` `@[...]` (right, in type position)

Notes:
- `@[...]` appears in type position; it does not participate in value-level precedence.
- Tuple projection `.n` is treated like field access.
- Exact parsing of `: T` (type ascription) depends on context (declaration vs expression); value-level ascription is discouraged unless explicitly supported.


