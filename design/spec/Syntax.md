## Flow Language — Syntax Specification (Surface)

This document specifies the concrete surface syntax of the Flow language. It focuses on tokens, forms, and composition rules. Type system, effects semantics, and elaboration details are covered in dedicated design documents referenced throughout.

- Semantics and type-level details: see `design/drafts/features/Types.md`, `design/drafts/features/Effects.md`, `design/drafts/features/Constraints.md`, `design/drafts/features/Data types.md`, `design/drafts/features/Functions.md`, `design/drafts/features/Scoped mutability.md`.
- This document intentionally omits a formal EBNF grammar; a future EBNF will be provided as a separate artifact.

### Status of this document
Complete enough for day-to-day authoring. Some parts reference semantics documents for non-syntactic constraints.

### Contents
- [Lexical structure](./syntax/Lexical.md)
- [Modules and paths](./syntax/Modules.md)
- [Declarations (data, traits, effects, aliases)](./syntax/Declarations.md)
- [Traits and instances](./syntax/Traits.md)
- [Generics, where, quantified constraints](./syntax/Generics.md)
- [Effects and effect rows](./syntax/Effects.md)
- [Functions and methods](./syntax/FunctionsAndMethods.md)
- [Bindings and scopes](./syntax/Bindings.md)
- [Control flow statements](./syntax/ControlFlow.md)
- [Patterns and match](./syntax/Patterns.md)
- [RefScopes and references](./syntax/RefScopes.md)
- [Operators: precedence and associativity](./syntax/Precedence.md)
- [Future work: EBNF grammar](#future-work-ebnf)

### Design principles (syntactic scope)
- Syntax is Rust-like where reasonable; notable Flow-specific features include unified effect rows, higher-kinded type parameters via `F<_>`, quantified constraints, and explicit RefScopes with `'s`.
- Module keyword is `mod`.
- Statements are separated by semicolons `;`. The final non-terminated item in a block is an expression and yields the block value.

### Quick taste
```rust
mod math {
  pub fn add(a: i32, b: i32) -> i32 { a + b }
}

use math::add;

fn main() -> @[IO] () {
  let x = add(1, 2);
  if x > 2 { println("ok") } else { println("no") }
}
```

### Cross-references
- Types and universes: see `../drafts/features/Types.md`
- Effects and handlers: see `../drafts/features/Effects.md`
- Constraints and traits: see `../drafts/features/Constraints.md`

### Formal grammar (ABNF)
An ABNF grammar (RFC 5234) is provided in `design/spec/Grammar.abnf`. The lexical layer inside the grammar is high-level: details of string escapes, byte strings, and numeric formats are left to the lexer; see `syntax/Lexical.md`.


