# Last Order Logic

An experimental logical language.

Based on paper [Last Order Logic](https://github.com/advancedresearch/path_semantics/blob/master/papers-wip2/last-order-logic.pdf).

```text
=== Last Order Logic 0.2 ===
Type `help` for more information.
> a := 1 ~= 0
a := 1 ~= 0
LOL: Added `a` to definitions
> a ~ 0
a ~ 0
> ty
1
```

To run LOL from your Terminal, type:

```text
cargo install --example lolz last_order_logic
```

Then, to run:

```text
lolz
```

### How to learn LOL

To learn how to use LOL, type "help" in LOLZ.
This command lists all topics, e.g. "help path".

### Examples

- [Geometry Synthesis](https://github.com/advancedresearch/last_order_logic/blob/main/source/geom.lol.md)

### Motivation

In [First Order Logic](https://en.wikipedia.org/wiki/First-order_logic),
the truth values of quantified expressions depend on evaluation.
This means that an automated theorem prover must annotate expressions with their truth values
in order to operate efficiently under modifications to the source.
The user of the language has no direct access to this information.

Last Order Logic bridges the gap between usability and automated theorem proving.

- Increased readability and improved communication
- Efficient reuse of truth values
- Extensible to higher dimensional truth values

For example:

`∀ x { ... }` - It is not easy to see whether this is `true` or `false`.

With other words, First Order Logic is not computationally progressive.

Last Order Logic fixes this problem by having quantified expressions evaluate to themselves,
while the truth value is encoded in the type.

`∀ x : I { ... } : un(1)` - It is easy to see this is `true`.

Types are used to communicate intentions of programs.
Last Order Logic uses this feature to increase readability.

The `un(..)` syntax stands for "uniform" which is `un(1)` for `∀` and `un(0)` for `∃`.
Correspondingly, `nu(..)` stands for "non-uniform" which is `nu(1)` for `∃` and `nu(0)`
for `∀`.

Another reason is to express truth over paths, e.g. `un(0 ~= 1)`.
These are higher dimensional truth values, not expressible in First Order Logic.

The distinction between uniform and non-uniform sense of truth comes from the theory of
[Avatar Extensions](https://advancedresearch.github.io/avatar-extensions/summary.html).
Only non-uniform truth has a meaningful example that shows its truth value.

### File formats

- Regular text format (`.lol.txt`)
- Markdown text format (`.lol.md`)

The Markdown format is designed for readability:

- Must start with `#` (markdown title)
- A codeblock must use 3 backticks and `lol`

Special commands in the LOLZ environment are not supported.
To refer to LOLZ usage, use 3 backticks and `text`.
