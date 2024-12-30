# cyan

[![Checks](https://img.shields.io/github/actions/workflow/status/norskeld/cyan/checks.yml?style=flat-square&colorA=22272d&colorB=22272d&label=checks)](https://github.com/norskeld/cyan/actions/workflows/checks.yml)

Compiler for a subset of C.

## Grammar

```ebnf
<program>    ::= <function>
<function>   ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
<statement>  ::= "return" <expression> ";"
<expression> ::= <factor> | <expression> <binary-op> <expression>
<factor>     ::= <int> | <unary-op> <factor> | "(" <expression> ")"
<unary-op>   ::= "-" | "~"
<binary-op>  ::= "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^"

<identifier> ::= ? An identifier token ?
<int>        ::= ? A constant token ?
```

## Trees and IRs

> [!NOTE]
> AST, TAC and AAST are described using [Zephyr ASDL][zephyr].

### AST

This is a syntax tree representation of a C program.

<details>
<summary>Definition</summary>

```scala
program = Program(function function)

function =
  | Function(identifier name, statement body)

statement =
  | Return(expression)

expression =
  | Constant(int)
  | Unary(unary_op op, expression expression)
  | Binary(binary_op op, expression left, expression right)

unary_op =
  | BitNot
  | Negate

binary_op =
  | Add
  | BitAnd
  | BitOr
  | BitShl
  | BitShr
  | BitXor
  | Div
  | Mod
  | Mul
  | Sub
```
</details>

### Three Address Code (TAC)

This IR lets us handle structural transformations — like removing nested expressions — separately from the details of assembly language, and it's also well suited for applying some compile-time optimizations.

<details>
<summary>Definition</summary>

```scala
program = Program(function function)

function =
  | Function(identifier name, instruction* instructions)

instruction =
  | Return(value value)
  | Unary(unary_op op, value src, value dst)
  | Binary(binary_op op, value left, value right, value dst)

value =
  | Constant(int)
  | Var(identifier)

unary_op =
  | BitNot
  | Negate

binary_op =
  | Add
  | BitAnd
  | BitOr
  | BitShl
  | BitShr
  | BitXor
  | Div
  | Mod
  | Mul
  | Sub
```
</details>

### Assembly AST (AAST)

This IR is used to emit assembly code.

<details>
<summary>Definition</summary>

```scala
program = Program(function function)

function =
  | Function(identifier name, instruction* instructions)

instruction =
  | Mov(operand src, operand dst)
  | Unary(unary_op op, operand operand)
  | Binary(binary_op op, operand src, operand dst)
  | Idiv(operand)
  | AllocateStack(int)
  | Cdq
  | Ret

unary_op =
  | Neg
  | Not

binary_op =
  | Add
  | And
  | Mul
  | Or
  | Sal
  | Sar
  | Sub
  | Xor

operand =
  | Imm(int)
  | Reg(reg)
  | Pseudo(identifier)
  | Stack(int)

reg =
  | AX
  | CX
  | DX
  | R10
  | R11
```
</details>

## Links

- [C23 standard (working draft)](https://open-std.org/JTC1/SC22/WG14/www/docs/n3220.pdf)
- [x86-64 instruction set](https://www.felixcloutier.com/x86/)
- [x86 assembly](https://en.wikibooks.org/wiki/X86_Assembly)
- [System V ABI](https://gitlab.com/x86-psABIs/x86-64-ABI)
- [Zephyr ASDL][zephyr]

## License

[MIT](LICENSE).

<!-- Links. -->

[zephyr]: https://www.cs.princeton.edu/~appel/papers/asdl97.pdf
