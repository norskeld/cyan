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
<binary-op>  ::= "+" | "-" | "*" | "/" | "%"

<identifier> ::= ? An identifier token ?
<int>        ::= ? A constant token ?
```

## Trees and IRs

> [!NOTE]
> AST, TAC and AAST are described using [Zephyr ASDL][zephyr].

### AST

This is a syntax tree representation of a C program.

```zephyr
program = Program(function)

function =
  | Function(identifier name, statement body)

statement =
  | Return(expression)

expression =
  | Constant(int)
  | Unary(unary_op operator, expression expression)
  | Binary(binary_op, expression left, expression right)

unary_op =
  | BitwiseNot
  | Negate

binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
```

### Three Address Code (TAC)

This IR lets us handle structural transformations — like removing nested expressions — separately from the details of assembly language, and it's also well suited for applying some compile-time optimizations.

```zephyr
program = Program(function)

function =
  | Function(identifier, instruction* instructions)

instruction =
  | Return(value)
  | Unary(unary_op, value src, value dst)
  | Binary(binary_op, value left, value right, value dst)

value =
  | Constant(int)
  | Var(identifier)

unary_op =
  | BitwiseNot
  | Negate

binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
```

### Assembly AST (AAST)

This IR is used to emit assembly code.

```zephyr
program = Program(function)

function =
  | Function(identifier name, instruction* instructions)

instruction =
  | Mov(operand src, operand dst)
  | Unary(unary_op operator, operand)
  | Binary(binary_op, operand, operand operand)
  | Idiv(operand)
  | AllocateStack(int)
  | Cdq
  | Ret

unary_op =
  | Neg
  | Not

binary_op =
  | Add
  | Subtract
  | Multiply

operand =
  | Imm(int)
  | Reg(reg)
  | Pseudo(identifier)
  | Stack(int)

reg =
  | AX
  | DX
  | R10
  | R11
```

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
