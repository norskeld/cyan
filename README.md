# cyan

[![Checks](https://img.shields.io/github/actions/workflow/status/norskeld/cyan/checks.yml?style=flat-square&colorA=22272d&colorB=22272d&label=checks)](https://github.com/norskeld/cyan/actions/workflows/checks.yml)

Compiler for a subset of C.

## Grammar

```ebnf
<program>     ::= <function>
<function>    ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
<block-item>  ::= <declaration> | <statement>
<declaration> ::= "int" <identifier> [ "=" <expression> ] ";"
<statement>   ::= "return" <expression> ";" | <expression> ";" | ";"
<expression>  ::= <factor> | <expression> <binary-op> <expression>
<factor>      ::= <int> | <identifier> | <unary-op> <factor> | "(" <expression> ")"
<unary-op>    ::= "-" | "~" | "!"
<binary-op>   ::= "+" | "-" | "*" | "/" | "%"
                | "<<" | ">>" | "&" | "|" | "^"
                | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="

<identifier>  ::= ? An identifier token ?
<int>         ::= ? A constant token ?
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
  | Function(identifier name, block_item* body)

block_item =
  | Declaration(declaration)
  | Statement(statement)

declaration =
  | Declaration(identifier name, expression? initializer)

statement =
  | Return(expression)
  | Expression(expression)
  | Null

expression =
  | Constant(int)
  | Var(identifier)
  | Unary(unary_op op, expression expression)
  | Binary(binary_op op, expression left, expression right)
  | Assignment(expression lvalue, expression rvalue)

unary_op =
  | BitNot
  | Negate
  | Not

binary_op =
  | Add
  | Div
  | Mod
  | Mul
  | Sub
  | BitAnd
  | BitOr
  | BitShl
  | BitShr
  | BitXor
  | And
  | Equal
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | NotEqual
  | Or
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
  | Copy(value src, value dst)
  | Jump(identifier)
  | JumpIfZero(value condition, identifier target)
  | JumpIfNotZero(value condition, identifier target)
  | Label(identifier)

value =
  | Constant(int)
  | Var(identifier)

unary_op =
  | BitNot
  | Negate
  | Not

binary_op =
  | Add
  | Div
  | Mod
  | Mul
  | Sub
  | BitAnd
  | BitOr
  | BitShl
  | BitShr
  | BitXor
  | Equal
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | NotEqual
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
  | Cmp(operand left, operand right)
  | Idiv(operand)
  | Jmp(identifier)
  | JmpCC(cond_code code, identifier target)
  | SetCC(cond_code code, operand dst)
  | Label(identifier)
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

cond_code =
  | E
  | NE
  | G
  | GE
  | L
  | LE

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
