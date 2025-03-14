# cyan

[![Checks](https://img.shields.io/github/actions/workflow/status/norskeld/cyan/checks.yml?style=flat-square&colorA=22272d&colorB=22272d&label=checks)](https://github.com/norskeld/cyan/actions/workflows/checks.yml)

C-to-Assembly (x86-64) compiler for a basic subset of C.

## Why

Simply to learn more about compilers, assembly, and how _not to_ design languages. :)

## Features

If something is missing in the list below, then it's not planned to be implemented.

- [x] Operators:
  - [x] Unary:
    - [x] Prefix (`--`, `++`, `!`, `~`, `-`)
    - [x] Postfix (`--`, `++`)
  - [x] Binary
    - [x] Arithmetic (`+`, `-`, `*`, `/`, `%`)
    - [x] Bitwise (`&`, `|`, `^`, `<<`, `>>`)
  - [x] Logical (`!`, `&&`, `||`)
  - [x] Relational (`<`, `<=`, `>`, `>=`, `==`, `!=`)
- [x] Local variables:
  - [x] Declaration
  - [x] Assignments
  - [x] Compound assignments (`+=`, `-=`, etc.)
  - [x] Scopes
- [ ] Storage-class specifiers:
  - [ ] `static`
  - [ ] `extern`
  - [ ] `typedef`
- [ ] Conditionals and control flow:
  - [x] If statements
  - [x] Ternary expressions
  - [x] Labeled statements
  - [ ] Switch statements
  - [x] `goto` statements
  - [x] `break` and `continue`
- [x] Loops:
  - [x] For loops
  - [x] While loops
  - [x] Do-while loops
- [ ] Functions:
  - [ ] Function declarations
  - [ ] Function definitions
  - [ ] Function calls
- [ ] Types:
  - [ ] `void`
  - [x] `int`
  - [ ] `long`
  - [ ] `unsigned int`
  - [ ] `unsigned long`
  - [ ] `double`
  - [ ] `char`
  - [ ] `signed char`
  - [ ] `unsigned char`
  - [ ] Structs
  - [ ] Unions
  - [ ] Pointers
  - [ ] Pointer arithmetic
  - [ ] Arrays
- [ ] Memory management:
  - [ ] `sizeof` operator
  - [ ] `malloc`
  - [ ] `calloc`
  - [ ] `realloc`
  - [ ] `aligned_alloc`
  - [ ] `free`

Optimizations:

- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Dead store elimination
- [ ] Copy propagation
- [ ] Register allocation
- [ ] Register coalescing

## Grammar

Defined using EBNF-like notation.

<details>
<summary>Definition</summary>

```ebnf
<program>     = <function>
<function>    = "int" <identifier> "(" "void" ")" <block>
<block>       = "{" { <block-item> } "}"
<block-item>  = <declaration> | <statement>
<declaration> = "int" <identifier> [ "=" <expression> ] ";"
<statement>   = "return" <expression> ";"
              | <expression> ";"
              | <identifier> ":" <statement>
              | "if" "(" <expression> ")" <statement> [ "else" <statement> ]
              | "break" ";"
              | "continue" ";"
              | "while" "(" <expression> ")" <statement>
              | "do" <statement> "while" "(" <expression> ")" ";"
              | "for" "(" <for-initializer> [ <expression> ] ";" [ <expression> ] ";" [ <expression> ] ")" <statement>
              | "goto" <identifier> ";"
              | <block>
              | ";"
<for-initializer> = <declaration> | [ <expression> ] ";"
<expression>  = <factor>
              | <expression> <binary-op> <expression>
              | <expression> "?" <expression> ":" <expression>
<factor>      = <unary-op> <factor> | <postfix>
<postfix>     = <primary> { <postfix-op> }
<primary>     = <int> | <identifier> | "(" <expression> ")"
<unary-op>    = "-" | "~" | "!" | "++" | "--"
<postfix-op>  = "++" | "--"
<binary-op>   = "+" | "-" | "*" | "/" | "%"
              | "<<" | ">>" | "&" | "|" | "^"
              | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="
              | "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="

<identifier>  = ? An identifier token ?
<int>         = ? A constant token ?
```
</details>

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
  | Function(identifier name, block body)

block =
  | Block(block_item*)

block_item =
  | Declaration(declaration)
  | Statement(statement)

declaration =
  | Declaration(identifier name, expression? initializer)

statement =
  | Break(identifier label)
  | Continue(identifier label)
  | While(expression condition, statement body, identifier label)
  | DoWhile(expression condition, statement body, identifier label)
  | For(for_initializer initializer, expression? condition, expression? post, statement body, identifier label)
  | Goto(identifier label)
  | Labeled(identifier label, statement statement)
  | Return(expression)
  | Expression(expression)
  | If(expression condition, statement then, statement? else)
  | Compound(block)
  | Null

for_initializer =
  | Declaration(declaration)
  | Expression(expression)
  | None

expression =
  | Constant(int)
  | Var(identifier)
  | Unary(unary_op op, expression expression)
  | Binary(binary_op op, expression left, expression right)
  | Postfix(postfix_op op, expression operand)
  | Ternary(expression condition, expression then, expression otherwise)
  | Assignment(expression lvalue, expression rvalue)
  | CompoundAssignment(binary_op op, expression lvalue, expression rvalue)

unary_op =
  | BitNot
  | Negate
  | Not
  | Inc
  | Dec

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
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | BitAndAssign
  | BitOrAssign
  | BitXorAssign
  | BitShlAssign
  | BitShrAssign

postfix_op =
  | Dec
  | Inc
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
