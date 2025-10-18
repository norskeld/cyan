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
- [x] Conditionals and control flow:
  - [x] If statements
  - [x] Ternary expressions
  - [x] Labeled statements
  - [x] Switch statements
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
  - [x] `void`
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

Additional features:

- Use the [QBE](https://c9x.me/compile/) backend.
- Non-standard extensions to the language, like modules (not macros).

## Grammar

Defined using EBNF-like notation.

<details>
<summary>Definition</summary>

```ebnf
program
  = { function-declaration }

declaration
  = variable-declaration
  | function-declaration

function-declaration
  = "int" identifier "(" param-list ")" ( block | ";" )

variable-declaration
  = "int" identifier [ "=" expression ] ";"

param-list
  = "void"
  | "int" identifier { "," "int" identifier }

block
  = "{" { block-item } "}"

block-item
  = declaration
  | statement

statement
  = "return" expression ";"
  | expression ";"
  | identifier ":" statement
  | "if" "(" expression ")" statement [ "else" statement ]
  | "break" ";"
  | "continue" ";"
  | "switch" "(" expression ")" statement
  | "while" "(" expression ")" statement
  | "do" statement "while" "(" expression ")" ";"
  | "for" "(" initializer [ expression ] ";" [ expression ] ";" [ expression ] ")" statement
  | "goto" identifier ";"
  | <block>
  | ";"

initializer
  = variable-declaration
  | [ expression ] ";"

expression
  = factor
  | expression binary-op expression
  | expression "?" expression ":" expression

factor
  = unary-op factor
  | postfix

postfix
  = primary { postfix-op }

primary
  = int
  | identifier
  | "(" expression ")"
  | identifier "(" [ argument-list ] ")"

argument-list
  = expression { "," expression }

unary-op
  = "-" | "~" | "!" | "++" | "--"

postfix-op
  = "++" | "--"

binary-op
  = "+" | "-" | "*" | "/" | "%"
  | "<<" | ">>" | "&" | "|" | "^"
  | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="
  | "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="

identifier
  = ? An identifier token ?

int
  = ? A constant token ?
```
</details>

## Trees and IRs

The standard drill: programs get lexed, then parsed into AST, then lowered to TAC, then into AAST, which is then used to emit assembly.

### AST

This is used to represent the syntax tree of the program, and to perform semantic analysis.

### Three Address Code (TAC)

This IR stands between the AST and the assembly code, and will let us handle structural transformations separately from the details of assembly language (this is to be done), and it's also should be well suited for applying some compile-time optimizations (also to be done).

### Assembly AST (AAST)

This IR is very low-level, relatively flat, and is used to emit assembly code in AT&T syntax.

## Links

- [C23 standard (working draft)](https://open-std.org/JTC1/SC22/WG14/www/docs/n3220.pdf)
- [x86-64 instruction set](https://www.felixcloutier.com/x86/)
- [x86 assembly](https://en.wikibooks.org/wiki/X86_Assembly)
- [System V ABI](https://gitlab.com/x86-psABIs/x86-64-ABI)

## License

[MIT](LICENSE).
