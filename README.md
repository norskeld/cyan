# cyan

[![Checks](https://img.shields.io/github/actions/workflow/status/norskeld/cyan/checks.yml?style=flat-square&colorA=22272d&colorB=22272d&label=checks)](https://github.com/norskeld/cyan/actions/workflows/checks.yml)

Compiler for a subset of C.

## Trees

AST, TAC and Assembly AST (AAST) are described using [Zephyr ASDL][zephyr].

### AST

Grammar:

```ebnf
<program>    ::= <function>
<function>   ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
<statement>  ::= "return" <expression> ";"
<expression> ::= <int> | <unary-op> <expression> | "(" <expression> ")"
<unary-op>   ::= "-" | "~"

<identifier> ::= ? An identifier token ?
<int>        ::= ? A constant token ?
```

AST definition:

```zephyr
program    = Program(function)
function   = Function(identifier name, statement body)
statement  = Return(expression)
expression = Constant(int) | Unary(unary_op, expression)
unary_op   = BitwiseNot | Negate
```

### TAC

Three Adress Code (TAC) is a simple IR

```zephyr
program     = Program(function)
function    = Function(identifier, instruction* body)
instruction = Return(value) | Unary(unary_op, value src, value dst)
value       = Constant(int) | Var(identifier)
unary_op    = BitwiseNot | Negate
```

### Assembly AST (AAST)

```zephyr
program     = Program(function)
function    = Function(identifier name, instruction* instructions)
instruction = Mov(operand src, operand dst) | Ret
operand     = Imm(int) | Register
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
