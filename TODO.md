# Technical Debt

Stuff that needs to be done, but isn't urgent.

## `cyan_compiler`

- [ ] Refactor/rewrite lowering passes.
  - Current implementation is way too dumb. Play around with visitor pattern or arenas.
  - Right now we have error structs for each pass. We need to centralize error management, add proper error/diagnostic reporting (`cyan_reporting` was created for exactly this purpose).
- [ ] Refactor and add more end-to-end tests. Probably use `insta` for this.
