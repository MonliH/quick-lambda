# Quick Lambda

A small lambda calculus implementation in haskell.

## Running
1. Install [`stack`, a haskell tool stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repo, then:
   ```bash
   cd quick-lambda
   stack build --exec quick-lambda-exe
   ```
3. `:h` for help in the repl, `:q` to quit the repl

## Syntax
This is just standard lambda calculus, for example the Y-Combinator:
```
λf.(λx.f (x x))(λx.f (x x))
```
or:
```
\f.(\x.f (x x))(\x.f (x x))
```

Take your pick.
