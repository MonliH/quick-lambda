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
λ> λf.((λx.f (x x))(λx.f (x x))) λa.a λa.a λa.a 1
1 (int)

// OR

λ> \f.((\x.f (x x))(\x.f (x x))) \a.a \a.a \a.a 1
1 (int)
```

As seen above, the interpreter is lazy (using call-by-need)!

If you try the same in Javascript, it'll just stack overflow:

```js
const y = (f) => ((x) => f(x)(x))((x) => f(x)(x));
y((a) => a)(1);
// Gives
// Uncaught RangeError: Maximum call stack size exceeded
```

There's also basic arithmetic built-in so you don't have to use the Church encodings:

```
λ> 1+1
2 (int)

λ> (λa.a+1) ((λa.a+1) 2)
3 (int)

λ> ()
```

Notice that function application is left-associtive so we need to add parentheses around `(λa.a+1) 2`.
