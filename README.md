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

This is just standard lambda calculus, for example the Y-Combinator (and factorial program):

```js
λ> λf.(λx.(f (x x)) λx.(f (x x))) λf x.(x == 1 1 (x * (f x - 1))) 5
120 (int)

// OR

λ> \f.(\x.(f (x x)) \x.(f (x x))) \f x.(x == 1 1 (x * (f x - 1))) 5
120 (int)
```

As seen above, the interpreter is lazy (using call-by-need)!

If you try the same in Javascript, it'll just stack overflow:

```js
const Y = (g) => ((x) => g(x(x)))((x) => g(x(x)));
Y((f) => (x) => (x === 0 ? 1 : x * f(x - 1)))(5);
// Results in
// Uncaught RangeError: Maximum call stack size exceeded
```

There's also basic arithmetic built-in so you don't have to use the Church encodings:

```js
λ> 1+1*3-1
3 (int)

λ> (λa.a+1) ((λa.a+1) 2)
3 (int)

λ> true 1 0  // True acts as a function (as λa b.a)
1 (int)

λ> false 1 0  // False does too (as λa b.b)
0 (int)

λ> 2 == 2  // Equality
true (bool)

λ> (2 == 2) 0 1  // Remember, true and false act as functions!
0 (bool)
```

Notice that function application is left-associtive so we need to add parentheses around `(λa.a+1) 2`.
