
# Pure Lisp Interpreter by Haskell
素朴な再帰を使った実装

# Usage

```
> stack build
> stack exec lisp example/fib.lisp
```

# Syntax
- `(do exprs)`
- `(def name body)`
- `(lam (args) body)`
- `(if bool then else)`
- `(eq a b) => #t or #f`
- `(atom a) => #t or #f`
- `(cons a b)`
- `(car cons)`
- `(cdr cons)`
- `(+ a b)`
- `(- a b)`
- `(puts a)`

トップレベルは大抵doで始まります
それはdefがdoの中で無いと意味が無いからです
よってdefを使う場合はこのように書きます

```lisp:power.lisp
(do
  (def *
    (lam (a b)
      (if (eq b 1)
        a
        (+ a (* a (- b 1))))))

  (def **
    (lam (a b)
      (if (eq b 1)
        a
        (* a (** a (- b 1))))))

  (** 2 10))
```

doの最後の式が返り値となります
