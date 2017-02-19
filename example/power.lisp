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
