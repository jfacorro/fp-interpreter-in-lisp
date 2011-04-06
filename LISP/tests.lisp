(test (fp-id) abc)

(test (fp-selector 2) (a b c))

(test (fp-selector-right 3) (a b c))

(test (fp-tl) (a b c))

(test (fp-tlr) (a b c))

(test (fp-atom) 0)
(test (fp-atom) (1 2 3))

(test (fp-eq) (b a))
(test (fp-eq) (a a))
(test (fp-eq) (2 1))
(test (fp-eq) (1 1))

(test (fp-null) ())
(test (fp-null) (1 3))
(test (fp-null) 1)

(test (fp-reverse) (a b c))

(test (fp-iota) 3)
(test (fp-iota) 5)

(test (fp-distl) (a (b c d)))
(test (fp-distl) (a ()))

(test (fp-distr) ((b c d) a))
(test (fp-distr) (() a ))

(test (fp-length) (1 2 3 a b c))
(test (fp-length) ())

(test (fp--) (4 2))
(test (fp-+) (4 2))
(test (fp-*) (4 2))
(test (fp-%) (4 2))
(test (fp-<) (4 2))
(test (fp->) (4 2))

(test (fp-trans) ((1 2) (3 4) (5 6)))

(test (fp-and) (t nil))

(test (fp-or) (t nil))

(test (fp-not) t)
(test (fp-not) nil)

(test (fp-appendl) (a (b c)))

(test (fp-appendr) ((b c) (a)))
(test (fp-appendr) ((b c) a))

(test (fp-rot) (1 2 3 4))

(test (fp-rotr) (1 2 3 4))

(test (fp-compose (fp--) (fp-rot)) (2 1))

(test (fp-construct (fp-selector 3) (fp-selector 2)) (1 2 3))

(test (fp-const 'a) ())

(test (fp-cond (fp-compose (fp-not) (fp-null))
               (fp-const 1)
               (fp-const '())) ())
(test (fp-cond (fp-null) (fp-null) (fp-null)) (1 2))

(test (fp-insert (fp-appendl)) (1 2 3 ()))

(test (fp-alpha (fp-null)) (() 1 2))
(test (fp-compose (fp-null) (fp-alpha (fp-null))) (() 1 2))
(test (fp-compose (fp-insert (fp-+)) (fp-iota)) 3)
