#lang racket

(require heresy)

(define-values (foo bar) (module->exports 'heresy))

(define l1 (->> foo
                (head)
                (tail)
                (heads)))

(define l2 (->> bar
                (head)
                (tail)
                (heads)))

(define l3 (->> bar
                (tail)
                (head)
                (tail)
                (heads)))

(->> (append l1 l2 l3)
     (sort (fn (x y) (string-ci<? (symbol->string x)
                                  (symbol->string y)))))