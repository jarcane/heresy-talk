#lang racket

(require heresy)
(provide the-heresy-language)

(define-values (foo bar) (module->exports 'heresy))

(define l1 (->> foo
                (head)
                (tail)
                (heads)))

(define l2 (->> bar
                (head)
                (tail)
                (heads)))

;(define l3 (->> bar
;                (tail)
;                (head)
;                (tail)
;                (heads)))

(define (the-heresy-language)
  (->> (append l1 l2)
       (sort (fn (x y) (string-ci<? (symbol->string x)
                                    (symbol->string y))))))