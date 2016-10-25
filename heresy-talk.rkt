#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require (only-in browser/external send-url))

;; Helper functions

(define (url address)
  (clickback (tt address)
             (λ () (send-url address))))

;; Begin slideshow

(slide
 (big (t "The Heresy Programming Language"))
 (bitmap "yellowsignicon.jpeg")
 (it "Or, Learning Through Madness")
 (t "")
 (para (t "Language: ")
       (url "http://github.com/jarcane/heresy"))
 (para (t "Docs:")
       (url "https://docs.racket-lang.org/heresy"))
 (para (t "Slide Source: ")
       (url "http://github.com/jarcane/heresy-talk")))

(slide
 #:title "What's a Lambda?"
 (size-in-pixels (bitmap "CoCo3system.jpg")))

(slide
 #:title "What's a Lambda"
 (bitmap "trs-80lisp.png")
 (size-in-pixels (url "http://mypage.iu.edu/~rdbeer/Software/BasicLisp/BasicLisp1.pdf")))

(slide
 #:title "Introducing Heresy"
 (item "Heresy is BASIC")
 (item "Heresy is a Lisp")
 (item "Heresy is functional")
 (item "Heresy is for learning")
 (item "Heresy is an experiment")
 (item "Heresy is for everyone"))

(slide
 #:title "For Loops"
 (code (def fn fizzbuzz (n)
         (for (x in (range 1 to n))
           (select
            ((zero? x) x)
            ((zero? (+ (mod x 5)
                       (mod x 3))) (print "FizzBuzz"))
            ((zero? (mod x 5)) (print "Buzz"))
            ((zero? (mod x 3)) (print "Fizz"))
            (else (print x)))))))