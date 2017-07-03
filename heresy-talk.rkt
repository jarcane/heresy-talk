#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require (only-in browser/external send-url))
(require "./heresy-language-generator.rkt")

;; Helper functions

(define (url address)
  (clickback (hyperlinkize (tt address))
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
 #:title "The guy wot is talking to you"
 (code
  (describe Me
    (name     "John Berry")
    (github   "http://github.com/jarcane")
    (employer "http://wolt.com"))))

(slide
 #:title "Content Warning: Stupid Ideas"
 (size-in-pixels (bitmap "warning.png"))
 (para "The following talk contains content that may be dangerous to your sanity,"
       "and at the very least contains many things that are neither practical"
       "or even particularly good ideas.")
 (para "Don't worry if you find anything confusing, in all likelihood, that"
       "merely means you are still sane."))

(slide
 #:title "What's a Lambda?"
 (size-in-pixels (bitmap "CoCo3system.jpg")))

(slide
 #:title "What's a Lambda"
 (bitmap "trs-80lisp.png")
 (size-in-pixels (url "http://mypage.iu.edu/~rdbeer/Software/BasicLisp/BasicLisp1.pdf")))

(slide
 #:title "Introducing Heresy"
 'next
 (item "Heresy is BASIC")
 'next
 (item "Heresy is a Lisp")
 'next
 (item "Heresy is functional")
 'next
 (item "Heresy is for learning")
 'next
 (item "Heresy is an experiment")
 'next
 (item "Heresy is for everyone"))

(slide
 #:title "Everything But kitchen-sink"
 (size-in-pixels
  (with-scale 0.75
    (para (foldr (λ (x y) (string-append (symbol->string x) ", " y))
               "☣"
               (the-heresy-language))))))

(slide
 #:title "For Loops"
 (size-in-pixels
  (code (def fn bottles (n)
          (for (x in (range n to 1 step -1))
            (? (format$ "#_ bottles of beer on the wall, #_ bottles of beer,"
                        x x))
            (? "Take one down pass it around,")
            (if (zero? (dec x)) then
              (? "No more bottles of beer on the wall.")
             else
              (do 
                (print & (dec x))
                (? " bottles of beer on the wall."))))))))

(slide
 #:title "Making For Functional with GOTO (kinda)"
 (size-in-pixels
  (code (define-syntax-rule (for-loop var lst x body ...)
          (let/ec break-k
            (syntax-parameterize 
                ((break (syntax-rules () 
                          [(_ ret) (break-k ret)]
                          [(_) (break-k)])))
              (let loop ((cry-v x)
                         (l lst))
                (syntax-parameterize
                    ([cry (make-rename-transformer #'cry-v)])
                  (cond [(null? l) cry-v]
                        [else (let ([var (car l)])
                                (loop
                                 (call/ec
                                  (lambda (k)
                                    (syntax-parameterize
                                        ([carry (make-rename-transformer #'k)])
                                      body ...)
                                    cry-v))
                                 (cdr l)))])))))))))

(slide
 #:title "It's My Party And I'll Cry If I Want To"
 (para "Question: How do we accumulate a value over a FOR loop, without mutable variables?")
 'next
 (para "Answer:" (tt "carry") "and" (tt "cry"))
 'next
 (size-in-pixels
  (code
   (def fn fact (n)
     (for (x in (range n to 1 step -1) with 1)
       (carry (* cry x))))))
 'next
 (size-in-pixels
  (code (def cards
          (for (suit in '(♠ ♣ ♥ ♦))
            (carry (append (for (x in (append (range 2 to 10) '(J Q K A)))
                             (carry (join `(,x ,suit) cry)))
                           cry)))))))

(slide
 #:title "Oh FOR God's Sake"
 (t "Cry can be anything you want, and so can a list, so:")
 'next
 (code (def fn :> (initial-value . fns)
         (for (f in fns with initial-value)
           (carry (f cry)))))
 'next
 (para #:align 'left
       (code
        > (:> 5
              inc
              (partial - 5)
              even?)))
 (para #:align 'left
       (code #f)))

(slide
 #:title "We can rebuild him"
 'next
 (para #:align 'left
       "First we need a little helper for currying:"
       (size-in-pixels
        (code
         (def macro f> (f args ...)
           (fn (x)
               (f x args ...))))))
 'next
 (para #:align 'left
       "Now one more little macro:"
       (size-in-pixels
        (code
         (def macro -> (iv (f args ...) ...)
           (:> iv
               (f> f args ...)
               ...)))))
 'next
 (para #:align 'left
       "Et voila!")
 (para #:align 'left
       (size-in-pixels
        (code
         > (-> '(1 2 3 4)
               (left 2)
               (append '(a b))))))
 (para #:align 'left
       (size-in-pixels
        (code '(1 2 a b)))))

(slide
 #:title "Better, Stronger, Faster"
 (para "Of course, those are just helpful shortcuts. We can call the helpers directly too:")
 'next
 (para #:align 'left
       (code
        > (:> '(1 2 3 4)
              (l> map (fn (x) (* x x)))
              (f> left 2)
              (l> append '(a b))
              (f> append '(a b)))))
 (para #:align 'left
       (code '(a b 1 4 a b))))

(slide
 #:title "useing you're pipe's good"
 (bitmap "C53qLh2XEAE-Jh3.jpg"))

(slide
 #:title "Bored CS Kid 101"
 (size-in-pixels
  (code  
   (def fn thing (lst)
     (fn args*
         (let ([alst lst])
           (select
            ((null? args*) alst)
            ((symbol? (head args*)) (tail (assoc (head args*) alst)))
            ((list? (head args*)) 
             (let recur ([al alst]
                         [pat (head args*)]
                         [c 1])
               (select
                ((null? pat) (thing al))
                ((eq? (head pat) '*) (recur alst (tail pat) (+ 1 c)))
                (else (recur (subst (head (index c al))
                                    (join (head pat) Null)
                                    al)
                        (tail pat)
                        (+ 1 c))))))
            (else (error "Thing expected a symbol or a pattern")))))))))

(slide
 #:title "It's a ... thing."
 (bitmap "thing-013.jpg"))

(slide
 #:title "From Beneath You, It Devours"
 (para "Things are immutable objects with pattern-matching syntax for self-copying")
 (size-in-pixels
  (code
   > (describe Cthulhu
       (name     "Great Lord Cthulhu")
       (size     "gigantic")
       (location "R'Lyeh")
       (status   "dreaming")
       (eat      (fn (victim)
                     (print (format$ "#_ devours #_" name victim)))))
   > (def The-Sleeper-Awakened (Cthulhu `(* * * "awake")))
   > (The-Sleeper-Awakened 'status)
   "awake")))

(slide
 #:title "Operator, Operator"
 (para #:align 'left
       (size-in-pixels
        (code
         (describe State))))
 'next
 (para #:align 'left
       (size-in-pixels
        (code
         (def macroset :=
           [(:= (name ...) var value)
            (fn (s) (thing extends s
                           (var (let ([name (s (quote name))] ...)
                                  value))))]
           [(:= var value)
            (fn (s) (thing extends s (var value)))]))))
 'next
 (para #:align 'left
       (size-in-pixels
        (code
         (def macroset :_ 
           [(:_ (name ...) f args ...)
            (fn (s)
                (let ([name (s (quote name))] ...)
                  (f args ...)
                  s))]
           [(:_ f args ...)
            (fn (s)
                (f args ...)
                s)]))))
 'next
 (para #:align 'left
       (size-in-pixels
        (code
         (def macro return (name)
           (fn (s) (s (quote name))))))))

(slide
 #:title "And one more thing ..."
 (para #:align 'left
       (size-in-pixels
        (code
         (def fn do> fns
           (apply :> (join State fns)))))))

(slide
 #:title "By your powers combined!"
 (para #:align 'left
       (code
         (do>
           (:= x 5)
           (:_ (x) print (format$ "Value was #_" x))
           (:= (x) x (+ x 5))
           (:_ (x) print (format$ "But now it's #_" x))
           (:= x "Behold, the state monad ... -ish.")
           (return x))))
 'next
 (para #:align 'left "Output:")
 (para #:align 'left (tt "Value was 5"))
 (para #:align 'left (tt "Now it's 10"))
 (para #:align 'left (code "Behold, the state monad ... -ish.")))

(slide
 #:title "But wait, there's more!"
 'next
 (item #:align 'left "The Y combinator")
 (para #:align 'left
       (size-in-pixels
        (code
         (def Y
           (fn (b)
               ((fn (f) (b (fn (x) ((f f) x))))
                (fn (f) (b (fn (x) ((f f) x))))))))))
 'next
 (item #:align 'left "The Y* combinator")
 (para #:align 'left
       (size-in-pixels
        (code
         (def Y*
           (fn (b)
               ((fn (f) (b (fn args (apply (f f) args))))
                (fn (f) (b (fn args (apply (f f) args)))))))))))

(slide
 #:title "We can't stop here, this is bat country"
 'next
 (item #:align 'left "Named anonymous functions")
 (para #:align 'left
          (size-in-pixels
           (code
            > (map (fnlet fib (n)
                          (select
                           ((zero? n) 0)
                           ((one? n) 1)
                           (else (+ (fib (- n 2))
                                    (fib (- n 1))))))
                   (range 0 to 10))
            '(0 1 1 2 3 5 8 13 21 34 55))))
 'next
 (item #:align 'left "Infix math DSL based on Clojure's maya.clj")
 (para #:align 'left
          (size-in-pixels
           (code
            (def fn quadratic (a b c)
              (m let d = 4 * a * c
                 let D = b * b - d ~> sqrt
                 let t = 2 * a let -b = (- b)
                 let x1 = -b + D / t
                 let x2 = -b - D / t in
                 (list x1 x2)))))))

(slide
 #:title "That's all folks!"
 (bitmap "notstupid.jpeg")
 (t "Programming should be fun.")
 (t "Try things. Crazy things.")
 (t "We learn by experimenting."))