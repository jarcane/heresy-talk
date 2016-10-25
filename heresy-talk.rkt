#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require (only-in browser/external send-url))

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
            (twitter  "http://twitter.com/J_Arcane")
            (employer "http://metosin.fi"))))

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
 (item "Heresy is BASIC")
 (item "Heresy is a Lisp")
 (item "Heresy is functional")
 (item "Heresy is for learning")
 (item "Heresy is an experiment")
 (item "Heresy is for everyone"))

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
 (para "Answer:" (tt "carry") "and" (tt "cry"))
 (size-in-pixels
  (code (def cards
          (for (suit in '(♠ ♣ ♥ ♦))
            (carry (append (for (x in (append (range 2 to 10) '(J Q K A)))
                             (carry (join `(,x ,suit) cry)))
                           cry)))))))

(slide
 #:title "Oh FOR God's Sake"
 (t "Cry can be anything you want, and so can a list, so:")
 (code (def fn :> (initial-value . fns)
         (for (f in fns with initial-value)
           (carry (f cry)))))
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
 (para #:align 'left
       "First we need a little helper for currying:"
       (code
        (def macro f> (f args ...)
          (fn (x)
              (f x args ...)))))
 (para #:align 'left
       "Now one more little macro:"
       (code
        (def macro -> (iv (f args ...) ...)
          (:> iv
              (f> f args ...)
              ...))))
 (para #:align 'left
       "Et voila!")
 (para #:align 'left
       (code
        > (-> '(1 2 3 4)
              (left 2)
              (append '(a b)))))
 (para #:align 'left
       (code '(1 2 a b))))

(slide
 #:title "Better, Stronger, Faster"
 (para "Of course, those are just helpful shortcuts. We can call the helpers directly too:")
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
 #:title "It's a ... thing."
 (bitmap "thing-013.jpg"))

(slide
 #:title "It's alive!"
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
 #:title "From Beneath You, It Devours"
 (para "Things are immutable objects with pattern-matching syntax for self-copying")
 (code
  > (describe Santa
              (size 'fat)
              (sleigh 'ready)
              (sack 'full))
  > (def Santa-after-Christmas (Santa `(* * empty)))
  > (Santa-after-Christmas)
  '((size fat) (sleigh ready) (sack empty))))

(slide
 #:title "We have to go deeper"
 (para #:align 'left
       (code
         (do>
           (:= x 5)
           (:_ (x) print (format$ "Value was #_" x))
           (:= (x) x (+ x 5))
           (:_ (x) print (format$ "But now it's #_" x))
           (:= x "Behold, monadish state")
           (return x)))))