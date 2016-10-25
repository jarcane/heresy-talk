#lang slideshow

(require slideshow/text)
(require slideshow/code)
(require (only-in browser/external send-url))

(slide
 (big (t "The Heresy Programming Language"))
 (bitmap "yellowsignicon.jpeg")
 (it "Or, Learning Through Madness")
 (t "")
 (para (t "Language: ")
       (clickback (tt "http://github.com/jarcane/heresy")
                  (λ () (send-url "http://github.com/jarcane/heresy"))))
 (para (t "Slide Source: ")
       (clickback (tt "http://github.com/jarcane/heresy-talk")
                  (λ () (send-url "http://github.com/jarcane/heresy-talk")))))