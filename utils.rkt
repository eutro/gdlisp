#lang typed/racket/base

(require racket/port
         racket/match
         racket/format)

(provide intersperse
         mangle)

(: intersperse (All (A B) (-> (Listof A) B (Listof (U A B)))))
(define (intersperse lst v)
  (cond
    [(null? lst) null]
    [else
     (let loop ([lst lst])
       (if (null? (cdr lst))
           lst
           (list* (car lst) v (loop (cdr lst)))))]))

(define (mangle [name : String]) : String
  (with-output-to-string
    (λ ()
      (cond
        [(zero? (string-length name))
         (display "_BLANK_")]
        [(char<=? #\0 (string-ref name 0) #\9)
         (display "N")])
      (for ([c (in-string name)])
        (display
         (match c
           [(? (λ (c)
                 (or (char<=? #\a c #\z)
                     (char<=? #\A c #\Z)
                     (char<=? #\0 c #\9))))
            c]
           [(or #\_ #\-) "_"]
           [#\? "_QMARK_"]
           [#\! "_EMARK_"]
           [#\space "_SPACE_"]
           [_
            (format "_U~a_"
                    (~a #:pad-string "0"
                        #:min-width 4
                        #:align 'right
                        (number->string
                         (char->integer c)
                         16)))]))))))
