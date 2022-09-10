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

(: mangle (-> String String))
(define (mangle name)
  (with-output-to-string
    (λ ()
      (define esc-slashes #t)
      (define i
        (cond
          [(zero? (string-length name))
           (display "_BLANK_")
           0]
          [(char<=? #\0 (string-ref name 0) #\9)
           (display "N")
           0]
          [(char=? (string-ref name 0) #\$)
           (display "$")
           (set! esc-slashes #f)
           1]
          [else 0]))
      (for ([c (in-string name i)])
        (display
         (match c
           [(? (λ (c)
                 (or (char<=? #\a c #\z)
                     (char<=? #\A c #\Z)
                     (char<=? #\0 c #\9))))
            c]
           [(or #\_ #\-) "_"]
           ;; copied shamelessly from Clojure...
           [#\: "_COLON_"]
           [#\+ "_PLUS_"]
           [#\> "_GT_"]
           [#\< "_LT_"]
           [#\= "_EQ_"]
           [#\~ "_TILDE_"]
           [#\! "_BANG_"]
           [#\@ "_CIRCA_"]
           [#\# "_SHARP_"]
           [#\' "_SINGLEQUOTE_"]
           [#\" "_DOUBLEQUOTE_"]
           [#\% "_PERCENT_"]
           [#\^ "_CARET_"]
           [#\& "_AMPERSAND_"]
           [#\* "_STAR_"]
           [#\| "_BAR_"]
           [#\{ "_LBRACE_"]
           [#\} "_RBRACE_"]
           [#\[ "_LBRACK_"]
           [#\] "_RBRACK_"]
           [#\/ (if esc-slashes "_SLASH_" "/")]
           [#\\ "_BSLASH_"]
           [#\? "_QMARK_"]
           [#\space "_SPACE_"]
           [_
            (format "_U~a_"
                    (~a #:pad-string "0"
                        #:min-width 4
                        #:align 'right
                        (number->string
                         (char->integer c)
                         16)))]))))))
