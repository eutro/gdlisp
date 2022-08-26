#lang typed/racket/base

(provide intersperse)

(: intersperse (All (A B) (-> (Listof A) B (Listof (U A B)))))
(define (intersperse lst v)
  (cond
    [(null? lst) null]
    [else
     (let loop ([lst lst])
       (if (null? (cdr lst))
           lst
           (list* (car lst) v (loop (cdr lst)))))]))
