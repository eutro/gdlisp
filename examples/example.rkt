#lang gdlisp

(module macro racket
  (require gdlisp
           (for-syntax syntax/parse))
  (provide swap!)
  (define-syntax (swap! stx)
    (syntax-parse stx
      [(_ a b)
       (with-syntax ([tmp (gensym "tmp")])
         (syntax/loc stx
           (let ([tmp a])
             (set! a b)
             (set! b tmp))))])))

(require 'macro)

(define (foo a b c)
  (swap! a b)
  (match a
    [1 10]
    [2 20]
    [3 30]
    [_
     (let ([acc 0])
       (for ([n a])
         (-set! acc 10))
       acc)]))
