#lang racket/base

(require "extra-symbols.rkt"
         (for-syntax
          racket/base
          racket/format
          syntax/parse))

(begin-for-syntax
  (define-syntax-class infix
    #:attributes (name str)
    (pattern [id:id tok:str]
             #:attr name #'id
             #:attr str #'tok)
    (pattern id:id
             #:attr name #'id
             #:attr str (datum->syntax this-syntax (~a (syntax-e #'name))))))

(define-syntax (define-infixes stx)
  (syntax-parse stx
    [(_ clause:infix ...)
     (syntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             [(_ lhs rhs)
              (syntax/loc stx
                (#%gdscript "(" (!expr lhs) ") " clause.str " (" (!expr rhs) ")"))]))
         ...
         (provide clause.name ...)))]))

(define-syntax (define-assignments stx)
  (syntax-parse stx
    [(_ clause:infix ...)
     (syntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             [(_ lhs rhs)
              (syntax/loc stx
                (#%gdscript (!expr lhs) " " clause.str " (" (!expr rhs) ")"))]))
         ...
         (provide clause.name ...)))]))

(define-syntax (define-castlike stx)
  (syntax-parse stx
    [(_ clause:infix ...)
     (syntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             [(_ lhs rhs)
              (syntax/loc stx
                (#%gdscript "(" (!expr lhs) ") " clause.str " " (!expr rhs)))]))
         ...
         (provide clause.name ...)))]))

(define-syntax (define-leftassoc stx)
  (syntax-parse stx
    [(_ [clause:infix
         {~optional {~seq #:prefix {~seq pre ...}}}
         {~optional {~seq #:identity identity}}]
        ...)
     (syntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             {~? [(_) #'identity]}
             [(_ x)
              (syntax/loc stx
                {~? (#%gdscript "(" (!expr x) ")")
                    x})]
             [(_ lhs rhs)
              (syntax/loc stx
                (#%gdscript
                 "(" (!expr lhs) ") "
                 clause.str
                 " (" (!expr rhs) ") "))]
             [(_ lhs rhses (... ...))
              (syntax/loc stx
                (clause.name lhs (clause.name rhses (... ...))))]))
         ...
         (provide clause.name ...)))]))

(define-infixes
  ** ~ % << >>
  < > == != >= <=
  in)

(define-leftassoc
  [+ #:identity 0]
  [- #:prefix "-"]
  [* #:identity 1]
  [/ #:prefix "1/"]
  [and #:identity #true]
  [or #:identity #false]
  [[bit-xor "^"]]
  [[bit-and "&"] #:identity -1]
  [[bit-or "|"] #:identity 0])

(define-assignments
  [set! "="]
  [+set! "+="]
  [-set! "-="]
  [*set! "-="]
  [/set! "-="]
  [%set! "-="]
  [**set! "-="]
  [bit-and-set! "&="]
  [<<set! "<<="]
  [>>set! ">>="])

(define-castlike
  is as)
