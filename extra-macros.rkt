#lang racket/base

(require (prefix-in $ "extra-symbols.rkt")
         (for-syntax
          racket/base
          racket/format
          syntax/parse
          syntax/datum))

(begin-for-syntax
  (define-syntax-class infix
    #:attributes (name str)
    (pattern [id:id tok:str]
             #:attr name #'id
             #:attr str #'tok
             #:attr left? #t
             #:attr right? #t)

    (pattern id:id
             #:attr name #'id
             #:attr str (datum->syntax this-syntax (~a (syntax-e #'name)))
             #:attr left? #t
             #:attr right? #t)))

(define-syntax (define-infixes stx)
  (syntax-parse stx
    [(_ clause:infix . tail)
     (quasisyntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             [(_ lhs rhs)
              (syntax/loc stx
                ($#%gdscript "(" (!expr lhs) " " clause.str " " (!expr rhs) ")"))]))
         (provide clause.name)
         #,(quasisyntax/loc stx
             (define-infixes . tail))))]
    [(_) #'(begin)]))

(define-syntax (define-assignments stx)
  (syntax-parse stx
    [(_ clause:infix ...)
     (syntax/loc stx
       (begin
         (define-syntax (clause.name stx)
           (syntax-parse stx
             [(_ lhs rhs)
              (syntax/loc stx
                ($begin
                  ($#%gdscript (!expr lhs) " " clause.str " " (!expr rhs))
                  null))]))
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
                {~? ($#%gdscript "(" pre ... (!expr x) ")")
                    x})]
             [(_ lhs rhs)
              (syntax/loc stx
                ($#%gdscript "(" (!expr lhs) " " clause.str " " (!expr rhs) ")"))]
             [(_ lhs rhs rhses (... ...))
              (syntax/loc stx
                (clause.name (clause.name lhs rhs) rhses (... ...)))]))
         ...
         (provide clause.name ...)))]))

(define-infixes
  ** ~ % << >>
  < > == != >= <=
  in is as)

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
  [*set! "*="]
  [/set! "/="]
  [%set! "%="]
  [**set! "**="]
  [bit-and-set! "&="]
  [bit-or-set! "|="]
  [<<set! "<<="]
  [>>set! ">>="])

;; General lisp macros...

(provide if when ref)

(define-syntax (define-good-syntax-parse-rule stx)
  (syntax-parse stx
    [(_ (macro-id . pattern) pattern-directive ... template)
     (syntax/loc stx
       (define-syntax (macro-id stx)
         (syntax-parse stx
           #:track-literals
           [({~var macro-id id} . pattern)
            pattern-directive ...
            (syntax/loc stx
              template)])))]))

(define-good-syntax-parse-rule (if pred-expr:expr
                                   then-expr:expr
                                   else-expr:expr)
  ($cond
    [pred-expr then-expr]
    [$else else-expr]))

(define-good-syntax-parse-rule (when pred-expr:expr
                                 body-exprs:expr ...)
  ($cond
    [pred-expr
     body-exprs ...]))

(define-good-syntax-parse-rule (ref target:expr key:expr)
  ($#%gdscript (!expr target) "[" (!expr key) "]"))

;; More specific macros...

(provide fset! yield-for some~> await)

(define-good-syntax-parse-rule (fset! x f y ...)
  (set! x (f x y ...)))

(define-good-syntax-parse-rule (yield-for secs:expr)
  (yield (.create-timer (get_tree) secs) "timeout"))

(define-syntax (some~> stx)
  (syntax-parse stx
    [(_ expr f:id)
     (syntax/loc stx
       (some~> expr (f)))]
    [(_ expr (f args ...))
     (with-syntax ([tmp (gensym "tmp")])
       (syntax/loc stx
         ($let ([tmp expr])
           (when (!= tmp null)
             (f tmp args ...)))))]))

(define-syntax (await stx)
  (syntax-parse stx
    [(_ maybe-coro:expr)
     (with-syntax ([tmp (gensym "tmp")])
       (syntax/loc stx
         ($let ([tmp maybe-coro])
           (if (is tmp GDScriptFunctionState)
             (yield tmp "completed")
             tmp))))]))
