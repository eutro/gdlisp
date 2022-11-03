#lang racket/base

(require (for-syntax
          racket/base
          syntax/parse)
         (prefix-in % racket/base))

(define-syntax (define-disallows stx)
  (syntax-parse stx
    [(_ [name:id only-in:str] ...)
     (syntax/loc stx
       (begin
         (define-syntax (name stx)
           (raise-syntax-error
            'name
            (format "~a is only allowed ~a"
                    'name only-in)
            stx))
         ...
         (provide name ...)))]))

(define-syntax-rule (begin-escape body ...)
  (%begin body ...))

(provide begin-escape)

(define-disallows
  [class-name "at the top level"]
  [export "in a definition"]
  [onready "in a definition"]
  [extends "in class"]
  [match "as an expression"]
  [cond "as an expression"]
  [else "as a cond branch"]
  [let "as an expression"]
  [begin "as an expression or at the top level"]
  [for "as an expression"]
  [define "as a statement"]
  [recur "as an expression"]
  [var "as a statement"]
  [const "in a definition"]
  [signal "as a statement"]
  [func "as a statement"]
  [static "as part of func"]
  [class "as a statement"]
  [.. "in a match pattern"]
  [: "in a binding"]
  [:= "in a binding"]
  [#%gdscript "as an expression"])

(provide _) ;; these are actually important for macros...
