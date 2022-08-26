#lang racket/base

(require (for-syntax
          racket/base
          syntax/parse))

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

(define-disallows
  [class-name "at the top level"]
  [extends "in class"]
  [match "as an expression"]
  [var "as a statement"]
  [signal "as a statement"]
  [func "as a statement"]
  [static "as part of func"]
  [class "as a statement"]
  [.. "in a match pattern"]
  [: "in a binding"]
  [:= "in a binding"]
  [#%gdscript "as an expression"])
