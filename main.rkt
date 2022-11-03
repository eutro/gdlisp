#lang racket/base

(module macros racket/base
  (require (for-syntax
            "compiler.rkt"
            "parser.rkt"
            racket/base
            racket/port
            syntax/parse
            syntax/datum)
           (only-in
            "extra-symbols.rkt"
            begin-escape))

  (provide gd-modbeg
           gd-top-interaction)

  (begin-for-syntax
    (define-splicing-syntax-class gd-module-stmts
      #:attributes ([class-stmts 1]
                    [other-stmts 1])
      #:literals [require module begin-escape]
      (pattern {~seq
                {~or* {~and ({~or require module begin-escape} . _) top-form}
                      class-form}
                ...}
               #:attr [other-stmts 1] (syntax-e #'({~? top-form} ...))
               #:attr [class-stmts 1] (syntax-e #'({~? class-form} ...)))))

  (define-syntax (compile-to-source stx)
    (syntax-parse stx
      [(_ class:gd-top-class-stmts)
       (define str
         (call-with-output-string
          (λ (port)
            (display-code
             (emit-ir (datum class.class-ir))
             port))))
       (datum->syntax stx str)]))

  (define-syntax (gd-top-interaction stx)
    (syntax-parse stx
      #:literals [module require]
      [(_ . {~and ({~or* require module} . _) form}) #'form]
      [(_ . form)
       (syntax/loc stx
         (display (compile-to-source form)))]))

  (define-syntax (gd-modbeg stx)
    (syntax-parse stx
      [(_ mod:gd-module-stmts)
       (syntax/loc stx
         (#%module-begin
          mod.other-stmts ...
          (provide gdscript-source)
          (define gdscript-source
            (compile-to-source
             mod.class-stmts ...))
          (module+ main
            (display gdscript-source))))])))

(require "extra-symbols.rkt"
         "extra-macros.rkt"
         'macros)

(provide (rename-out
          [gd-modbeg #%module-begin]
          [gd-top-interaction #%top-interaction])
         (all-from-out
          "extra-symbols.rkt"
          "extra-macros.rkt")

         module
         #%require require
         only-in except-in prefix-in rename-in combine-in
         relative-in only-meta-in only-space-in for-syntax
         for-template for-label for-meta for-space submod
         quote

         define-syntax-rule ...

         #%app #%datum #%top)
