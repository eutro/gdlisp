#lang racket/base

(module macros racket/base
  (require (for-syntax
            "compiler.rkt"
            "parser.rkt"
            racket/base
            racket/port
            syntax/parse
            syntax/datum))

  (provide gd-modbeg
           gd-top-interaction)

  (begin-for-syntax
    (define-splicing-syntax-class gd-module-stmts
      #:attributes ([class-stmts 1]
                    [other-stmts 1])
      #:literals [require module]
      (pattern {~seq
                {~or* {~and ({~or require module} . _) top-form}
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
         (except-out
          (all-from-out racket/base)
          #%module-begin
          #%top-interaction))
