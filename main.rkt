#lang racket/base

(module macros racket/base
  (require (for-syntax
            "compiler.rkt"
            "parser.rkt"
            racket/base
            racket/port
            syntax/parse
            syntax/datum))

  (provide gd-modbeg)

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
          [gd-modbeg #%module-begin])
         (all-from-out
          "extra-symbols.rkt"
          "extra-macros.rkt")
         (except-out
          (all-from-out racket/base)
          #%module-begin))

(module main racket/base
  (require file-watchers
           racket/cmdline
           racket/match)

  (define (make-gdlisp-namespace)
    (define cns (current-namespace))
    (define ns (make-base-namespace))
    (for ([mod (in-list (unbox to-attach))])
      (namespace-attach-module cns mod ns))
    ns)

  (define to-attach (box '(gdlisp racket)))
  (define watch? (box #f))
  (command-line
   #:multi
   [("--attach" "-a")
    module-name
    "preload a module and attach it before reloading a file, saving time if it is (require ...)d"
    (set-box! to-attach (cons (string->symbol module-name) (unbox to-attach)))]
   #:once-each
   [("--watch" "-w")
    "compile once, then watch files for changes"
    (set-box! watch? #t)]
   #:args (path . more-paths)
   (let ()
     (for ([mod (in-list (unbox to-attach))])
       (dynamic-require mod #f))

     (define (update-path path)
       (with-handlers ([exn:fail?
                        (λ (e)
                          (fprintf (current-error-port)
                                   "An error occurred updating ~s: ~a\n"
                                   (path->string path)
                                   (exn-message e))
                          (void))])
         (let/cc break
           (when (and (regexp-match #px"(.*)\\.rkt$" (path->string path))
                      (file-exists? path))
             (printf "Checking ~s...\n" (path->string path))
             (define code
               (parameterize ([current-namespace (make-gdlisp-namespace)])
                 (dynamic-require
                  path 'gdscript-source
                  (λ ()
                    (displayln "Not a gdlisp file, nothing written.")
                    (break (void))))))
             (define out-path (path-replace-extension path ".gd"))
             (call-with-output-file* out-path
               #:mode 'text
               #:exists 'replace
               (λ (port) (write-string code port)))
             (printf "Wrote ~s.\n" (path->string out-path))))))

     (define paths (map string->path (cons path more-paths)))
     (for ([path (in-list paths)])
       (unless (path-on-disk? path)
         (raise-user-error
          (format "path ~s does not exist" (path->string path)))))

     (displayln "Compiling paths.")
     (for* ([root-path (in-list paths)]
            [path (in-directory root-path)])
       (update-path path))
     (when (unbox watch?)
       (define (on-change msg)
         (match msg
           [(list _ (or 'change 'add) path) (update-path path)]
           [_ (void)]))

       (define watcher
         (watch
          paths
          on-change
          void))
       (displayln "Watching paths.")
       (thread-wait watcher)))))
