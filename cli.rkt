#lang racket/base

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
   #:program "gdlisp"
   #:multi
   [("--attach" "-a")
    module-name
    "Preload a module and attach it before reloading a file, saving time if it is (require ...)d"
    (set-box! to-attach (cons (string->symbol module-name) (unbox to-attach)))]
   #:once-each
   [("--watch" "-w")
    "Compile once, then watch files for changes"
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
