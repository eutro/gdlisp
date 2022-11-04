#lang info
(define collection "gdlisp")
(define deps '("base" "syntax-classes-lib" "file-watchers" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/gdlisp.scrbl" ())))
(define pkg-desc "A lisp dialect that compiles to GDScript.")
(define version "0.3")
(define pkg-authors '(eutro))
(define license '(Apache-2.0 OR MIT))
(define raco-commands
  '(("gdlisp" (submod gdlisp/cli main) "Compile GDLisp files" #f)))

