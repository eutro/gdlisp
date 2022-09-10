#lang info
(define collection "gdlisp")
(define deps '("base" "syntax-classes-lib" "file-watchers" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/gdlisp.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.1")
(define pkg-authors '(eutro))
(define license '(Apache-2.0 OR MIT))
(define raco-commands
  '(("gdlisp" gdlisp/cli "Compile GDLisp files" #f)))

