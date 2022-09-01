#lang scribble/manual

@(require (for-label gdlisp))

@title{GDLisp}

@defmodule[gdlisp]

GDLisp is a lisp dialect that compiles to
@(hyperlink
  "https://docs.godotengine.org/en/latest/tutorials/scripting/gdscript/gdscript_basics.html#doc-gdscript"
  @list{GDScript}).

@codeblock|{
  #lang gdlisp
  (print "Hello, world!")
}|

Produces (when run):

@codeblock|{
  print("Hello, world!")
}|

@section{Overview}

GDLisp is a way to write script files for the
@(hyperlink
  "https://godotengine.org/"
  @list{Godot engine})
without actually writing (directly) in the scripting language it
provides.

The main benefits this provides is that you can write macros and
(hopefully) enter into Lisp game jams. Also you don't have to deal
with semantic whitespace.

@subsection{The GDLisp script}

A GDLisp script has the following grammar:

@racketgrammar*[#:literals
                [require module class-name extends]

                [program
                 {code:line top-level-stmt ...}]
                [top-level-stmt
                 escape-stmt
                 top-class-stmt]
                [escape-stmt
                 (require require-spec ...)
                 (module module-body ...)]
                [top-class-stmt
                 (class-name name)
                 class-stmt]
                [class-stmt
                 (extends parent-name)
                 stmt]]

Where @racket[escape-stmt]s are put directly into the surrounding
module, and @racket[top-class-stmt]s are interpreted as a GDLisp
class.

Statements and expressions have the following grammar:

@racketgrammar*[#:literals
                [cond else let begin define
                 class signal var func
                 static : := match
                 for or export ..]

                [stmt
                 (define maybe-var-prefix binding)
                 (var maybe-var-prefix binding)
                 (define maybe-static (name-id func-param ...)
                   body-expr ...+)
                 (func maybe-static (name-id func-param ...)
                   body-expr ...+)
                 (class name-id
                   class-stmt ...)
                 (signal name-id)
                 (begin stmt ...)
                 stmt-expr]
                [func-param
                 name-id
                 [binding]]
                [maybe-var-prefix
                 code:blank
                 export
                 (export export-arg ...)]
                [maybe-static
                 code:blank
                 static]
                [binding
                 name-id
                 {code:line name-id : type-id}
                 {code:line name-id : type-id default-expr}
                 {code:line name-id := default-expr}]

                [expr
                 (begin body-expr ...)
                 variable-id
                 (.-field-name target-expr)
                 (.method-name target-expr method-arg-expr ...)
                 (function-expr function-arg-expr ...)
                 literal-expr
                 special-expr]

                [literal-expr
                 [list-element-expr ...]
                 {kv-pair ...}]

                [special-expr
                 cond-expr
                 let-expr
                 recur-expr
                 match-expr
                 for-expr]

                [kv-pair
                 {code:line key-expr val-expr}]

                [body-expr
                 (define binding)
                 expr]

                [cond-expr
                 (cond
                   [pred-expr then-body-expr ...+] ...
                   maybe-else-clause)]
                [maybe-else-clause
                 code:blank
                 [else else-body-expr ...+]]

                [let-expr
                 (let maybe-let-name
                     (binding ...)
                   body-expr ...+)]
                [maybe-let-name
                 code:blank
                 name-id]

                [recur-expr
                 (recur recur-target-id arg-expr ...)]

                [match-expr
                 (match target-expr
                   [match-pattern body-expr ...+]
                   ...)]
                [match-pattern
                 {code:line (or subpattern ...) @code:comment{where subpattern has no (var _) patterns}}
                 subpattern]
                [subpattern
                 constant-pattern
                 variable-id
                 _
                 (var name-id)
                 [subpattern ...]
                 [subpattern ... ..]
                 {kv-pattern ...}
                 {kv-pattern ... ..}]
                [kv-pattern
                 {code:line constant-pattern subpattern}]
                [constant-pattern
                 constant-number
                 constant-string
                 constant-boolean]

                [for-expr
                 (for ([name-id target-expr])
                   body-expr ...+)]]

@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{Syntax transformers}
are expanded (roughly) as usual.

@subsection{Bindings}

@deftogether[[@defidform[or]
              @defidform[and]
              @defidform[if]
              @defidform[when]]]{

Analogous to the equivalent bindings from @racket[racket/base].

}

@deftogether[[@defidform[class-name]
              @defidform[extends]
              @defidform[export]
              @defidform[match]
              @defidform[var]
              @defidform[signal]
              @defidform[func]
              @defidform[static]
              @defidform[class]
              @defidform[..]
              @defidform[:]
              @defidform[:=]
              @defidform[#%gdscript]]]{

These have transformer bindings that prohibit them from being used
outside of their context.

}

@subsection{Caveats}

There are a few caveats and things to note about GDLisp.

@subsubsection{Name mangling}

Racket identifiers used as variables are mangled before being emitted
as GDScript, so they are valid GDScript identifiers. Most notably,
hyphens ("-") are converted to underscores ("_"). Names that mangle to
the same identifier @italic{will} conflict. @bold{This does also mean
that macros which introduce variable bindings are unhygienic.} Use
@racket[(gensym)] if you need a name which doesn't conflict.
