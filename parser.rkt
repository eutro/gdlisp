#lang racket/base

(require "compiler.rkt"
         "utils.rkt"
         "extra-symbols.rkt"
         racket/format
         racket/string
         (rename-in
          racket/match
          [match rkt-match])
         syntax/parse
         syntax/parse/class/paren-shape
         (for-template racket/base
                       "extra-symbols.rkt"))

(provide gd-top-class-stmts)

(define-splicing-syntax-class kv-pair
  #:description "key-value pair"
  #:attributes (key val)
  (pattern {~seq key-e:gd-expr val-e:gd-expr}
           #:attr key #'key-e.expr
           #:attr val #'val-e.expr))

(define-syntax-class gd-id
  #:description "identifier"
  #:attributes (str)
  (pattern id:id
           #:attr str (datum->syntax this-syntax (~a (syntax-e #'id)))))

(define-splicing-syntax-class gd-binding
  #:description "binding"
  #:attributes (binding)
  #:literals (: :=)
  (pattern {~seq name:gd-id value:gd-expr}
           #:attr
           binding
           (datum->syntax
            #'name
            (binding
             (syntax-e #'name.str)
             #f
             (syntax->datum #'value.expr))))
  (pattern {~seq name:gd-id : type:gd-id}
           #:attr
           binding
           (datum->syntax
            #'name
            (binding
             (syntax-e #'name.str)
             (syntax-e #'type.str)
             #f)))
  (pattern {~seq name:gd-id := value:gd-expr}
           #:attr
           binding
           (datum->syntax
            #'name
            (binding
             (syntax-e #'name.str)
             #t
             (syntax->datum #'value.expr))))
  (pattern {~seq name:gd-id : type:gd-id value:gd-expr}
           #:attr
           binding
           (datum->syntax
            #'name
            (binding
             (syntax-e #'name.str)
             (syntax-e #'type.str)
             (syntax->datum #'value.expr)))))

(define-syntax-class gd-param
  #:description "function parameter"
  #:attributes (binding)
  (pattern [bd:gd-binding]
           #:attr binding #'bd.binding)
  (pattern arg:gd-id
           #:attr binding
           (datum->syntax
            this-syntax
            (binding
             (syntax-e #'arg.str)
             #f
             #f))))

(define-syntax-class gd-const-pattern
  #:description "constant match pattern"
  #:attributes (pattern has-var)
  (pattern n:number
           #:attr pattern (datum->syntax this-syntax (~a (syntax-e #'n)))
           #:attr has-var #'#f)
  (pattern s:str
           #:attr pattern (datum->syntax this-syntax (~s (syntax-e #'s)))
           #:attr has-var #'#f)
  (pattern #true
           #:attr pattern "true"
           #:attr has-var #'#f)
  (pattern #false
           #:attr pattern "false"
           #:attr has-var #'#f))

(define-syntax-class gd-pattern
  #:description "match pattern"
  #:attributes (pattern has-var)
  #:literals [_ var ..]
  #:commit
  (pattern :gd-const-pattern)
  (pattern v:gd-id
           #:attr pattern #'v.str
           #:attr has-var #'#f)
  (pattern _
           #:attr pattern #'"_"
           #:attr has-var #'#f)
  (pattern (var name:gd-id)
           #:attr pattern (datum->syntax this-syntax (format "var " (syntax-e #'name.str)))
           #:attr has-var #'#t)
  (pattern [~brackets
            ~!
            {~and {~not ..} subpat:gd-pattern} ...
            {~optional {~and .. dots}}]
           #:attr pattern
           (datum->syntax
            this-syntax
            (format
             "[~a]"
             (string-append*
              (intersperse
               (append
                (syntax->datum #'(subpat.pattern ...))
                (if (syntax-e #'{~? dots #f})
                    '("..")
                    null))
               ", "))))
           #:attr has-var
           (datum->syntax
            this-syntax
            (ormap values
                   (syntax->datum #'(subpat.has-var ...)))))
  (pattern {~braces
            ~!
            {~seq key:gd-const-pattern value:gd-pattern} ...
            {~optional {~and .. dots}}}
           #:attr pattern
           (datum->syntax
            this-syntax
            (format
             "{~a}"
             (string-append*
              (intersperse
               (append
                (for/list ([kv (syntax->datum #'((key.pattern value.pattern) ...))])
                  (apply format "~a: ~a" kv))
                (if (syntax-e #'{~? dots #f})
                    '("..")
                    null))
               ", "))))
           #:attr has-var
           (datum->syntax
            this-syntax
            (ormap values
                   (syntax->datum #'(value.has-var ...))))))

(define-syntax-class gd-top-pattern
  #:description "top-level match pattern"
  #:attributes (pattern)
  #:literals [or]
  (pattern (or ~! subpat:gd-pattern ...)
           #:fail-when
           (ormap values
                  (syntax->datum #'(subpat.has-var ...)))
           "(or ...) pattern is not allowed to bind variables"
           #:attr pattern
           (string-append*
            (intersperse
             (syntax->datum #'(subpat.pattern ...))
             ", ")))
  (pattern :gd-pattern))

(define-splicing-syntax-class gd-block
  #:description "sequence of expressions"
  #:attributes (expr)
  #:literals [define]
  (pattern {~seq {~or* (define ~! bd:gd-binding)
                       stmt:gd-expr}
                 ...
                 last:gd-expr}
           #:attr expr
           (datum->syntax
            #f
            (foldr
             (λ (v acc)
               (rkt-match v
                 [(cons 'define dfn)
                  `(let #f (,dfn) ,acc)]
                 [(cons 'expr expr)
                  `(begin (,expr ,acc))]))
             (syntax->datum #'last.expr)
             (syntax->datum
              #'({~? (define . bd.binding)
                     {~? (expr . stmt.expr)}}
                 ...))))))

(module+ test
  (syntax-parse #'((define x 10) x)
    [(block:gd-block) #'block.expr]))

(define stop-exprs
  (syntax-e
   #'[cond let begin for match
      var func define
      signal class
      extends class-name
      #%gdscript
      #%app #%datum #%top]))

(define-syntax-class gd-special-form-name
  #:literals [cond let begin for match
              var func define
              signal class
              extends class-name
              #%gdscript]
  (pattern {~or*
            cond let begin for match
            var func define
            signal class
            extends class-name
            #%gdscript}))

(define-syntax-class gd-special-form
  (pattern {~or* :gd-special-form-name
                 (:gd-special-form-name . _)}))

(define-syntax-class gd-stmt
  #:description "statement"
  #:attributes (stmt)
  (pattern :gd-special-form #:with :gd-stmt-postexpand this-syntax)
  (pattern form
           #:do [(define expanded
                   (local-expand #'form 'expression stop-exprs))
                 #;
                 [(writeln (syntax->datum #'form))
                  (writeln (syntax->datum expanded))]]
           #:with :gd-stmt-postexpand expanded))

(define-syntax-class gd-expr
  #:description "expression"
  #:attributes (expr)
  #:commit
  (pattern :gd-special-form
           #:with :gd-expr-postexpand this-syntax)
  (pattern form
           #:do [(define expanded
                   (local-expand #'form 'expression stop-exprs))
                 #;
                 [(writeln (syntax->datum #'form))
                  (writeln (syntax->datum expanded))]]
           #:with :gd-expr-postexpand expanded))

(define-syntax-class gd-asm
  #:description "assembly expression"
  #:attributes (asm)
  (pattern raw:str
           #:attr asm #'raw)
  (pattern ({~datum !expr} exp:gd-expr)
           #:attr asm #'exp.expr))

(define-syntax-class gd-expr-postexpand
  #:description "expression"
  #:attributes (expr)
  #:literals [cond else let begin for match #%gdscript]
  #:commit

  (pattern (cond
            ~!
            [{~and pred-expr:gd-expr {~not else}} then-expr:gd-block] ...
            {~optional [else else-expr:gd-block]})
           #:attr expr
           (syntax/loc this-syntax
             (cond
               ((pred-expr.expr . then-expr.expr) ...)
               {~? else-expr.expr #f})))

  (pattern (let ~! ([binding:gd-binding] ...)
             body:gd-block)
           #:attr expr
           (syntax/loc this-syntax
             (let #f (binding.binding ...)
               body.expr)))

  (pattern (begin ~! :gd-block))

  (pattern (for ~! ([name:gd-id target:gd-expr])
                body:gd-block)
           #:attr expr
           (syntax/loc this-syntax
             (for name.str target.expr
                  body.expr)))

  (pattern (match ~!
             target:gd-expr
             [pattern:gd-top-pattern body:gd-block] ...)
           #:attr expr
           (syntax/loc this-syntax
             (match target.expr
               ([pattern.pattern . body.expr] ...))))

  (pattern (#%gdscript ~! asm-e:gd-asm ...)
           #:attr expr
           (syntax/loc this-syntax
             (asm (asm-e.asm ...))))

  (pattern {~describe "list literal" [~brackets ~! subexprs:gd-expr ...]}
           #:attr expr
           (with-syntax ([(exprs ...) (intersperse (syntax-e #'(subexprs.expr ...)) ", ")])
             (syntax/loc this-syntax
               (asm ("[" exprs ... "]")))))

  (pattern {~describe "map literal" {~braces ~! kv:kv-pair ...}}
           #:attr
           expr
           (let ([kvs (intersperse
                       (for/list ([pair (syntax-e #'([kv.key kv.val] ...))])
                         (with-syntax ([[k v] pair])
                           #'(asm (k ": " v))))
                       ", ")])
             (datum->syntax
              this-syntax
              `(asm ("{" (asm ,kvs) "}")))))

  (pattern name:gd-id #:attr expr (syntax/loc this-syntax (var name.str)))
  (pattern n:number #:attr expr (syntax/loc this-syntax (const n)))
  (pattern n:string #:attr expr (syntax/loc this-syntax (const n)))
  (pattern #true #:attr expr (syntax/loc this-syntax (var "true")))
  (pattern #false #:attr expr (syntax/loc this-syntax (var "false")))

  (pattern {~describe "call expression" (callee:gd-expr args:gd-expr ...)}
           #:attr
           expr
           (syntax/loc this-syntax
             (call callee.expr (args.expr ...)))))

(define-syntax-class gd-def
  #:attributes (stmt)
  #:description "definition"
  #:literals [var func define]
  (pattern ({~or* var define} binding:gd-binding)
           #:attr
           stmt
           (syntax/loc this-syntax
             (var binding.binding)))
  (pattern ({~or* func define}
            (name:gd-id params:gd-param ...)
            body:gd-block)
           #:attr
           stmt
           (datum->syntax
            this-syntax
            (list
             'func
             #f
             (syntax-e #'name.str)
             (syntax->datum #'(params.binding ...))
             #f
             (syntax->datum #'body.expr)))))

(define-syntax-class gd-stmt-postexpand
  #:attributes (stmt)
  #:description "statement"
  #:literals [signal class define begin]
  #:commit

  (pattern :gd-def)
  (pattern (define ~! _ ...)
           ;; fail with gd-def
           #:with :gd-def this-syntax)

  (pattern (class ~! name:gd-id
             body:gd-class-stmts)
           #:attr
           stmt
           (datum->syntax
            this-syntax
            (list
             'class
             (class-ir
              (syntax-e #'name.str)
              (syntax-e #'body.parent)
              (syntax->datum #'body.stmts)))))

  (pattern (signal ~! name:gd-id)
           #:attr
           stmt
           (syntax/loc this-syntax
             (signal name.str)))

  (pattern (begin ~! stmts:gd-stmt ...)
           #:attr
           stmt
           (syntax/loc this-syntax
             (begin (stmts.stmt ...))))

  (pattern gde:gd-expr-postexpand
           #:attr
           stmt
           (syntax/loc this-syntax
             (expr gde.expr))))

(define-splicing-syntax-class gd-class-stmts
  #:description "class statements"
  #:attributes (parent stmts)
  #:literals [extends]
  #:commit
  (pattern {~seq
            {~or* (extends ~! parent-name:gd-id)
                  stmt:gd-stmt}
            ...}
           #:do [(define extendses (syntax-e #'({~? parent-name.str} ...)))]
           #:fail-when (< 1 (length extendses)) "expected at most one (extends ...) statement"
           #:attr parent (if (null? extendses) #'#f (car extendses))
           #:attr stmts #'({~? stmt.stmt} ...)))

(define-splicing-syntax-class gd-top-class-stmts
  #:description "top level statements"
  #:attributes (class-ir)
  #:literals [extends class-name]
  #:commit
  (pattern {~seq
            {~or* (class-name ~! name:gd-id)
                  (extends ~! parent-name:gd-id)
                  stmt:gd-stmt}
            ...}
           #:do [(define extendses (syntax-e #'({~? parent-name.str} ...)))
                 (define names (syntax-e #'({~? name.str} ...)))]
           #:fail-when (< 1 (length extendses)) "expected at most one (extends ...) statement"
           #:fail-when (< 1 (length names)) "expected at most one (class-name ...) statement"
           #:attr
           class-ir
           (datum->syntax
            #f
            (class-ir
             (if (null? names) #f (syntax-e (car names)))
             (if (null? extendses) #f (syntax-e (car extendses)))
             (syntax->datum #'({~? stmt.stmt} ...))))))

(module+ test
  (define (syntax->ir stx)
    (syntax-e
     (syntax-parse stx
       [(class:gd-top-class-stmts)
        #'class.class-ir])))

  (display-code
   (emit-ir
    (syntax->ir
     #'((class-name MyClass)
        (extends BaseClass)
        (@icon "res://path/to/optional/icon.svg")
        (define a 10)
        (define s "Hello")
        (define arr [1 2 3])
        (define dict
          {"key" "value" 2 3})
        (define typed-var : int)
        (define inferred-type := "String")

        (define v1 (Vector2 1 2))
        (define v2 (Vector3 1 2 3))

        (define (some-function param1 param2 param3)
          (let ([local-const 5])
            (begin
              (cond
                [(< param1 local-const)
                 (print param1)]
                [(> param2 5)
                 (print param2)]
                [else
                 (print "Fail!")])

              (let ([local-var (+ param1 3)])
                local-var))))

        (define (something p1 p2)
          (super p1 p2))

        (define (other-something p1 p2)
          (super.something p1 p2))

        (class Something
          (define a 10))

        (define (_init)
          (begin
            (print "Constructed")
            (let ([lv (Something.new)])
              (print lv.a))))

        ;
        ))))

  ;
  )
