#lang racket/base

(require "compiler.rkt"
         "utils.rkt"
         "extra-symbols.rkt"
         racket/format
         racket/string
         (rename-in
          racket/match
          [match rkt-match])
         (except-in
          syntax/parse
          static)
         syntax/datum
         syntax/parse/class/paren-shape
         (for-template
          racket/base
          "extra-symbols.rkt"))

(provide gd-top-class-stmts)

(define-splicing-syntax-class kv-pair
  #:description "key-value pair"
  #:attributes (key val)
  (pattern {~seq key-e:gd-expr val-e:gd-expr}
           #:attr key (attribute key-e.expr)
           #:attr val (attribute val-e.expr)))

(define-syntax-class gd-id
  #:description "identifier"
  #:attributes (stx-str dat-str)
  (pattern id:id
           #:do [(define str (~a (syntax-e #'id)))]
           #:attr dat-str str
           #:attr stx-str (datum->syntax this-syntax str)))

(define-splicing-syntax-class gd-binding
  #:description "binding"
  #:attributes (binding)
  #:literals (: :=)
  (pattern {~seq name:gd-id value:gd-expr}
           #:attr binding
           (binding
            (attribute name.dat-str)
            #f
            (attribute value.expr)))
  (pattern {~seq name:gd-id : type:gd-id}
           #:attr binding
           (binding
            (attribute name.dat-str)
            (attribute type.dat-str)
            #f))
  (pattern {~seq name:gd-id := value:gd-expr}
           #:attr binding
           (binding
            (attribute name.dat-str)
            #t
            (attribute value.expr)))
  (pattern {~seq name:gd-id : type:gd-id value:gd-expr}
           #:attr binding
           (binding
            (attribute name.dat-str)
            (attribute type.dat-str)
            (attribute value.expr))))

(define-syntax-class gd-param
  #:description "function parameter"
  #:attributes (binding)
  (pattern [:gd-binding])
  (pattern arg:gd-id
           #:attr binding
           (binding
            (attribute arg.dat-str)
            #f
            #f)))

(define-syntax-class gd-const-pattern
  #:description "constant match pattern"
  #:attributes (pattern has-var)
  (pattern n:number
           #:attr pattern (~a (syntax-e #'n))
           #:attr has-var #f)
  (pattern s:str
           #:attr pattern (~s (syntax-e #'s))
           #:attr has-var #f)
  (pattern #true
           #:attr pattern "true"
           #:attr has-var #f)
  (pattern #false
           #:attr pattern "false"
           #:attr has-var #f))

(define-syntax-class gd-pattern
  #:description "match pattern"
  #:attributes (pattern has-var)
  #:literals [_ var ..]
  #:commit
  (pattern :gd-const-pattern)
  (pattern v:gd-id
           #:attr pattern (attribute v.dat-str)
           #:attr has-var #f)
  (pattern _
           #:attr pattern "_"
           #:attr has-var #f)
  (pattern (var name:gd-id)
           #:attr pattern (format "var ~a" (attribute name.dat-str))
           #:attr has-var #t)
  (pattern [~brackets
            ~!
            {~and {~not ..} subpat:gd-pattern} ...
            {~optional {~and .. dots}}]
           #:attr pattern
           (format
            "[~a]"
            (string-append*
             (intersperse
              (append
               (datum (subpat.pattern ...))
               (if (datum {~? dots #f})
                   '("..")
                   null))
              ", ")))
           #:attr has-var
           (ormap values (datum (subpat.has-var ...))))
  (pattern {~braces
            ~!
            {~seq key:gd-const-pattern value:gd-pattern} ...
            {~optional {~and .. dots}}}
           #:attr pattern
           (format
            "{~a}"
            (string-append*
             (intersperse
              (append
               (for/list ([kv (datum ((key.pattern value.pattern) ...))])
                 (apply format "~a: ~a" kv))
               (if (datum {~? dots #f})
                   '("..")
                   null))
              ", ")))
           #:attr has-var
           (ormap values (datum (value.has-var ...)))))

(define-syntax-class gd-top-pattern
  #:description "top-level match pattern"
  #:attributes (pattern)
  #:literals [or]
  (pattern (or ~! subpat:gd-pattern ...)
           #:fail-when
           (ormap values (datum (subpat.has-var ...)))
           "(or ...) pattern is not allowed to bind variables"
           #:attr pattern
           (string-append*
            (intersperse
             (datum (subpat.pattern ...))
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
           (foldr
            (λ (v acc)
              (expr-ir
               (rkt-match v
                 [(list dfn #f)
                  `(let #f (,dfn) ,acc)]
                 [(list #f expr)
                  `(begin (,expr ,acc))])))
            (attribute last.expr)
            (datum
             (({~? bd.binding #f} {~? stmt.expr})
              ...)))))

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

(define (maybe-local-expand stx)
  (if (syntax-transforming?)
      (local-expand stx 'expression stop-exprs)
      stx))

(define-syntax-class gd-stmt
  #:description "statement"
  #:attributes (stmt)
  (pattern :gd-special-form #:with :gd-stmt-postexpand this-syntax)
  (pattern form
           #:do [(define expanded (maybe-local-expand #'form))]
           #:with :gd-stmt-postexpand expanded
           #;#;
           #:do [(displayln "--- expansion ---")
                 (writeln (syntax->datum #'form))
                 (writeln (syntax->datum expanded))
                 (writeln (datum expr))]))

(define-syntax-class gd-expr
  #:description "expression"
  #:attributes (expr)
  #:commit
  (pattern :gd-special-form
           #:with e:gd-expr-postexpand this-syntax
           #:attr expr (expr-ir (datum e.expr)))
  (pattern form
           #:do [(define expanded (maybe-local-expand #'form))]
           #:with e:gd-expr-postexpand expanded
           #:attr expr (expr-ir (datum e.expr))
           #;#;
           #:do [(displayln "--- expansion ---")
                 (writeln (syntax->datum #'form))
                 (writeln (syntax->datum expanded))
                 (writeln (datum expr))]))

(define-syntax-class gd-asm
  #:description "assembly expression"
  #:attributes (asm)
  (pattern raw:str
           #:attr asm (syntax-e #'raw))
  (pattern ({~datum !expr} exp:gd-expr)
           #:attr asm (attribute exp.expr)))

(define-syntax-class gd-fieldref
  #:attributes (name)
  (pattern ref:id
           #:do [(define sym-val (symbol->string (syntax-e #'ref)))]
           #:when (string-prefix? sym-val ".-")
           #:attr name (mangle (substring sym-val 2))))

(define-syntax-class gd-methodref
  #:attributes (name)
  (pattern ref:id
           #:do [(define sym-val (symbol->string (syntax-e #'ref)))]
           #:when (string-prefix? sym-val ".")
           #:attr name (mangle (substring sym-val 1))))

(define-syntax-class gd-expr-postexpand
  #:description "expression"
  #:attributes (expr)
  #:literals [cond else let begin for match #%gdscript]
  #:commit

  (pattern (cond
            ~!
            [{~and {~not else} pred-expr:gd-expr} then-expr:gd-block] ...
            {~optional [else else-expr:gd-block]})
           #:attr expr
           (datum
            (cond
              ((pred-expr.expr . then-expr.expr) ...)
              {~? else-expr.expr #f})))

  (pattern (let ~! {~optional name:gd-id}
             ([binding:gd-binding] ...)
             body:gd-block)
           #:attr expr
           (datum
            (let {~? name.dat-str #f}
              (binding.binding ...)
              body.expr)))

  (pattern (begin ~! e:gd-block)
           #:attr expr (expr-ir-expr (datum e.expr)))

  (pattern (for ~! ([name:gd-id target:gd-expr])
                body:gd-block)
           #:attr expr
           (datum
            (for name.dat-str target.expr
                 body.expr)))

  (pattern (match ~!
             target:gd-expr
             [pattern:gd-top-pattern body:gd-block] ...)
           #:attr expr
           (datum
            (match target.expr
              ([pattern.pattern . body.expr] ...))))

  (pattern (#%gdscript ~! asm-e:gd-asm ...)
           #:attr expr
           (datum
            (asm (asm-e.asm ...))))

  (pattern {~describe "list literal" [~brackets ~! subexprs:gd-expr ...]}
           #:attr expr
           (with-datum ([(exprs ...)
                         (intersperse (datum (subexprs.expr ...)) ", ")])
             (datum
              (asm ("[" exprs ... "]")))))

  (pattern {~describe "map literal" {~braces ~! kv:kv-pair ...}}
           #:attr expr
           (with-datum ([kvs
                         (intersperse
                          (for/list ([pair (datum ([kv.key kv.val] ...))])
                            (with-datum ([[k v] pair])
                              (datum (asm (k ": " v)))))
                          ", ")])
             (datum
              (asm ("{" (asm kvs) "}")))))

  (pattern name:gd-id #:attr expr (datum (var name.dat-str)))
  (pattern n:number #:attr expr `(const ,(syntax-e #'n)))
  (pattern s:string #:attr expr `(const ,(syntax-e #'s)))
  (pattern #true #:attr expr '(var "true"))
  (pattern #false #:attr expr '(var "false"))

  (pattern (field:gd-fieldref ~! target:gd-expr)
           #:attr expr
           (datum
            (asm (target.expr "." field.name))))
  (pattern (method:gd-methodref ~! target:gd-expr args:gd-expr ...)
           #:attr expr
           (quasidatum
            (call
             (undatum (expr-ir (datum (asm (target.expr "." method.name)))))
             (args.expr ...))))

  (pattern {~describe
            "call expression"
            (callee:gd-expr args:gd-expr ...)}
           #:attr expr
           (datum
            (call callee.expr (args.expr ...)))))

(define-syntax-class gd-var-prefix
  #:attributes (pref)
  #:literals [export]
  (pattern export
           #:attr pref "export")
  (pattern (export args ...)
           #:attr pref
           (format
            "export(~a)"
            (apply
             ~s
             #:separator ", "
             (map syntax-e (datum (args ...)))))))

(define-syntax-class gd-def
  #:attributes (stmt)
  #:description "definition"
  #:literals [var func define : static]
  (pattern ({~or* {~and var ~!} define}
            {~optional exp:gd-var-prefix}
            binding:gd-binding)
           #:attr stmt
           (datum
            (var exp.pref binding.binding)))
  (pattern ({~or* {~and func ~!} define}
            {~optional {~and static {~bind [is-static #t]}}}
            (name:gd-id params:gd-param ...)
            ~!
            {~commit
             {~seq
              {~optional {~seq : ~! return-hint:gd-id}}
              body:gd-block}})
           #:attr stmt
           (quasidatum
            (func
             {~? is-static #f}
             name.dat-str
             (params.binding ...)
             {~? return-hint.dat-str #f}
             body.expr))))

(define-syntax-class gd-stmt-postexpand
  #:attributes (stmt)
  #:description "statement"
  #:literals [signal class define begin]
  #:commit

  (pattern {~and (define ~! _ ...) :gd-def})
  (pattern :gd-def)

  (pattern (class ~! name:gd-id
             body:gd-class-stmts)
           #:attr stmt
           `(class
              ,(class-ir
                (datum name.dat-str)
                (datum body.parent)
                (datum body.stmts))))

  (pattern (signal ~! name:gd-id)
           #:attr stmt
           (datum
            (signal name.dat-str)))

  (pattern (begin ~! stmts:gd-stmt ...)
           #:attr stmt
           (datum
            (begin (stmts.stmt ...))))

  (pattern gde:gd-expr-postexpand
           #:attr stmt
           (list 'expr (expr-ir (datum gde.expr)))))

(define-splicing-syntax-class gd-class-stmts
  #:description "class statements"
  #:attributes (parent stmts)
  #:literals [extends]
  #:commit
  (pattern {~seq
            {~or* (extends ~! parent-name:gd-id)
                  {~and ~! stmt:gd-stmt}}
            ...}
           #:do [(define extendses (datum ({~? parent-name.dat-str} ...)))]
           #:fail-when (< 1 (length extendses)) "expected at most one (extends ...) statement"
           #:attr parent (if (null? extendses) #f (car extendses))
           #:attr stmts (filter values (datum ({~? stmt.stmt} ...)))))

(define-splicing-syntax-class gd-top-class-stmts
  #:description "top level statements"
  #:attributes (class-ir)
  #:literals [extends class-name]
  #:commit
  (pattern {~seq
            {~or* (class-name ~! name:gd-id)
                  (extends ~! parent-name:gd-id)
                  {~and ~! stmt:gd-stmt}}
            ...}
           #:do [(define extendses (filter values (datum ({~? parent-name.dat-str} ...))))
                 (define names (filter values (datum ({~? name.dat-str} ...))))]
           #:fail-when (< 1 (length extendses))
           (format "expected at most one (extends ...) statement, got ~s" (length extendses))
           #:fail-when (< 1 (length names))
           (format "expected at most one (class-name ...) statement, got ~s" (length names))
           #:attr class-ir
           (class-ir
            (if (null? names) #f (car names))
            (if (null? extendses) #f (car extendses))
            (filter values (datum ({~? stmt.stmt} ...))))))
