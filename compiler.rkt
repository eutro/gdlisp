#lang typed/racket/base

(require "utils.rkt"
         racket/format
         racket/match)

(provide class-ir ClassIr
         binding Binding
         Stmt
         expr-ir ExprIr
         Expr

         emit-ir
         display-code)

(define-syntax-rule
  (define-enum
    name
    (tag fields ...) ...)
  (define-type name
    (U (List 'tag fields ...) ...)))

(struct class-ir
  ([name : (Option String)]
   [parent : (Option String)]
   [stmts : (Listof Stmt)])
  #:transparent
  #:type-name ClassIr)

(struct binding
  ([name : String]
   [type-hint : (U String #t #f)]
   [value : (Option ExprIr)])
  #:transparent
  #:type-name Binding)

(struct linebreak-type ()
  #:type-name Linebreak)
(define linebreak? linebreak-type?)
(define linebreak (let ([lb (linebreak-type)]) (λ () lb)))

(struct block
  ([body : Code])
  #:transparent
  #:type-name Block)

(define-type Code
  (U String Linebreak
     Void
     Block
     (Listof Code)))

(define (display-code
         [code : Code]
         [port : Output-Port (current-output-port)])
  (define new-line? #t)
  (let recur : Any
       ([code code]
        [depth : Nonnegative-Integer 0])
    (match code
      [(? void?) (void)]
      [(? linebreak?)
       (newline port)
       (set! new-line? #t)]
      [(? block?)
       (recur (block-body code) (add1 depth))]
      [_
       (when new-line?
         (display (make-string depth #\tab) port)
         (set! new-line? #f))
       (match code
         [(? string?) (display code port)]
         [(? list?)
          (for ([c (in-list code)])
            (recur c depth))])]))
  (void))

(define (intersperse-commas [codes : (Listof Code)]) : (Listof Code)
  (intersperse codes ", "))

(define-enum Stmt
  (class ClassIr)
  (var (Option String) Binding)
  (signal String)
  (func (Option 'static)
        #;name String
        (Listof Binding)
        #;hint (Option String)
        ExprIr)
  (begin (Listof Stmt))
  (expr ExprIr))

(struct expr-ir
  ([expr : Expr])
  #:transparent
  #:type-name ExprIr)

(define (emit-ir
         [ir : ClassIr]
         #:root [root : Boolean #true]) : Code
  (when root (reset-temps!))
  (match-define (class-ir name parent body) ir)
  ((ann
    (cond
      [(not name)
       (λ (f) (f))]
      [root
       (λ (f)
         (list "class_name " (mangle name) (linebreak)
               (f)))]
      [else
       (λ (f)
         (list (linebreak)
               "class " (mangle name) ":" (linebreak)
               (block (f))
               (linebreak)))])
    (-> (-> Code) Code))
   (λ ()
     (list
      (when parent
        (list "extends " (mangle parent) (linebreak)))
      (cond
        [(and (null? body)
              (not root))
         (list "pass" (linebreak))]
        [else
         (for/list : (Listof Code)
                   ([stmt (in-list body)])
           (emit-stmt stmt))])))))

(define (emit-stmt [stmt : Stmt]) : Code
  (match stmt
    [(list 'class ir) (emit-ir ir #:root #false)]

    [(list 'var pref bd)
     (match-define (binding name hint value) bd)
     (define header : Code
       (list (if pref (list pref " ") (void))
             "var "
             (mangle name)))
     (cond
       [value
        (emit-expr
         value
         (λ (code value)
           (list
            code
            (match hint
              [#t (list header " := " value (linebreak))]
              [#f (list header " = " value (linebreak))]
              [(? string? h)
               (list header ": " (mangle h) " = " value (linebreak))]))))]
       [else
        (match hint
          [#t (list header " := null" (linebreak))]
          [#f (list header (linebreak))]
          [(? string? h) (list header ": " (mangle h) (linebreak))])])]

    [(list 'signal sig)
     (list "signal " (mangle sig) (linebreak))]

    [(list 'func static? name bindings hint expr)
     (emit-expr
      expr
      (λ (code value)
        (list
         (linebreak)
         (when static? "static ")
         "func "
         (mangle name)
         "("
         (intersperse-commas
          (for/list : (Listof Code)
                    ([bd (in-list bindings)])
            (match-define (binding name hint value) bd)
            (mangle name)))
         ")"
         (when hint (list " -> " hint))
         ":" (linebreak)
         (block
          (list
           code
           "return " value (linebreak)))
         (linebreak))))]

    [(list 'begin stmts)
     (for/list : (Listof Code)
               ([stmt (in-list stmts)])
       (emit-stmt stmt))]

    [(list 'expr expr)
     (emit-expr
      expr
      (λ (code value)
        (list code
              value (linebreak))))]))

(define-enum Expr
  (const Any)
  (begin (Listof ExprIr))
  (cond (Listof (Pair ExprIr ExprIr))
        (Option ExprIr))
  (let (Option String)
    (Listof Binding)
    ExprIr)
  (for String #;in ExprIr
       ExprIr)
  (match ExprIr
    (Listof (Pair String ExprIr)))
  (call ExprIr (Listof ExprIr))
  (var String)
  (asm (Listof (U ExprIr String))))

(define-enum StackVal
  (expr Code)
  (var String))

(define tempc : (Thread-Cellof Number)
  (make-thread-cell 0))

(define (reset-temps!) : Void
  (void (thread-cell-set! tempc 0)))

(define (gentemp) : String
  (define v (thread-cell-ref tempc))
  (begin0 (format "__temp_~a" v)
    (thread-cell-set! tempc (add1 v))))

(define (emit-expr
         [expr : ExprIr]
         [fmt : (-> Code Code Code)]) : Code
  (define stack : (Listof StackVal) null)

  (define (push! [v : StackVal])
    (set! stack (cons v stack)))

  (define (pop!) : StackVal
    (begin0 (car stack)
      (set! stack (cdr stack))))

  (define (ensure-evaluated!) : Code
    (define stack-and-code : (Listof (Pair StackVal Code))
      (for/list : (Listof (Pair StackVal Code))
                ([v (in-list stack)])
        (match v
          [(list 'expr code)
           (define temp (gentemp))
           (cons
            (list 'var temp)
            (ann (list "var " temp " = " code (linebreak))
                 Code))]
          [(list 'var var)
           (cons (list 'var var) (void))])))
    (set! stack
          (for/list : (Listof StackVal)
                    ([v (in-list stack-and-code)])
            (car v)))
    (for/list : (Listof Code)
              ([v (in-list stack-and-code)])
      (cdr v)))

  (define (val->code [val : StackVal]) : Code
    (cadr val))

  (define expr-null (list 'expr "null"))

  (define code
    (let recur : Code ([expr expr])
      (match (expr-ir-expr expr)

        [(list 'const v)
         (push! (list 'expr (~s v)))
         (void)]

        [(list 'begin exprs)
         (cond
           [(null? exprs)
            (push! expr-null)]
           [else
            (let loop ([exprs exprs])
              (define code (recur (car exprs)))
              (if (null? (cdr exprs))
                  code
                  (list code
                        (val->code (pop!))
                        (linebreak)
                        (loop (cdr exprs)))))])]

        [(list 'cond clauses else-clause)
         (cond
           [(null? clauses)
            (cond
              [else-clause
               (recur else-clause)]
              [else
               (push! expr-null)
               (void)])]
           [else
            (define temp (gentemp))
            (define code
              (list
               (ensure-evaluated!)
               "var " temp (linebreak)
               (let loop : Code
                    ([clause (car clauses)]
                     [next-clauses (cdr clauses)])
                 (define next-clause
                   (if (null? next-clauses)
                       #f
                       (car next-clauses)))
                 (match-define (cons predicate value) clause)
                 (list (recur predicate)
                       "if " (val->code (pop!)) ":" (linebreak)
                       (block
                        (list
                         (recur value)
                         temp " = " (val->code (pop!)) (linebreak)))
                       (when (or next-clause else-clause)
                         (list
                          "else:" (linebreak)
                          (block
                           (if next-clause
                               (loop next-clause (cdr next-clauses))
                               (list
                                (recur else-clause)
                                temp " = " (val->code (pop!)) (linebreak))))))))))
            (push! (list 'var temp))
            code])]

        [(list 'let name bindings body)
         (list
          (ensure-evaluated!)
          (for/list : (Listof Code)
                    ([b (in-list bindings)])
            (match-define (binding name hint value)
              b)
            (cond
              [value
               (list
                (recur value)
                "var " (mangle name) " = " (val->code (pop!)) (linebreak))]
              [else
               (list "var " (mangle name) (linebreak))]))
          (recur body))]

        [(list 'for name target body)
         (begin0
             (list
              (ensure-evaluated!)
              (recur target)
              "for " (mangle name) " in " (val->code (pop!)) ":" (linebreak)
              (block
               (list
                (recur body)
                (val->code (pop!)) (linebreak))))
           (push! expr-null))]

        [(list 'match target clauses)
         (define temp (gentemp))
         (begin0
             (list
              (ensure-evaluated!)
              (recur target)
              "var " temp (linebreak)
              "match " (val->code (pop!)) ":" (linebreak)
              (block
               (if (null? clauses)
                   (list "pass" (linebreak))
                   (for/list : (Listof Code) ([clause (in-list clauses)])
                     (list
                      (car clause) ":" (linebreak)
                      (block
                       (list
                        (recur (cdr clause))
                        temp " = " (val->code (pop!)) (linebreak))))))))
           (push! (list 'var temp)))]

        [(list 'call callee args)
         (begin0
             (list
              (recur callee)
              (for/list : (Listof Code)
                        ([arg (in-list args)])
                (recur arg)))
           (let ()
             (define argvs
               (intersperse-commas
                (reverse
                 (for/list : (Listof Code)
                           ([_arg (in-list args)])
                   (val->code (pop!))))))
             (define calleev
               (val->code (pop!)))
             (push! (list 'expr (list calleev "(" argvs ")")))))]

        [(list 'var name)
         (push! (list 'var (mangle name)))
         (void)]

        [(list 'asm asm)
         (begin0
             (for/list : (Listof Code)
                       ([v (in-list asm)])
               (unless (string? v)
                 (recur v)))
           (let ()
             (define expr
               (reverse
                (for/list : (Listof Code)
                          ([v (in-list (reverse asm))])
                  (if (string? v)
                      v
                      (val->code (pop!))))))
             (push! (list 'expr expr))))])))

  (fmt code (val->code (pop!))))
