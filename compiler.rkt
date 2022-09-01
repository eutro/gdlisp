#lang typed/racket/base

(require "utils.rkt"
         racket/format
         racket/match)

(provide class-ir ClassIr
         binding Binding
         Stmt
         (struct-out expr-ir)
         ExprIr
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
               (list header ": " (mangle h) " = " value (linebreak))])))
         'expr)]
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
         (linebreak)))
      'expr)]

    [(list 'begin stmts)
     (for/list : (Listof Code)
               ([stmt (in-list stmts)])
       (emit-stmt stmt))]

    [(list 'expr expr)
     (emit-expr
      expr
      (λ (code value)
        (list code
              value (linebreak)))
      'expr)]))

(define-enum Expr
  (const Any)
  (begin (Listof ExprIr))
  (cond (Listof (Pair ExprIr ExprIr))
        (Option ExprIr))
  (let (Option String)
    (Listof Binding)
    ExprIr)
  (recur String (Listof ExprIr))
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

(define-type Position (U 'stmt 'expr 'tail))

(: expr-null StackVal)
(define expr-null (list 'expr "null"))

(: block-expr-in-pos (-> Position (Values Code Code StackVal)))
(define (block-expr-in-pos pos)
  (case pos
    [(expr tail)
     (define temp (gentemp))
     (values
      (list "var " temp (linebreak))
      (list temp " = ")
      (list 'var temp))]
    [(stmt)
     (values
      (void)
      (void)
      expr-null)]))

(struct tail
  ([cont-var : String]
   [tail-args : (Listof String)])
  #:type-name Tail)

(: empty-tails (Immutable-HashTable String Tail))
(define empty-tails (make-immutable-hash))

(: emit-expr (-> ExprIr (-> Code Code Code) Position Code))
(define (emit-expr expr fmt pos)
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

  (define code
    (let recur : Code
         ([expr expr]
          [pos : Position pos]
          ;; expr -> push
          ;; tail -> maybe return and push
          ;; stmt -> don't push
          [tails : (Immutable-HashTable String Tail) empty-tails]
          )
      
      (match (expr-ir-expr expr)

        [(list 'const v)
         (cond
           [(eq? pos 'stmt)
            (list "pass" (linebreak))]
           [else
            (push! (list 'expr (~s v)))
            (void)])]

        [(list 'begin exprs)
         (cond
           [(null? exprs)
            (cond
              [(eq? pos 'stmt)
               (list "pass" (linebreak))]
              [else
               (push! expr-null)
               (void)])]
           [else
            (let loop ([exprs exprs])
              (define last? (null? (cdr exprs)))
              (define code
                (recur
                 (car exprs)
                 (if last? pos 'stmt)
                 (if last? tails empty-tails)))
              (if last?
                  code
                  (list code
                        (loop (cdr exprs)))))])]

        [(list 'cond clauses else-clause)
         (cond
           [(null? clauses)
            (cond
              [else-clause
               (recur else-clause pos tails)]
              [(eq? pos 'stmt)
               (list "pass" (linebreak))]
              [else
               (push! expr-null)
               (void)])]
           [else
            (define-values (header emit result)
              (block-expr-in-pos pos))
            (define code
              (list
               (ensure-evaluated!)
               header
               (let loop : Code
                    ([clause (car clauses)]
                     [next-clauses (cdr clauses)])
                 (define next-clause
                   (if (null? next-clauses)
                       #f
                       (car next-clauses)))
                 (match-define (cons predicate value) clause)
                 (list (recur predicate 'expr empty-tails)
                       "if " (val->code (pop!)) ":" (linebreak)
                       (block
                        (list
                         (recur value pos tails)
                         (unless (eq? pos 'stmt)
                           (list emit (val->code (pop!)) (linebreak)))))
                       (when (or next-clause else-clause)
                         (list
                          "else:" (linebreak)
                          (block
                           (if next-clause
                               (loop next-clause (cdr next-clauses))
                               (list
                                (recur else-clause pos tails)
                                (unless (eq? pos 'stmt)
                                  (list emit (val->code (pop!)) (linebreak))))))))))))
            (unless (eq? pos 'stmt)
              (push! result))
            code])]

        [(list 'let #f bindings body)
         (list
          (ensure-evaluated!)
          (for/list : (Listof Code)
                    ([b (in-list bindings)])
            (match-define (binding name hint value) b)
            (cond
              [value
               (list
                (recur value 'expr empty-tails)
                "var " (mangle name) " = " (val->code (pop!)) (linebreak))]
              [else
               (list "var " (mangle name) (linebreak))]))
          (recur body pos tails))]

        [(list 'let (? string? name) bindings body)
         (define-values (header emit result)
           (block-expr-in-pos pos))
         (define mangled (mangle name))
         (define cont-name (string-append "__continue_" mangled))
         (define temp-names
           (for/list : (Listof String)
                     ([b (in-list bindings)]
                      [i (in-naturals)])
             (~a "__" mangled "_arg_" i)))
         (define tailv (tail cont-name temp-names))
         (list
          (ensure-evaluated!)
          "var " cont-name " = true" (linebreak)
          (for/list : (Listof Code)
                    ([b (in-list bindings)]
                     [tmp (in-list temp-names)])
            (match-define (binding name hint value) b)
            (cond
              [value
               (list
                (recur value 'expr empty-tails)
                "var " tmp " = " (val->code (pop!)) (linebreak))]
              [else
               (list "var " tmp (linebreak))]))
          header
          "while " cont-name ":" (linebreak)
          (block
           (list
            cont-name " = false" (linebreak)
            (for/list : (Listof Code)
                      ([b (in-list bindings)]
                       [tmp (in-list temp-names)])
              (list
               "var " (mangle (binding-name b)) " = " tmp (linebreak)))
            (recur body pos (hash-set tails mangled tailv))
            (unless (eq? pos 'stmt)
              (begin0 (list emit (val->code (pop!)) (linebreak))
                (push! result))))))]

        [(list 'recur target args)
         (define mangled (mangle target))
         (unless (hash-has-key? tails mangled)
           (raise-syntax-error
            'recur
            (format "target ~s does not exist, or recur is not in tail position"
                    target)))
         (match-define (tail cont-name var-names)
           (hash-ref tails target))
         (unless (= (length var-names)
                    (length args))
           (raise-syntax-error
            'recur
            (format "target ~s takes ~a arguments, but called with ~a"
                    target
                    (length var-names)
                    (length args))))
         (begin0
             (list
              (for/list : (Listof Code)
                        ([name (in-list var-names)]
                         [val (in-list args)])
                (list
                 (recur val 'expr empty-tails)
                 name " = " (val->code (pop!)) (linebreak)))
              cont-name " = true" (linebreak))
           (unless (eq? pos 'tail)
             (push! expr-null)))]

        [(list 'for name target body)
         (begin0
             (list
              (ensure-evaluated!)
              (recur target 'expr empty-tails)
              "for " (mangle name) " in " (val->code (pop!)) ":" (linebreak)
              (block (recur body 'stmt empty-tails)))
           (unless (eq? pos 'stmt)
             (push! expr-null)))]

        [(list 'match target clauses)
         (define-values (header emit result)
           (block-expr-in-pos pos))
         (begin0
             (list
              (ensure-evaluated!)
              (recur target 'expr empty-tails)
              header
              "match " (val->code (pop!)) ":" (linebreak)
              (block
               (if (null? clauses)
                   (list "pass" (linebreak))
                   (for/list : (Listof Code) ([clause (in-list clauses)])
                     (list
                      (car clause) ":" (linebreak)
                      (block
                       (list
                        (recur (cdr clause) pos tails)
                        (unless (eq? pos 'stmt)
                          (list emit (val->code (pop!)) (linebreak))))))))))
           (unless (eq? pos 'stmt)
             (push! result)))]

        [(list 'call callee args)
         (list
          (recur callee 'expr empty-tails)
          (for/list : (Listof Code)
                    ([arg (in-list args)])
            (recur arg 'expr empty-tails))
          (let ()
            (define argvs
              (intersperse-commas
               (reverse
                (for/list : (Listof Code)
                          ([_arg (in-list args)])
                  (val->code (pop!))))))
            (define calleev
              (val->code (pop!)))
            (push! (list 'expr (list calleev "(" argvs ")")))
            (when (eq? pos 'stmt)
              (list (val->code (pop!)) (linebreak)))))]

        [(list 'var name)
         (push! (list 'var (mangle name)))
         (when (eq? pos 'stmt)
           (list (val->code (pop!)) (linebreak)))]

        [(list 'asm asm)
         (list
          (for/list : (Listof Code)
                    ([v (in-list asm)])
            (unless (string? v)
              (recur v 'expr empty-tails)))
          (let ()
            (define expr
              (reverse
               (for/list : (Listof Code)
                         ([v (in-list (reverse asm))])
                 (if (string? v)
                     v
                     (val->code (pop!))))))
            (push! (list 'expr expr))
            (when (eq? pos 'stmt)
              (list (val->code (pop!)) (linebreak)))))])))

  (fmt code (val->code (pop!))))
