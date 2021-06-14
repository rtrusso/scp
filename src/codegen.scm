;; codegen.scm
;; SASM generator for the MiniJava frontend

(need util/list)
(need sasm/sasm-insn)
(need pat/pat)
(need scheme/genproc)
(need ast)

;; code-generation visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-genproc codegen)

(define-genproc (codegen :codegen-program
                         (p program) (env global-symtab) (ctx context))
  (let* ((omit-main-class? (not (not (assoc 'omit-main-class (safe-contents 'context ctx)))))
         (main-class (let ((main (let ((ctx (assoc 'main-class (safe-contents 'context ctx))))
                                   (if ctx
                                       (lookup-class-type (cadr ctx) env)
                                       (@ast p :main-class)))))
                       (if omit-main-class?
                           #f
                           (if (and main
                                    (ast-node-attr? main ':potential-main-class))
                               main
                               (error "Could not find suitable main class"))))))
    (emit-code ctx
               `(extern rtl-malloc))
    (emit-code ctx
               `(extern mj-system-out-println))
    (emit-code ctx
               `(extern cp-rtl-malloc))
    (emit-code ctx
               `(extern cp-rtl-array-malloc))
    (emit-code ctx
               `(extern cp-mj-system-out-println))
    (emit-code ctx
               `(extern cp-rtl-array-length))
    (if (not omit-main-class?)
        (begin
          (emit-code ctx
                     `(export mm-rtl-heap-begin-mark-global-vars))
          (emit-code ctx
                     `(export mm-rtl-heap-end-mark-global-vars))
          (emit-code ctx
                     `(entry ,(get-entry-point main-class)))
          (emit-code ctx
                     `(global mm-rtl-heap-begin-mark-global-vars (const 0))))
        (begin
          (emit-code ctx
                     '(extern cp-rtl-add-global-root-range))
          (emit-code ctx
                     '(export $java-library-entry))
          (emit-code ctx
                     `(function (name $java-library-entry)
                                (locals 0)
                                (body (perform (op push-frame))
                                      (perform (op reserve-locals) (const 0))
                                      ,@(insn-seq:insns
                                         (insn-seq '()
                                                   '(accum index operand this)
                                                   `((assign (reg accum) (op load-array) (const begin-mark-global-vars-range) (const 0))
                                                     (branch-nonzero (label $java-library-entry/exit) (reg accum))
                                                     (assign (reg accum) (const begin-mark-global-vars-range))
                                                     (push (reg accum))
                                                     (perform (op call) (const cp-rtl-add-global-root-range))
                                                     (label $java-library-entry/exit)
                                                     (perform (op pop-frame))
                                                     (return (const 0))
                                                     )
                                                   )
                                         )
                                      )
                                ))
          (emit-code ctx
                     '(global begin-mark-global-vars-range (const 0)))
          (emit-code ctx
                     '(global end-mark-global-vars-range-pointer (label end-mark-global-vars-range)))
          )
        )
    (class-info-preamble (@ast p :class-list) env ctx)
    (for-each (lambda (class)
                (class-preamble class env ctx))
              (@ast p :class-list))
    (if (not omit-main-class?)
        (begin
          (emit-code ctx
                     `(global mm-rtl-heap-end-mark-global-vars (const 0))))
        (begin
          (emit-code ctx
                     '(global end-mark-global-vars-range (const 0)))))
    (ast-visit p
               (ast-visit-context
                '(string-constant-expression)
                (lambda (exp)
                  (codegen-declare-string-constant exp ctx))))
    (for-each (lambda (class)
                (codegen class env ctx))
              (@ast p :class-list))))

(define-genproc (codegen :codegen-class
                         (c class) (env global-symtab) (ctx context))
  (for-each (lambda (method)
              (codegen method c env ctx))
            (append (@ast c :methods)
                    (@ast c :static-methods)
                    (@ast c :constructors))))

(define-genproc (codegen :codegen-method
                         (m method) (c class) (env global-symtab) (ctx context))
  (if (not (ast-node-attr? m ':sasm-impl-no-codegen))
      (emit-code 
       ctx
       `(function 
         (name ,(class-get-method-label c m))
         (locals ,(length (@ast m :local-vars)))
         (body (perform (op push-frame))
               (perform (op reserve-locals) (const ,(length (@ast m :local-vars))))
               ,@(insn-seq:insns
                  (codegen-sequence-toplevel 
                   #t
                   (@ast m :statements)
                   m
                   c
                   env
                   (register-target 'accum)
                   (return-linkage (length (@ast m :param-list))))))))
      (emit-code ctx
                 `(extern ,(class-get-method-label c m)))
      )
  )

(define-genproc (codegen :codegen-constructor
                         (ctor constructor) (c class) (env global-symtab) (ctx context))
  (emit-code
   ctx
   `(function
     (name ,(class-get-constructor-label c ctor))
     (locals ,(length (@ast ctor :local-vars)))
     (body (perform (op push-frame))
           (perform (op reserve-locals) (const ,(length (@ast ctor :local-vars))))
           ,@(insn-seq:insns
              (codegen-sequence-toplevel
               #t
               (ctor-append-return ctor c env
                                   (ctor-prepend-superclass-ctor ctor c env (@ast ctor :statements)))
               (ast-node method
                         (:return-type (@ast ctor :return-type))
                         (:statements (@ast ctor :statements))
                         (:static? #t)
                         (:virtual-ctor #t)
                         (:local-vars (@ast ctor :local-vars))
                         (:param-list (@ast ctor :param-list)))
               c
               env
               (register-target 'accum)
               (return-linkage (length (@ast ctor :param-list)))))))))

(define (codegen-sequence-toplevel print? stmt-seq method class env target linkage)
  (cond ((null? stmt-seq) 
         (with-linkage (empty-insn-seq) linkage))
        ((null? (cdr stmt-seq))
         (let ((code (codegen (car stmt-seq) method class env target linkage)))
           (if print? (emit-statement (car stmt-seq) code))
           code))
        (else
         (let ((code (codegen (car stmt-seq) method class env target (next-linkage))))
           (if print? (emit-statement (car stmt-seq) code))
           (preserving '(this)
                       code
                       (codegen-sequence-toplevel print? (cdr stmt-seq) method class env target linkage))))))

(define (codegen-sequence stmt-seq method class env target linkage)
  (codegen-sequence-toplevel #f stmt-seq method class env target linkage))

;; statements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-genproc (codegen :codegen-return-statement
                         (stmt return-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (cond ((ast-node-attr? m ':virtual-ctor)
         (codegen (ast-node this-expression) m c env (register-target 'accum) (return-linkage (length (@ast m :param-list)))))
        (else
         (codegen (@ast stmt :exp) m c env (register-target 'accum) (return-linkage (length (@ast m :param-list)))))))

(define-genproc (codegen :codegen-compound-statement
                         (stmt compound-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-sequence (@ast stmt :statements) m c env target linkage))

(define-genproc (codegen :codegen-if-statement
                         (stmt if-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (let* ((token (make-label-token))
         (f-label (make-label 'if-else token))
         (end-label (make-label 'if-done token))
         (t-linkage (if (linkage-goto? linkage) linkage (link-goto end-label))))
    (let ((p-code (codegen (@ast stmt :condition) m c env (register-target 'accum) (next-linkage)))
          (t-code (codegen (@ast stmt :consequent) m c env target t-linkage))
          (f-code (codegen (@ast stmt :antecedent) m c env target linkage)))
      (preserving '(this)
                  p-code
                  (append-insn-seq (insn-seq '(accum) '()
                                             `((branch-zero (label ,f-label)
                                                            (reg accum))))
                                   (parallel-insn-seq
                                    t-code
                                    (append-insn-seq (label-insn-seq f-label)
                                                     f-code))
                                   (with-linkage (label-insn-seq end-label)
                                                 linkage))))))

(define-genproc (codegen :codegen-while-statement
                         (stmt while-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (let* ((token (make-label-token))
         (end-label (make-label 'while-end token))
         (test-label (make-label 'while-test token))
         (body-label (make-label 'while-body token)))
    (let ((p-code (codegen (@ast stmt :condition) m c env (register-target 'accum) (next-linkage)))
          (b-code (codegen (@ast stmt :body) m c env target (next-linkage)))
          (test-linkage (if (linkage-goto? linkage) linkage (link-goto end-label)))
          (end-linkage (if (linkage-goto? linkage) (next-linkage) linkage)))
      (preserving 
       '(this)
       (append-insn-seq
        (label-insn-seq test-label)
        p-code)
       (append-insn-seq
        (with-linkage (insn-seq '(accum) '()
                                `((branch-nonzero (label ,body-label) (reg accum))))
                      test-linkage)
        (label-insn-seq body-label)
        (preserving-loop-body '(this) b-code p-code)
        (insn-seq '() '() `((goto (label ,test-label))))
        (with-linkage (label-insn-seq end-label)
                      end-linkage))))))

(define-genproc (codegen :codegen-print-statement
                         (stmt print-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (preserving '(this)
              (codegen (@ast stmt :exp) m c env (register-target 'accum) (next-linkage))
              (with-linkage 
               (insn-seq '(accum) '(accum index operand this)
                         `((push (reg accum))
                           (perform (op call) (const cp-mj-system-out-println))))
               linkage)))

(define-genproc (codegen :codegen-assignment-statement
                         (stmt assignment-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (preserving '(this)
              (codegen (@ast stmt :exp) m c env (register-target 'accum) (next-linkage))
              (codegen-store-ident (@ast stmt :ident) m c env target linkage)))

(define (codegen-load-ident ident method class env req-target linkage)
  (let ((ident-info (lookup-ident-info ident method class env))
        (cur-target #f))
    (define (get-target)
      (if (not cur-target)
          (set! cur-target (codegen-expression-target req-target)))
      cur-target)
    (if (not ident-info)
        (error "No ident info load-ident " ident method class))
    (case (car ident-info)
      ((local arg)
       (with-expression-linkage (empty-insn-seq)
                                req-target
                                (make-target (list (car ident-info) (cdr ident-info)))
                                linkage))
      ((member)
       (with-expression-linkage (insn-seq '(this) (exp-target-registers-modified (get-target))
                                          `((assign ,(exp-assign-target (get-target))
                                                    (op load-array)
                                                    (reg this)
                                                    (const ,(+ 2 (cdr ident-info))))))
                                req-target
                                (get-target)
                                linkage))
      ((static)
       (with-expression-linkage (insn-seq '() (exp-target-registers-modified (get-target))
                                          `((assign ,(exp-assign-target (get-target))
                                                    (op load-array)
                                                    (const ,(cdr ident-info))
                                                    (const 0))))
                                req-target
                                (get-target)
                                linkage))
      (else
       (error "Bad identifier info " ident-info))) ))

(define (codegen-store-ident ident method class env target linkage)
  (let ((ident-info (lookup-ident-info ident method class env)))
     (if (not ident-info)
         (error "No ident info store-ident " ident method class))
    (with-linkage
     (case (car ident-info)
       ((local)
        (insn-seq '(accum) '()
                  `((assign (local ,(cdr ident-info)) 
                            (reg accum)))))
       ((arg)
        (insn-seq '(accum) '()
                  `((assign (arg ,(cdr ident-info)) 
                            (reg accum)))))
       ((member)
        (insn-seq '(accum this) '()
                  `((perform (op store-array) 
                             (reg this) 
                             (const ,(+ 2 (cdr ident-info))) 
                             (reg accum)))))
       ((static)
        (insn-seq '(accum) '()
                  `((perform (op store-array) 
                             (const ,(cdr ident-info)) 
                             (const 0) 
                             (reg accum)))))
       (else
        (error "Bad identifier info " ident-info)))
     linkage)))

(define-genproc (codegen :codegen-array-assignment-statement
                         (stmt array-assignment-statement) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (let ((operand-code (codegen (@ast stmt :value-exp) m c env (register-target 'operand) (next-linkage)))
        (index-code (codegen (@ast stmt :index-exp) m c env (register-target 'index) (next-linkage)))
        (ident-code (codegen-load-ident (@ast stmt :ident) m c env (register-target 'accum) (next-linkage)))
        (store-code (insn-seq '(accum index operand) '()
                              `((perform (op store-array) (reg accum) (reg index) (reg operand))))))
    (preserving '(this)
                operand-code
                (preserving '(this operand)
                            index-code
                            (preserving '(this operand index)
                                        ident-code
                                        (with-linkage store-code linkage))))))

;; boolean expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (codegen-expression-target target)
  (if (null-target? target)
      (make-target (make-temp 'temp (make-temp-token)))
      target))

(define (codegen-child-expressions helper m c env target linkage . children)
  (let ((child-codes (map (lambda (child)
                            (let* ((child-link (expression-linkage))
                                   (child-code (codegen child m c env (null-target) child-link))
                                   (child-target (expression-linkage-target child-link)))
                              (cons child-code
                                    child-target)))
                          children))
        (current-target (codegen-expression-target target)))
    (define (compute-code rest)
      (if (null? rest)
          (apply helper current-target (map cdr child-codes))
          (preserving '(this)
                      (caar rest)
                      (compute-code (cdr rest)))))
    (with-expression-linkage
     (compute-code child-codes)
     target
     current-target
     linkage)))

(define-genproc (codegen :codegen-not-expression
                         (exp not-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-child-expressions
   (lambda (derived-target child-target)
     (insn-seq (exp-target-registers-used child-target)
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         (op bit-xor)
                         ,(exp-assign-target child-target)
                         (const 1)))))
   m c env target linkage
   (@ast exp :exp)))

(define-genproc (codegen :codegen-less-than-expression
                         (exp less-than-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-child-expressions 
   (lambda (derived-target left-target right-target)
     (insn-seq (append (exp-target-registers-used left-target)
                       (exp-target-registers-used right-target))
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         (op less-than)
                         ,(exp-assign-target left-target)
                         ,(exp-assign-target right-target)))))
   m c env target linkage
   (@ast exp :left)
   (@ast exp :right)))

(define-genproc (codegen :codegen-equal-to-expression
                         (exp equal-to-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-child-expressions 
   (lambda (derived-target left-target right-target)
     (insn-seq (append (exp-target-registers-used left-target)
                       (exp-target-registers-used right-target))
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         (op equal-to)
                         ,(exp-assign-target left-target)
                         ,(exp-assign-target right-target)))))
   m c env target linkage
   (@ast exp :left)
   (@ast exp :right)))

(define-genproc (codegen :codegen-and-expression
                         (exp and-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (let* ((token (make-label-token))
         (done-label (make-label 'and-done token))
         (derived-target (codegen-expression-target target)))
    (let ((lhs-code (codegen (@ast exp :left) m c env derived-target (next-linkage)))
          (rhs-code (codegen (@ast exp :right) m c env derived-target (next-linkage))))
      (append-insn-seq
       (preserving '(this)
                   lhs-code
                   (append-insn-seq (insn-seq (exp-target-registers-used derived-target)
                                              '()
                                              `((branch-zero (label ,done-label)
                                                             ,(exp-assign-target derived-target))))
                                    rhs-code))
       (with-expression-linkage (label-insn-seq done-label)
                                target
                                derived-target
                                linkage)))))

;; binop expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (codegen-binop op exp method class env target linkage)
  (codegen-child-expressions
   (lambda (derived-target left right)
     (insn-seq (append (exp-target-registers-used left)
                       (exp-target-registers-used right))
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         ,op
                         ,(exp-assign-target left)
                         ,(exp-assign-target right)))))
   method class env target linkage
   (@ast exp :left)
   (@ast exp :right)))

(define-genproc (codegen :codegen-add-expression
                         (exp add-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-binop '(op add) exp m c env target linkage))

(define-genproc (codegen :codegen-int-binop-expression
                         (exp int-binop-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-binop `(op ,(@ast exp :op)) exp m c env target linkage))

(define-genproc (codegen :codegen-int-unop-expresion
                         (exp int-unop-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-child-expressions
   (lambda (derived-target child-target)
     (insn-seq (exp-target-registers-used child-target)
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         (op ,(@ast exp :op))
                         ,(exp-assign-target child-target)))))
   m c env target linkage
   (@ast exp :exp)))

(define-genproc (codegen :codegen-subtract-expression
                         (exp subtract-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-binop '(op sub) exp m c env target linkage))

(define-genproc (codegen :codegen-multiply-expression
                         (exp multiply-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-binop '(op mul) exp m c env target linkage))

;; array reference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-genproc (codegen :codegen-array-ref-expression
                         (exp array-ref-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-child-expressions
   (lambda (derived-target index-target array-target)
     (insn-seq (append (exp-target-registers-used index-target)
                       (exp-target-registers-used array-target))
               (exp-target-registers-modified derived-target)
               `((assign ,(exp-assign-target derived-target)
                         (op load-array)
                         ,(exp-assign-target array-target)
                         ,(exp-assign-target index-target)))))
   m c env target linkage
   (@ast exp :index-exp)
   (@ast exp :array-exp)))

;; "trivial" expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-accum-to-target target linkage)
  (let ((actual-target (codegen-expression-target target)))
    (with-expression-linkage (if (exp-target-is-specified-register? actual-target 'accum)
                                 (empty-insn-seq)
                                 (insn-seq (exp-target-registers-used (register-target 'accum))
                                           (exp-target-registers-modified actual-target)
                                           `((assign ,(exp-assign-target actual-target)
                                                     ,(exp-assign-target (register-target 'accum))))))
                             target
                             actual-target
                             linkage)))

(define-genproc (codegen :codegen-length-expression 
                         (exp length-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (preserving '(this)
              (codegen (@ast exp :exp) m c env (register-target 'accum) (next-linkage))
              (append-insn-seq
               (insn-seq '(accum) '(accum index operand this)
                         `((push (reg accum))
                           (perform (op call) (const cp-rtl-array-length))))
               (move-accum-to-target target linkage))))

(define-genproc (codegen :codegen-number-constant-expression
                         (exp number-constant-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (with-expression-linkage (empty-insn-seq)
                           target
                           (make-target `(const ,(string->number (@ast exp :value))))
                           linkage))

(define (codegen-parse-char-group val)
  (cond ((= 1 (string-length val))
         (string-ref val 0))
        ((and (= 2 (string-length val))
              (char=? #\\ (string-ref val 0)))
         (case (string-ref val 1)
           ((#\r) #\return)
           ((#\n) #\newline)
           ((#\t) #\tab)
           ((#\\) #\\)
           ((#\') #\')
           ((#\0) (integer->char 0))
           (else "Invalid character escape sequence" str)))
        (else
         (error "Invalid character literal " str))))

(define (codegen-parse-char-constant str)
  (cond
   ((char? str)
    str)
   ((and (starts-with? str "'")
         (ends-with? str "'"))
    (let ((val (substring str 1 (- (string-length str) 1))))
      (codegen-parse-char-group val)))
   (else
    (error "Invalid character literal " str))))

(define (codegen-parse-string-constant val)
  (let loop ((idx 0) (result '()))
    (cond ((>= idx (string-length val))
           (reverse result))
          ((and (char=? #\\ (string-ref val idx))
                (< (+ idx 1) (string-length val)))
           (loop (+ idx 2) (cons (codegen-parse-char-group (substring val idx (+ idx 2)))
                                 result)))
          ((not (char=? #\\ (string-ref val idx)))
           (loop (+ idx 1) (cons (codegen-parse-char-group (substring val idx (+ idx 1)))
                                 result)))
          (else
           (error "Invalid character escape sequence" str idx)))))

(define-genproc (codegen :codegen-char-constant-expression 
                         (exp char-constant-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (with-expression-linkage (empty-insn-seq)
                           target
                           (make-target `(const ,(char->integer (codegen-parse-char-constant (@ast exp :value)))))
                           linkage))

(define-genproc (codegen :codegen-string-constant-expression
                         (exp string-constant-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (with-expression-linkage (empty-insn-seq)
                           target
                           (make-target `(label ,(@ast exp :strconst-label)))
                           linkage))

(define codegen-declare-string-constant
  (let ((strconst-counter 0))
    (define (emit-strconst-declaration emit-code-ctx string label)
      (emit-code
       emit-code-ctx
       `(global ,label (const ,string))))
    (define (actual-string-value exp)
      (let ((str (@ast exp :value)))
        (if (and (starts-with? str "\"")
                 (ends-with? str "\""))
            (substring str 1 (- (string-length str) 1))
            (error "Malformed string constant -- " str))))
    (lambda (exp emit-code-ctx)
      (let* ((val (actual-string-value exp))
             (chars (codegen-parse-string-constant val))
             (label (begin
                      (set! strconst-counter (+ strconst-counter 1))
                      (string->symbol (string-append "$strconst-label-"
                                                     (number->string
                                                         strconst-counter))))))
        (emit-strconst-declaration emit-code-ctx (list->string chars) label)
        (ast-node-attr! exp ':strconst-label label)))))

(define-genproc (codegen :codegen-boolean-constant-expression
                         (exp boolean-constant-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (with-expression-linkage (empty-insn-seq)
                           target
                           (make-target `(const ,(if (@ast exp :scheme-value) 1 0)))
                           linkage))

(define-genproc (codegen :codegen-variable-expression
                         (exp variable-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (codegen-load-ident (@ast exp :ident) m c env target linkage))

(define-genproc (codegen :codegen-this-expression
                         (exp this-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (with-expression-linkage (empty-insn-seq)
                           target
                           (register-target 'this)
                           linkage))

(define-genproc (codegen :codegen-new-array-expression
                         (exp new-array-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (preserving '(this)
              (codegen (@ast exp :length) m c env (register-target 'accum) (next-linkage))
              (append-insn-seq
               (insn-seq '(accum) '(accum index operand this)
                         `((push (reg accum))
                           (perform (op call) (const cp-rtl-array-malloc))))
               (move-accum-to-target target linkage))))

(define-genproc (codegen :codegen-new-object-expression
                         (exp new-object-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (define exp-class (lookup-class-type (@ast exp :class-name) env))
  (append-insn-seq
   (insn-seq '() '(accum index operand this)
             `((push (const ,(class-get-word-size exp-class env)))
               (perform (op call) (const cp-rtl-malloc))
               (perform (op store-array) (reg accum) (const 0) (label ,(class-get-name-label exp-class)))))
   (if (not (null? (@ast exp-class :constructors)))
       (append-insn-seq 
        (insn-seq '(accum) '(this)
                  `((assign (reg this) (reg accum))))
        (codegen (ast-node method-call-expression
                           (:object (@ast exp :class-name))
                           (:explicit-method-class-name (@ast exp :class-name))
                           (:explicit-method (@ast exp :target-ctor))
                           (:arg-list (@ast exp :arg-list))
                           (:requires-this #t)
                           (:constructor-call #t)
                           (:method-name (class-get-constructor-method-name exp-class 
                                                                            (@ast exp :target-ctor))))
                 m c env (register-target 'accum) (next-linkage)))
       (empty-insn-seq))
   (move-accum-to-target target linkage)))

;; method call expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-genproc (codegen :codegen-method-call-expression
                         (exp method-call-expression) (m method) (c class) (env global-symtab) (target <target>) (linkage <linkage>))
  (define (codegen-arg param-exp)
    (append-insn-seq
     (codegen param-exp m c env (register-target 'accum) (next-linkage))
     (insn-seq '(accum) '()
               `((push (reg accum))))))
  (define (codegen-arg-list reverse-list)
    (if (null? reverse-list)
        (empty-insn-seq)
        (preserving '(this)
                    (codegen-arg (car reverse-list))
                    (codegen-arg-list (cdr reverse-list)))))
  (define (get-object-class)
    (cond ((ast-node-attr? exp ':explicit-method-class-name)
           (lookup-class-type (@ast exp :explicit-method-class-name) env))
          ((ast-node-attr? exp ':static-method-call)
           (let ((classref (@ast exp :object)))
             (lookup-class-type (@ast (typecheck classref m c env) :name) env)))
          (else
           (let ((u (typecheck (@ast exp :object) m c env)))
             (and u
                  (user-type? u)
                  (user-type->class-type u env))))))
  (define (get-method)
    (cond ((ast-node-attr? exp ':explicit-method)
           (@ast exp :explicit-method))
          (else
           (error "Explicit method not defined for method call"))))
; NOTE - these clauses in (get-method) are out-of-date.
;        codegen now assumes that typechk finds the correct method
;        explicitly
;
;          ((ast-node-attr? exp ':static-method-call)
;           (lookup-static-method (@ast exp :method-name)
;                                 (get-object-class)
;                                 env))
;          (else
;           (lookup-method (@ast exp :method-name) (get-object-class) env))))
  (define (get-call-op)
    (cond ((tailcall? exp)
           '(op tail-call))
          ((or (ast-node-attr? exp ':constructor-call)
               (not (@ast (get-method) :static?)))
           '(op this-call))
          (else
           '(op call))))
  (define (get-tailcall-extras)
    (if (tailcall? exp)
        `((const ,(length (@ast exp :arg-list)))
          (const ,(length (@ast m :param-list))))
        '()))
  (define (get-call-insns)
    (if (@ast (get-method) :static?)
        (get-static-call-insns)
        (get-dynamic-call-insns)))
  (define (get-dynamic-call-insns)
    (append-insn-seq
     (codegen (@ast exp :object) m c env (register-target 'this) (next-linkage))
     (insn-seq '(this accum) '(accum index operand this)
               `((assign (reg accum) (op load-array) (reg this) (const 0))
                 (assign (reg accum) (op load-array) (reg accum) (const ,(+ 1 (class-get-method-offset (get-method) (get-object-class)  env))))
                 (perform ,(get-call-op) (reg accum) ,@(get-tailcall-extras))))))
  (define (get-static-call-insns)
    (insn-seq (if (ast-node-attr? exp ':constructor-call)
                  '(this)
                  '())
              '(accum index operand this)
              `((perform ,(get-call-op) (const ,(class-get-method-label (get-object-class) (get-method))) ,@(get-tailcall-extras)))))
  (preserving
   '(this)
   (codegen-arg-list (reverse (@ast exp :arg-list)))
   (preserving '(this)
               (get-call-insns)
               (move-accum-to-target target linkage))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (class-get-name-label class)
  (string->symbol (string-append "$class_" (@ast class :class-name))))

(define (class-get-method-label class method)
  (if (ast-node-attr? method ':static-method-alternate-identifier)
      (string->symbol (@ast method :static-method-alternate-identifier))
      (let ((candidates (filter (lambda (m) (method-name=? (@ast method :name) (@ast m :name)))
                                (append (@ast class :methods)
                                        (@ast class :static-methods)
                                        (@ast class :constructors)))))
        (cond ((null? candidates)
               (error "Unable to derive class method label " class method))
              (else
               (string->symbol (string-append "$class_" (@ast class :class-name)
                                              "$$" (@ast method :name)
                                              (if (= 1 (length candidates))
                                                  ""
                                                  (number->string (index-of method eq? candidates))))))))))

(define (class-get-constructor-method-name class constructor)
  (string-append "$ctor_" 
                 (number->string (index-of constructor eq? (@ast class :constructors)))))

(define (class-get-constructor-label class constructor)
  (string->symbol (string-append "$class_" (@ast class :class-name)
                                 "$$" (class-get-constructor-method-name class constructor))))

(define (class-get-static-var-label class static-var)
  (string->symbol (string-append "$static_class_" (@ast class :class-name)
                                 "$$" (@ast static-var :name))))

(define (class-find-method-in-table method-table method)
  (define (find-method m table)
    (cond ((null? table) #f)
          ((and (method-name=? (@ast m :name) (@ast (caar table) :name))
                (methods-equivalent? m (caar table)))
           table)
          (else
           (find-method m (cdr table)))))
  (find-method method method-table))

(define (class-get-method-table class env)
  (define (coalesce-method labeled-method table)
    (let ((entry (class-find-method-in-table table (car labeled-method))))
      (if entry
          (begin (set-car! entry labeled-method)
                 table)
          (append table 
                  (list labeled-method)))))
  (define (coalesce-all labeled-methods table)
    (if (null? labeled-methods)
        table
        (coalesce-all (cdr labeled-methods)
                      (coalesce-method (car labeled-methods) table))))
  (coalesce-all (map (lambda (x) (cons x (class-get-method-label class x)))
                     (@ast class :methods))
                (if (@ast class :parent)
                    (class-get-method-table (lookup-class-type (@ast class :parent) env)
                                            env)
                    '())))

(define (class-get-method-offset method class env)
  (let loop ((tab (class-get-method-table class env))
             (idx 0))
    (cond ((null? tab)
           (error "Unable to find method " (@ast method :name) " in class "
                  (@ast class :class-name))
           #f)
          ((eq? method (caar tab))
           idx)
          (else
           (loop (cdr tab) (+ idx 1))))))

(define (class-get-member-offset member class env)
  (or (and (identifier-assoc member (@ast class :instance-vars))
           (+ (identifier-offset member (@ast class :instance-vars))
              (if (@ast class :parent)
                  (class-get-size (lookup-class-type (@ast class :parent) env) env)
                  0)))
      (and (@ast class :parent)
           (class-get-member-offset member 
                                    (lookup-class-type (@ast class :parent) env)
                                    env))))

(define (class-get-size class env)
  (+ (length (@ast class :instance-vars))
     (if (@ast class :parent)
         (class-get-size (lookup-class-type (@ast class :parent) env) env)
         0)))

(define (class-get-word-size class env)
  (+ 2 (class-get-size class env)))

(define (lookup-ident-info ident method class env)
  (let ((resolved (resolve-identifier ident method class env)))
    (and 
     resolved
     (case (list-ref resolved 1)
       ((local)
        (cons 'local
              (identifier-offset ident (@ast method :local-vars))))
       ((arg)
        (cons 'arg
              (identifier-offset ident (@ast method :param-list))))
       ((member)
        (let ((offset (class-get-member-offset ident class env)))
          (and offset (cons 'member offset))))
       ((static)
        (cons 'static
              (class-get-static-var-label (list-ref resolved 2)
                                          (ast-node static-var
                                                    (:name ident)))))
       (else
        (error "lookup-ident-info failed " ident method class env))))))

(define (class-preamble class env context)
  (emit-code context
             `(export ,(class-get-name-label class)))
  (for-each (lambda (ctor)
              (emit-code context
                         `(export ,(class-get-constructor-label class ctor))))
            (@ast class :constructors))
  (for-each (lambda (method-entry)
              (emit-code context
                         `(export ,(cdr method-entry))))
            (class-get-method-table class env))
  (emit-code context
             `(class ,(class-get-name-label class)
                     (const ,(class-get-size class env))
                     ,@(map (lambda (x) (list 'label (cdr x)))
                            (class-get-method-table class env))))
  (for-each (lambda (static-var)
              (emit-code context
                         `(global ,(class-get-static-var-label class static-var))))
            (@ast class :static-vars)))

(define (class-info-preamble class-list env context)
  (define (type-info type)
    (case (ast-node-type type)
      ((simple-type) (cadr (assoc (@ast type :name)
                                  '(("int" int) ("boolean" bool)
                                    ("void" void) ("char" char)))))
      ((array-type) `(array ,(type-info (@ast type :element-type))))
      ((user-type) `(user ,(@ast type :name)))
      (else (error "Invalid type node" (ast-node-type type)))))
  (define (var-info var)
    `(var (name ,(@ast var :name))
          (type ,(type-info (@ast var :type)))))
  (define (method-info method)
    `(method (name ,(@ast method :name))
             (return-type ,(type-info (@ast method :return-type)))
             (param-list ,@(map var-info (@ast method :param-list)))))
  (define (per-class-info class)
    `(class (name ,(@ast class :class-name))
            (parent ,(@ast class :parent))
            (instance-vars ,@(map var-info (@ast class :instance-vars)))
            (static-vars ,@(map var-info (@ast class :static-vars)))
            (instance-methods ,@(map method-info (@ast class :methods)))
            (static-methods ,@(map method-info (@ast class :static-methods)))))
  (emit-code context
             `(class-info
               ,@(map per-class-info class-list))))

(define (get-entry-point main-class)
  (class-get-method-label main-class (car (@ast main-class :static-methods))))

(define (class-find-ctor env class sig)
  (find-method-by-sig sig (@ast class :constructors) env))

(define (ctor-prepend-superclass-ctor ctor class env statements)
  (if (and (@ast class :parent)
           (or (null? statements)
               (not (eqv? 'super-statement (ast-node-type (car statements))))))
      (let* ((parent-class (lookup-class-type (@ast class :parent) env))
             (parent-ctor (class-find-ctor env parent-class '())))
        (if (not parent-ctor)
            (if (null? (@ast parent-class :constructors))
                statements
                (error "Unable to find default constructor for parent class" (@ast parent-class :constructors)))
            (cons
             (ast-node method-call-expression
                       (:object (ast-node this-expression))
                       (:arg-list '())
                       (:explicit-method-class-name (@ast parent-class :class-name))
                       (:explicit-method parent-ctor)
                       (:requires-this #t)
                       (:constructor-call #t)
                       (:method-name (class-get-constructor-method-name
                                      parent-class
                                      parent-ctor)))
             statements)))
      statements))

(define (ctor-append-return ctor class env statements)
  (let ((r (reverse statements)))
    (if (and (not (null? r))
             (eqv? 'return-statement (car r)))
        statements
        (reverse (cons (ast-node return-statement)
                       r)))))
