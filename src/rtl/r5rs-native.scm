;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gc-array-mask -2147483648)

(define <integer> '(const 1))
(define <bool> '(const 2))
(define <char> '(const 3))
(define <pair> `(const ,(+ gc-array-mask 4)))
(define <vector> `(const ,(+ gc-array-mask 5)))
(define <string> '(const 6))
(define <symbol> '(const 7))
(define <nil> '(const 8))
(define <proc> `(const ,(+ gc-array-mask 9)))
(define <input-port> '(const 10))
(define <output-port> '(const 11))
(define <eof-object> '(const 12))
(define <unspecified> '(const 314159))

(define (argument-count-check n)
  (let* ((token (make-label-token))
         (ok-label (make-label 'argument-count-check token)))
    `((assign (reg accum) (op equal-to) (reg index) (const ,n))
      (branch-nonzero (label ,ok-label) (reg accum))
      (assign (reg accum) (const ,n))
      (push (reg accum))
      (push (reg index))
      (perform (op call) (label scheme-rtl-runtime-argument-count-error))
      (label ,ok-label))))

(define (predicate-impl tag)
  (let* ((token (make-label-token))
         (done-label (make-label 'type-predicate-done token))
         (check-type-label (make-label 'type-predicate-check token))
         )
    `(:enter
      ,@(argument-count-check 1)
      (assign (reg accum) obj)
      (assign (reg accum) (op equal-to) (reg accum) (const 0))
      (branch-zero (label ,check-type-label) (reg accum))
      (assign (reg accum) (label $scmliteral-false))
      (goto (label ,done-label))
      (label ,check-type-label)
      (assign (reg accum) obj)
      (assign (reg accum) (op load-array) (reg accum) (const 1))
      (assign (reg accum) (op equal-to) (reg accum) ,tag)
      (assign (reg operand) (label $scmliteral-boolean))
      (assign (reg accum) (op load-array) (reg operand) (reg accum))
      (label ,done-label)
      :leave)))

;; the code produced by runtime-type-check is allowed use of the accum
;; register.  "obj" is the input register which contains a pointer to
;; the associated object to be checked.  "tag" is a type tag value of
;; the form "(const N)", which is the required type of the object.
;; If the typecheck fails, the code will terminate the given
;; process.
(define (runtime-type-check obj tag)
  (if (equal? tag '(reg accum))
      (error "runtime-type-check: input must not be (reg accum)"))
  (let* ((token (make-label-token))
         (ok-label (make-label 'rttc-ok token))
         (err-label (make-label 'rttc-fail token))
         )
    ;; we are only allowed to stomp on (reg accum) in this code.
    `((assign (reg accum) ,obj)
      (assign (reg accum) (op equal-to) (reg accum) (const 0))
      (branch-nonzero (label ,err-label) (reg accum))
      (assign (reg accum) ,obj)
      (assign (reg accum) (op load-array) (reg accum) (const 1))
      (assign (reg accum) (op equal-to) (reg accum) ,tag)
      (branch-nonzero (label ,ok-label) (reg accum))
      (label ,err-label)
      ;; set up a call to the "ZOMG" function.  Notice that this
      ;; isn't saving registers because it is not expected to
      ;; return.
      (assign (reg accum) ,obj)
      (push (reg accum))
      (assign (reg accum) ,tag)
      (push (reg accum))
      (perform (op call) (label scheme-rtl-runtime-type-error))
      (label ,ok-label))))

(define (integer-binary-op-impl op-name)
  `(
    :enter
    ,@(argument-count-check 2)
    ,@(runtime-type-check 'x <integer>)
    ,@(runtime-type-check 'y <integer>)
    (assign (reg operand) x)
    (assign (reg index) (op load-array) (reg operand) (const 0))
    (assign (reg operand) y)
    (assign (reg accum) (op load-array) (reg operand) (const 0))
    (assign (reg index) (op ,op-name) (reg index) (reg accum))
    (save (reg index))
    (push (const 2))
    (perform (op call) (label cp-rtl-malloc))
    (restore (reg index))
    (perform (op store-array) (reg accum) (const 0) (reg index))
    (perform (op store-array) (reg accum) (const 1) ,<integer>)
    :leave))

(define (integer-comparison-op-impl op-name)
  `(
    :enter
    ,@(argument-count-check 2)
    ,@(runtime-type-check 'x <integer>)
    ,@(runtime-type-check 'y <integer>)
    (assign (reg operand) x)
    (assign (reg index) (op load-array) (reg operand) (const 0))
    (assign (reg operand) y)
    (assign (reg accum) (op load-array) (reg operand) (const 0))
    (assign (reg index) (op ,op-name) (reg index) (reg accum))
    (assign (reg operand) (label $scmliteral-boolean))
    (assign (reg accum) (op load-array) (reg operand) (reg index))
    :leave))

(define (char-comparison-op-impl op-name)
  `(
    :enter
    ,@(argument-count-check 2)
    ,@(runtime-type-check 'char1 <char>)
    ,@(runtime-type-check 'char2 <char>)
    (assign (reg operand) char1)
    (assign (reg index) (op load-array) (reg operand) (const 0))
    (assign (reg operand) char2)
    (assign (reg accum) (op load-array) (reg operand) (const 0))
    (assign (reg index) (op ,op-name) (reg index) (reg accum))
    (assign (reg operand) (label $scmliteral-boolean))
    (assign (reg accum) (op load-array) (reg operand) (reg index))
    :leave))

(define (get-pair-car reg)
  `((op load-array) ,reg (const 2)))

(define (get-pair-cdr reg)
  `((op load-array) ,reg (const 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imported symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(emit (extern scheme-rtl-runtime-type-error)
      (extern scheme-rtl-runtime-argument-count-error)
      (extern scheme-rtl-runtime-bounds-error)
      (extern cp-rtl-malloc)
      (extern scheme-rtl-close-file-handle)
      (extern scheme-rtl-get-stdin-handle)
      (extern scheme-rtl-get-stdout-handle)
      (extern scheme-rtl-open-input-file-handle)
      (extern scheme-rtl-open-output-file-handle)
      (extern scheme-rtl-read-byte)
      (extern scheme-rtl-write-byte)
      (extern scheme-rtl-getenv)
      (extern scheme-rtl-strlen)
      (extern scheme-rtl-strcpy)
      (extern scheme-rtl-mkstring)
      (extern scheme-rtl-current-seconds)
      (extern scheme-rtl-current-milliseconds)
      (extern scheme-rtl-stat-file)
      (extern scheme-rtl-delete-file)
      (extern scheme-rtl-rename-file)
      (extern $class_scheme_counter$$$ctor_0)
      (extern $class_scheme_counter)

      (export $scmliteral-unspecified)
      (export $scmliteral-true)
      (export $scmliteral-false)
      (export $scmliteral-boolean)
      (export $scmliteral-nil)
      (export $scmliteral-eof)
      (export $scmliteral-stdin)
      (export $scmliteral-stdout)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions of distinguished RTL objects (nil, true, false, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(emit (global $scmliteral-true
              (const 1)
              ,<bool>)
      (global $scmliteral-false
              (const 0)
              ,<bool>)
      (global $scmliteral-boolean
              (label $scmliteral-false)
              (label $scmliteral-true))
      )

(emit (global $scmliteral-nil
              (const 0)
              ,<nil>
              )
      )

(emit (global $scmliteral-eof
              (const 0)
              ,<eof-object>)
      )

(emit (global $scmliteral-unspecified
              (const 0)
              ,<unspecified>)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (%apply f args)
  ,@(argument-count-check 2)
  (assign (reg index) (const 0))
  ;; operand := f
  (assign (reg operand) (op load-array) (sys stack-pointer) (const 0))
  ,@(runtime-type-check '(reg operand) <proc>)
  ;; pointer := args
  (assign (reg pointer) (op load-array) (sys stack-pointer) (const 1))
  ;; pop args off the stack
  (assign (sys stack-pointer) (op add-pointer) (sys stack-pointer) (const 2))
  ;; this := head-of-list
  (assign (reg this) (reg pointer))
  ;; loop to count arguments
  (label %apply/begin-count)
  (assign (reg accum) (op equal-to) (reg this) (label $scmliteral-nil))
  (branch-nonzero (label %apply/count-done) (reg accum))
  (assign (reg index) (op add) (reg index) (const 1))
  ;; this := cdr(this)
  ,@(runtime-type-check '(reg this) <pair>)
  (assign (reg this) (op load-array) (reg this) (const 3))
  (goto (label %apply/begin-count))
  (label %apply/count-done)
  ;; reserve space on the stack for the arguments.  index holds the number of
  ;; arguments, we need to shift this to multiply by the size of a pointer.
  (assign (reg index) (op bit-lshift) (reg index) (symconst shift-cells-per-word))
  (assign (sys stack-pointer) (op sub) (sys stack-pointer) (reg index))
  (assign (reg index) (op bit-rshift) (reg index) (symconst shift-cells-per-word))
  ;; pop the pointer to the head of the list back into accum, then copy those
  ;; arguments into the stack
  (assign (reg this) (sys stack-pointer))
  (label %apply/begin-copy-args)
  (assign (reg accum) (op equal-to) (reg pointer) (label $scmliteral-nil))
  (branch-nonzero (label %apply/copy-args-done) (reg accum))
  ;; accum := car(pointer)
  (assign (reg accum) (op load-array) (reg pointer) (const 2))
  (perform (op store-array) (reg this) (const 0) (reg accum))
  ;; pointer := cdr(pointer)
  (assign (reg pointer) (op load-array) (reg pointer) (const 3))
  (assign (reg this) (op add-pointer) (reg this) (const 1))
  (goto (label %apply/begin-copy-args))
  (label %apply/copy-args-done)
  ;; args are copied, index is set, stack pointer is correct.
  ;; function pointer is in operand, set the env register and jump to it
  (assign (reg this) (op load-array) (reg operand) (const 2))
  (assign (reg operand) (op load-array) (reg operand) (const 3))
  (goto (reg operand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (char->integer char)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'char <char>)
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (assign (reg operand) char)
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) (const 1))
  :leave)

(define-glue (integer->char int)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'int <integer>)
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (assign (reg operand) int)
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) (const 3))
  :leave)

(define-glue (char<=? char1 char2)
  ,@(char-comparison-op-impl 'less-than-or-equal)
  )

(define-glue (char<? char1 char2)
  ,@(char-comparison-op-impl 'less-than)
  )

(define-glue (char=? char1 char2)
  ,@(char-comparison-op-impl 'equal-to)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equivalence Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (eq? obj-a obj-b)
  (assign (reg accum) (label scmglue-eqv?))
  (goto (reg accum))
  )

(define-glue (eqv? obj-a obj-b)
  :enter
  ,@(argument-count-check 2)
  ;; start out checking to see if the pointers for a and b are equal
  (assign (reg accum) obj-a)
  (assign (reg operand) obj-b)
  (assign (reg index) (op equal-to) (reg accum) (reg operand))
  (branch-zero (label eqv?/compare-types) (reg index))
  ;; they were equal, so return true immediately
  (assign (reg accum) (label $scmliteral-true))
  (goto (label eqv?/done))
  (label eqv?/compare-types)
  ;; the pointers of a and b were not equal, so compare types
  ;; accum := type of object a
  (assign (reg accum) (op load-array) (reg accum) (const 1))
  ;; operand := type of object b
  (assign (reg operand) (op load-array) (reg operand) (const 1))
  ;; compare the type of a to the type of b
  (assign (reg accum) (op equal-to) (reg accum) (reg operand))
  (branch-nonzero (label eqv?/compare-values) (reg accum))
  ;; types were not equal, so return false
  (assign (reg accum) (label $scmliteral-false))
  (goto (label eqv?/done))
  (label eqv?/compare-values)
  ;; types are equal, so compare values.
  ;; We only need to consider characters, numbers and symbols.
  ;; integer?
  (assign (reg accum) (op equal-to) (reg operand) ,<integer>)
  (branch-nonzero (label eqv?/process-integer) (reg accum))
  ;; not an integer.  character?
  (assign (reg accum) (op equal-to) (reg operand) ,<char>)
  (branch-zero (label eqv?/check-symbol) (reg accum))
  (label eqv?/process-integer)
  ;; this is either an integer or a character.  They have similar
  ;; layout and semantics, so we process them with the same code.
  (assign (reg accum) obj-a)
  (assign (reg operand) obj-b)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  (assign (reg accum) (op equal-to) (reg accum) (reg operand))
  ;; we've got accum equal to 0 if they are equal and 1 otherwise.  use this
  ;; value to index into $scmliteral-boolean.
  (assign (reg accum) (op load-array) (label $scmliteral-boolean) (reg accum))
  (goto (label eqv?/done))
  (label eqv?/check-symbol)
  ;; this is neither an integer or a character.  symbol?
  (assign (reg accum) (op equal-to) (reg operand) ,<symbol>)
  (branch-nonzero (label eqv?/process-symbol) (reg accum))
  ;; not a symbol.  Well, these two aren't equivalent.
  (assign (reg accum) (label $scmliteral-false))
  (goto (label eqv?/done))
  (label eqv?/process-symbol)
  ;; this is a symbol.  Let's do a strcmp.  Check the lengths first.
  (assign (reg accum) obj-a)
  (assign (reg operand) obj-b)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  ;; accum is the length of a, operand the length of b.
  (assign (reg index) (op equal-to) (reg accum) (reg operand))
  (branch-nonzero (label eqv?/compare-symbols) (reg index))
  ;; the lengths aren't equal so these aren't equal symbols.
  (assign (reg accum) (label $scmliteral-false))
  (goto (label eqv?/done))
  (label eqv?/compare-symbols)
  (assign (reg accum) obj-a)
  (assign (reg operand) obj-b)
  (assign (reg index) (op load-array) (reg accum) (const 0))
  (assign (reg pointer) (op add-pointer) (reg accum) (const 2))
  (assign (reg operand) (op add-pointer) (reg operand) (const 2))
  (label eqv?/compare-symbols-loop)
  (branch-nonzero (label eqv?/compare-symbols-iter) (reg index))
  ;; we got to the end of the symbol names.  they must be equal.
  (assign (reg accum) (label $scmliteral-true))
  (goto (label eqv?/done))
  (label eqv?/compare-symbols-iter)
  (assign (reg accum) (op load-array) (reg pointer) (const 0))
  (assign (reg this) (op load-array) (reg operand) (const 0))
  (assign (reg accum) (op equal-to) (reg accum) (reg this))
  (branch-nonzero (label eqv?/symbols-next-char) (reg accum))
  ;; the characters weren't equal, so bail out.
  (assign (reg accum) (label $scmliteral-false))
  (goto (label eqv?/done))
  (label eqv?/symbols-next-char)
  ;; the characters were equal, so set up for the next iteration
  (assign (reg pointer) (op add-pointer) (reg pointer) (const 1))
  (assign (reg operand) (op add-pointer) (reg operand) (const 1))
  (assign (reg index) (op sub) (reg index) (const 1))
  (goto (label eqv?/compare-symbols-loop))
  (label eqv?/done)
  :leave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (cons car cdr)
  :enter
  ,@(argument-count-check 2)
  (push (const 4))
  (perform (op call) (label cp-rtl-malloc))
  (perform (op store-array) (reg accum) (const 0) (const 2))
  (perform (op store-array) (reg accum) (const 1) (const ,(+ gc-array-mask 4)))
  (perform (op store-array) (reg accum) (const 2) car)
  (perform (op store-array) (reg accum) (const 3) cdr)
  :leave)

(define-glue (car pair)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'pair <pair>)
  (assign (reg accum) pair)
  (assign (reg accum) ,@(get-pair-car '(reg accum)))
  :leave)

(define-glue (cdr pair)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'pair <pair>)
  (assign (reg accum) pair)
  (assign (reg accum) ,@(get-pair-cdr '(reg accum)))
  :leave)

(define-glue (set-car! pair obj)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'pair <pair>)
  (assign (reg accum) pair)
  (assign (reg operand) obj)
  (perform (op store-array) (reg accum) (const 2) (reg operand))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

(define-glue (set-cdr! pair obj)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'pair <pair>)
  (assign (reg accum) pair)
  (assign (reg operand) obj)
  (perform (op store-array) (reg accum) (const 3) (reg operand))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (%make-string k char)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'k <integer>)
  ,@(runtime-type-check 'char <char>)
  ;; get the string length
  (assign (reg accum) k)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  ;; add 3, for the GCDW, array length and NUL terminator
  (assign (reg accum) (op add) (reg accum) (const 2))
  ;; call malloc
  (push (reg accum))
  (perform (op call) (label cp-rtl-malloc))
  ;; write the header information into the string
  (assign (reg index) k)
  (assign (reg index) (op load-array) (reg index) (const 0))
  ;; length
  (perform (op store-array) (reg accum) (const 0) (reg index))
  ;; string type tag
  (perform (op store-array) (reg accum) (const 1) (const 6))
  (branch-zero (label %make-string/write-done) (reg index))
  ;; write the character into the string buffer
  (assign (reg operand) char)
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  (assign (reg pointer) (op add-pointer) (reg accum) (const 2))
  (label %make-string/write-loop)
  (assign (reg index) (op sub) (reg index) (const 1))
  (perform (op store-array) (reg pointer) (const 0) (reg operand))
  (assign (reg pointer) (op add-pointer) (reg pointer) (const 1))
  (branch-nonzero (label %make-string/write-loop) (reg index))
  (label %make-string/write-done)
  :leave)

(define-glue (string->symbol str)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'str <string>)
  ;; calculate required buffer size: length + 2
  (assign (reg accum) str)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (assign (reg accum) (op add) (reg accum) (const 2))
  ;; allocate buffer
  (push (reg accum))
  (perform (op call) (label cp-rtl-malloc))
  ;; write header information
  (assign (reg pointer) str)
  (assign (reg index) (op load-array) (reg pointer) (const 0))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) (const 7))
  ;; copy buffer
  (assign (reg this) (reg accum))
  (assign (reg this) (op add-pointer) (reg this) (const 2))
  (assign (reg pointer) (op add-pointer) (reg pointer) (const 2))
  (label string->symbol/copy-buffer)
  (assign (reg index) (op sub) (reg index) (const 1))
  (assign (reg operand) (op load-array) (reg pointer) (reg index))
  (perform (op store-array) (reg this) (reg index) (reg operand))
  (branch-nonzero (label string->symbol/copy-buffer) (reg index))
  (label string->symbol/done)
  :leave)

(define-glue (string-length str)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'str <string>)
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (assign (reg operand) str)
  (assign (reg index) (op load-array) (reg operand) (const 0))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) (const 1))
  :leave)

(define-glue (string-ref str idx)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'str <string>)
  ,@(runtime-type-check 'idx <integer>)

  (assign (reg this) str)

  ; bounds check
  (assign (reg operand) (op load-array) (reg this) (const 0)) ; unbox string length
  (assign (reg index) idx) ; unbox integer arg
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg accum) (op less-than) (reg index) (reg operand))
  (branch-nonzero (label string-ref/less-than) (reg accum))

  ; report error
  (push (const 1))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label string-ref/less-than)
  (branch-nonzero (label string-ref/greater-than) (result (op greater-than) (reg index) (const -1)))

  ; report error
  (push (const 2))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label string-ref/greater-than)

  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (assign (reg index) idx)
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg operand) str)
  (assign (reg this) (op add-pointer) (reg operand) (const 2))
  (assign (reg operand) (op load-array) (reg this) (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<char>)
  :leave)

(define-glue (string-set! str idx c)
  :enter
  ,@(argument-count-check 3)
  ,@(runtime-type-check 'str <string>)
  ,@(runtime-type-check 'idx <integer>)
  ,@(runtime-type-check 'c <char>)

  (assign (reg this) str)

  ; bounds check
  (assign (reg operand) (op load-array) (reg this) (const 0)) ; unbox string length
  (assign (reg index) idx) ; unbox integer arg
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg accum) (op less-than) (reg index) (reg operand))
  (branch-nonzero (label string-set!/less-than) (reg accum))

  ; report error
  (push (const 1))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label string-set!/less-than)
  (branch-nonzero (label string-set!/greater-than) (result (op greater-than) (reg index) (const -1)))

  ; report error
  (push (const 2))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label string-set!/greater-than)

  (assign (reg index) idx)
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg operand) c)
  (assign (reg operand) (op load-array) (reg operand) (const 0))
  (assign (reg this) str)
  (assign (reg this) (op add-pointer) (reg this) (const 2))
  (perform (op store-array) (reg this) (reg index) (reg operand))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (symbol->string sym)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'sym <symbol>)
  ;; calculate required buffer size: length + 2
  (assign (reg accum) sym)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (assign (reg accum) (op add) (reg accum) (const 2))
  ;; allocate buffer
  (push (reg accum))
  (perform (op call) (label cp-rtl-malloc))
  ;; write header information
  (assign (reg pointer) sym)
  (assign (reg index) (op load-array) (reg pointer) (const 0))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) (const 6))
  ;; copy buffer
  (assign (reg this) (reg accum))
  (assign (reg this) (op add-pointer) (reg this) (const 2))
  (assign (reg pointer) (op add-pointer) (reg pointer) (const 2))
  (label symbol->string/copy-buffer)
  (assign (reg index) (op sub) (reg index) (const 1))
  (assign (reg operand) (op load-array) (reg pointer) (reg index))
  (perform (op store-array) (reg this) (reg index) (reg operand))
  (branch-nonzero (label symbol->string/copy-buffer) (reg index))
  (label symbol->string/done)
  :leave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (number? obj)
  ,@(predicate-impl <integer>))

(define-glue (char? obj)
  ,@(predicate-impl <char>))

(define-glue (pair? obj)
  ,@(predicate-impl <pair>))

(define-glue (vector? obj)
  ,@(predicate-impl <vector>))

(define-glue (string? obj)
  ,@(predicate-impl <string>))

(define-glue (symbol? obj)
  ,@(predicate-impl <symbol>))

(define-glue (procedure? obj)
  ,@(predicate-impl <proc>))

(define-glue (input-port? obj)
  ,@(predicate-impl <input-port>))

(define-glue (output-port? obj)
  ,@(predicate-impl <output-port>))

(define-glue (eof-object? obj)
  ,@(predicate-impl <eof-object>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (%- x y)
  ,@(integer-binary-op-impl 'sub))

(define-glue (%* x y)
  ,@(integer-binary-op-impl 'mul))

(define-glue (%+ x y)
  ,@(integer-binary-op-impl 'add))

(define-glue (%= x y)
  ,@(integer-comparison-op-impl 'equal-to))

(define-glue (%< x y)
  ,@(integer-comparison-op-impl 'less-than))

(define-glue (%<= x y)
  ,@(integer-comparison-op-impl 'less-than-or-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-glue (%make-vector k value)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'k <integer>)
  (assign (reg index) k)
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg operand) value)
  (assign (reg accum) (op add) (reg index) (const 2))
  (save (reg index))
  (save (reg operand))
  (push (reg accum))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (restore (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) ,<vector>)
  (save (reg accum))
  (assign (reg accum) (op add-pointer) (reg accum) (const 2))
  (label %make-vector/begin-loop)
  (branch-zero (label %make-vector/end-loop) (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (assign (reg accum) (op add-pointer) (reg accum) (const 1))
  (assign (reg index) (op sub) (reg index) (const 1))
  (goto (label %make-vector/begin-loop))
  (label %make-vector/end-loop)
  (restore (reg accum))
  :leave)


(define-glue (vector-length vec)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'vec <vector>)
  (assign (reg accum) vec)
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (save (reg operand))
  (assign (reg accum) (const 2))
  (push (reg accum))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  :leave)


(define-glue (vector-ref vec k)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'vec <vector>)
  ,@(runtime-type-check 'k <integer>)
  (assign (reg this) vec)
  ; bounds check
  (assign (reg operand) (op load-array) (reg this) (const 0)) ; unbox vector length
  (assign (reg index) k) ; unbox integer arg
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg accum) (op less-than) (reg index) (reg operand))
  (branch-nonzero (label vector-ref/less-than) (reg accum))

  ; report error
  (push (const 1))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label vector-ref/less-than)
  (branch-nonzero (label vector-ref/greater-than) (result (op greater-than) (reg index) (const -1)))

  ; report error
  (push (const 2))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label vector-ref/greater-than)
  ; move past vector header
  (assign (reg accum) (op add-pointer) (reg this) (const 2))
  ; reference vector
  (assign (reg accum) (op load-array) (reg accum) (reg index))
  :leave)


(define-glue (vector-set! vec k obj)
  :enter
  ,@(argument-count-check 3)
  ,@(runtime-type-check 'vec <vector>)
  ,@(runtime-type-check 'k <integer>)

  (assign (reg this) vec)
  ; bounds check
  (assign (reg operand) (op load-array) (reg this) (const 0)) ; unbox vector length
  (assign (reg index) k) ; unbox integer arg
  (assign (reg index) (op load-array) (reg index) (const 0))
  (assign (reg accum) (op less-than) (reg index) (reg operand))
  (branch-nonzero (label vector-set!/less-than) (reg accum))

  ; report error
  (push (const 3))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label vector-set!/less-than)
  (branch-nonzero (label vector-set!/greater-than) (result (op greater-than) (reg index) (const -1)))

  ; report error
  (push (const 4))
  (push (reg operand))
  (push (reg index))
  (perform (op call) (label scheme-rtl-runtime-bounds-error))
  (perform (op break))

  (label vector-set!/greater-than)
  ; move past vector header
  (assign (reg accum) (op add-pointer) (reg this) (const 2))

  ; set vector element
  (assign (reg operand) obj)
  (perform (op store-array) (reg accum) (reg index) (reg operand))
  (assign (reg accum) (label $scmliteral-unspecified))

  :leave)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-glue (close-input-port port)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'port <input-port>)
  (assign (reg accum) port)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-close-file-handle))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

(define-glue (close-output-port port)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'port <output-port>)
  (assign (reg accum) port)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-close-file-handle))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

(emit (global $scmliteral-stdin
              (const 0)
              ,<input-port>))

(emit (global $scmliteral-stdout
              (const 0)
              ,<output-port>))

(define-glue (current-input-port)
  :enter
  ,@(argument-count-check 0)
  (assign (reg accum) (label $scmliteral-stdin))
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (assign (reg operand) (op equal-to) (reg operand) (const 0))
  (branch-zero (label current-input-port/done) (reg operand))
  (perform (op call) (label scheme-rtl-get-stdin-handle))
  (assign (reg operand) (label $scmliteral-stdin))
  (perform (op store-array) (reg operand) (const 0) (reg accum))
  (assign (reg accum) (reg operand))
  (label current-input-port/done)
  :leave)

(define-glue (current-output-port)
  :enter
  ,@(argument-count-check 0)
  (assign (reg accum) (label $scmliteral-stdout))
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (assign (reg operand) (op equal-to) (reg operand) (const 0))
  (branch-zero (label current-output-port/done) (reg operand))
  (perform (op call) (label scheme-rtl-get-stdout-handle))
  (assign (reg operand) (label $scmliteral-stdout))
  (perform (op store-array) (reg operand) (const 0) (reg accum))
  (assign (reg accum) (reg operand))
  (label current-output-port/done)
  :leave)

(define-glue (open-input-file fname)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'fname <string>)
  (assign (reg operand) fname)
  ;; arg 1 - string length
  (assign (reg accum) (op load-array) (reg operand) (const 0))
  (push (reg accum))
  ;; arg 0 - string pointer
  (assign (reg accum) (op add-pointer) (reg operand) (const 2))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-open-input-file-handle))

  (assign (reg operand) (reg accum))
  (save (reg operand))
  (push (const 4))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<input-port>)
  (assign (reg operand) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 2) (reg operand))
  :leave)


(define-glue (open-output-file fname)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'fname <string>)
  (assign (reg operand) fname)
  ;; arg 1 - string length
  (assign (reg accum) (op load-array) (reg operand) (const 0))
  (push (reg accum))
  ;; arg 0 - string pointer
  (assign (reg accum) (op add-pointer) (reg operand) (const 2))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-open-output-file-handle))

  (assign (reg operand) (reg accum))
  (save (reg operand))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<output-port>)
  :leave)

(define-glue (%peek-char port)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'port <input-port>)
  (assign (reg operand) port)
  (assign (reg accum) (op load-array) (reg operand) (const 2))
  (assign (reg index) (op equal-to) (reg accum) (label $scmliteral-false))
  (assign (reg accum) (op load-array) (reg operand) (const 3))
  (branch-nonzero (label %peek-char-proceed) (reg index))
  (goto (label %peek-char-read))
  (label %peek-char-proceed)
  (assign (reg accum) (op load-array) (reg operand) (const 0))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-read-byte))
  (assign (reg operand) port)
  (perform (op store-array) (reg operand) (const 2) (label $scmliteral-true))
  (perform (Op store-array) (reg operand) (const 3) (reg accum))
  (label %peek-char-read)
  (assign (reg operand) (op less-than) (reg accum) (const 0))
  (branch-zero (label %peek-char-regular-char) (reg operand))
  (assign (reg accum) (label $scmliteral-eof))
  (goto (label %peek-char-done))
  (label %peek-char-regular-char)
  (assign (reg operand) (reg accum))
  (save (reg operand))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<char>)
  (label %peek-char-done)
  :leave)

(define-glue (%read-char port)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'port <input-port>)
  (assign (reg operand) port)
  (assign (reg accum) (op load-array) (reg operand) (const 2))
  (assign (reg index) (op equal-to) (reg accum) (label $scmliteral-false))
  (assign (reg accum) (op load-array) (reg operand) (const 3))
  (branch-nonzero (label %read-char-proceed) (reg index))

  (assign (reg index) (label $scmliteral-false))
  (perform (op store-array) (reg operand) (const 2) (reg index))
  (assign (reg accum) (op load-array) (reg operand) (const 3))
  (goto (label %read-char-byte-already-read))

  (label %read-char-proceed)
  (assign (reg accum) (op load-array) (reg operand) (const 0))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-read-byte))

  (label %read-char-byte-already-read)
  (assign (reg operand) (op less-than) (reg accum) (const 0))
  (branch-zero (label %read-char-regular-char) (reg operand))

  (assign (reg accum) (label $scmliteral-eof))
  (goto (label %read-char-done))

  (label %read-char-regular-char)
  (assign (reg operand) (reg accum))
  (save (reg operand))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg operand))
  (perform (op store-array) (reg accum) (const 0) (reg operand))
  (perform (op store-array) (reg accum) (const 1) ,<char>)

  (label %read-char-done)
  :leave)

(define-glue (%write-char char port)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'char <char>)
  ,@(runtime-type-check 'port <output-port>)
  (assign (reg accum) port)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (push (reg accum))
  (assign (reg accum) char)
  (assign (reg accum) (op load-array) (reg accum) (const 0))
  (push (reg accum))
  (perform (op call) (label scheme-rtl-write-byte))
  (assign (reg accum) (label $scmliteral-unspecified))
  :leave)

(define-glue (getenv var)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'var <string>)

  ;; push arg1 = length
  (assign (reg accum) var)
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (push (reg operand))
  ;; push arg0 = string pointer
  (assign (reg operand) (op add-pointer) (reg accum) (const 2))
  (push (reg operand))
  ;; call
  (perform (op call) (label scheme-rtl-getenv))

  (branch-nonzero (label getenv/false) (reg accum))
  (assign (reg accum) (label $scmliteral-false))
  (goto (label getenv/done))

  (label getenv/false)
  (push (reg accum))
  (perform (op call) (label scheme-rtl-mkstring))

  (label getenv/done)
  :leave)

(define-glue (stat path)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'path <string>)

  ;;
  ;; allocate a vector of 10 elements
  ;;
  (push (const 12))
  (perform (op call) (label cp-rtl-malloc))
  (perform (op store-array) (reg accum) (const 0) (const 10))
  (perform (op store-array) (reg accum) (const 1) ,<vector>)
  (perform (op store-array) (reg accum) (const 2) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 3) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 4) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 5) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 6) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 7) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 8) (label $scmliteral-false))
  ;(perform (op store-array) (reg accum) (const 9) (label $scmliteral-false))
  (perform (op store-array) (reg accum) (const 10) (label $scmliteral-false))
  ;(perform (op store-array) (reg accum) (const 11) (label $scmliteral-false))

  (save (reg accum))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (perform (op store-array) (reg accum) (const 0) (const 0))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  (restore (reg operand))
  (perform (op store-array) (reg operand) (const 9) (reg accum))

  (save (reg operand))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (perform (op store-array) (reg accum) (const 0) (const 0))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  (restore (reg operand))
  (perform (op store-array) (reg operand) (const 11) (reg accum))

  (assign (reg accum) (reg operand))
  (save (reg accum))

  ;; push arg3 = vector length
  (push (const 10))
  ;; push arg2 = vector buffer pointer
  (assign (reg accum) (op add-pointer) (reg accum) (const 2))
  (push (reg accum))
  ;; push arg1 = string length
  (assign (reg accum) path)
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (push (reg operand))
  ;; push arg0 = string buffer pointer
  (assign (reg operand) (op add-pointer) (reg accum) (const 2))
  (push (reg operand))
  ;; call
  (perform (op call) (label scheme-rtl-stat-file))

  (branch-nonzero (label stat/failed) (reg accum))
  (restore (reg accum))
  (goto (label stat/done))

  (label stat/failed)
  (assign (reg accum) (label $scmliteral-false))

  (label stat/done)
  :leave)

(define-glue (current-seconds)
  :enter
  (perform (op call) (label scheme-rtl-current-seconds))
  (save (reg accum))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  :leave)

(define-glue (current-milliseconds)
  :enter
  (perform (op call) (label scheme-rtl-current-milliseconds))
  (save (reg accum))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  :leave)

(define-glue (delete-file path)
  :enter
  ,@(argument-count-check 1)
  ,@(runtime-type-check 'path <string>)

  (assign (reg accum) path)
  ;; push arg1 = length
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (push (reg operand))
  ;; push arg0 = buffer pointer
  (assign (reg accum) (op add-pointer) (reg accum) (const 2))
  (push (reg accum))
  ;; call
  (perform (op call) (label scheme-rtl-delete-file))

  ;; assign correct result object
  (branch-nonzero (label delete-file/failed) (reg accum))
  (assign (reg accum) (label $scmliteral-true))
  (goto (label delete-file/done))

  (label delete-file/failed)
  (assign (reg accum) (label $scmliteral-false))

  (label delete-file/done)
  :leave)

(define-glue (rename-file from to)
  :enter
  ,@(argument-count-check 2)
  ,@(runtime-type-check 'from <string>)
  ,@(runtime-type-check 'to <string>)

  (assign (reg accum) to)
  ;; push arg3 = length
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (push (reg operand))
  ;; push arg2 = buffer pointer
  (assign (reg accum) (op add-pointer) (reg accum) (const 2))
  (push (reg accum))

  (assign (reg accum) from)
  ;; push arg1 = length
  (assign (reg operand) (op load-array) (reg accum) (const 0))
  (push (reg operand))
  ;; push arg0 = buffer pointer
  (assign (reg accum) (op add-pointer) (reg accum) (const 2))
  (push (reg accum))

  ;; call
  (perform (op call) (label scheme-rtl-rename-file))

  ;; assign correct result object
  (branch-nonzero (label rename-file/failed) (reg accum))
  (assign (reg accum) (label $scmliteral-true))
  (goto (label rename-file/done))

  (label rename-file/failed)
  (assign (reg accum) (label $scmliteral-false))

  (label rename-file/done)
  :leave)

(define-glue (new-counter)
  :enter
  (push (const 3))
  (perform (op call) (const cp-rtl-malloc))
  (perform (op store-array) (reg accum) (const 0) (label $class_scheme_counter))
  (assign (reg this) (reg accum))
  (perform (op this-call) (label $class_scheme_counter$$$ctor_0))
  :leave)

(define (java-library-runtime-type-check obj java-class)
  (if (not (symbol? java-class))
      (error "java-library-runtime-type-check: java-class must be specified as a symbol"))
  (let* ((token (make-label-token))
         (ok-label (make-label 'java-rttc-ok token))
         (err-label (make-label 'java-rttc-fail token))
         )
    ;; we are only allowed to stomp on (reg accum) in this code.
    `((assign (reg accum) ,obj)
      (assign (reg accum) (op equal-to) (reg accum) (const 0))
      (branch-nonzero (label ,err-label) (reg accum))
      (assign (reg accum) ,obj)
      (assign (reg accum) (op load-array) (reg accum) (const 1))
      (assign (reg accum) (op equal-to) (reg accum) (const 0))
      (branch-zero (label ,err-label) (reg accum))
      (assign (reg accum) ,obj)
      (assign (reg accum) (op load-array) (reg accum) (const 0))
      (assign (reg accum) (op equal-to) (reg accum) (label ,java-class))
      (branch-nonzero (label ,ok-label) (reg accum))
      (label ,err-label)
      ;; set up a call to the "ZOMG" function.  Notice that this
      ;; isn't saving registers because it is not expected to
      ;; return.
      (assign (reg accum) ,obj)
      (push (reg accum))
      (assign (reg accum) (label ,java-class))
      (push (reg accum))
      (perform (op call) (label scheme-rtl-runtime-type-error))
      (label ,ok-label))))

(define-glue (inc-counter counter)
  :enter
  ,@(argument-count-check 1)
  ,@(java-library-runtime-type-check 'counter '$class_scheme_counter)
  (assign (reg this) counter)
  (assign (reg accum) (op load-array) (reg this) (const 0))
  (assign (reg accum) (op load-array) (reg accum) (const 45))
  (perform (op this-call) (reg accum))
  (assign (reg index) (reg accum))
  (save (reg index))
  (push (const 2))
  (perform (op call) (label cp-rtl-malloc))
  (restore (reg index))
  (perform (op store-array) (reg accum) (const 0) (reg index))
  (perform (op store-array) (reg accum) (const 1) ,<integer>)
  :leave)
