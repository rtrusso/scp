;; sasm-interp.scm
;;
;; This module implements the sasm interpreter, interfacing to the
;; sasm machine simulator and sasm language evaluator.

(need util/list)
(need util/string)
(need sasm/sasm-machine)
(need sasm/sasm-eval)
(need sasm/sasm-interp-spec)
(need sasm/sasm-tx)
(need sasm/tx/interp)
(need sasm/sasm-ast)
(need sasm/sasm-visitor)
(need sasm/sasm-analyze)
(need sasm/sasm-parse)
(need sasm/sasm-rewrite)
(need util/filesystem)

(define *sasm-interp-output-port* #f)
(define (sasm-interp-output-port)
  (if *sasm-interp-output-port*
      *sasm-interp-output-port*
      (current-output-port)))

(define *sasm-interp-input-port* #f)
(define (sasm-interp-input-port)
  (if *sasm-interp-input-port*
      *sasm-interp-input-port*
      (current-input-port)))

(define *sasm-interp-print-timings* #f)

(define (sasm-interp-make-machine)
  (let ((machine (make-machine2)))
    (let ((stack (sasm-interp-spec-sys-register 'stack-pointer))
          (frame (sasm-interp-spec-sys-register 'frame-pointer)))
      (if (and stack frame)
          (machine2-set-sys-registers! machine stack frame)))
    machine))

(define *machine* (sasm-interp-make-machine))
(define *memory* '())
(define *controller* '())
(define *main-entry* #f)
(define *debug-mode* #f)

(define (sasm-interp-module-context file-name)
  (let ((controller '())
	(memory '()))
    (define (get-file)
      file-name)
    (define (get-memory)
      memory)
    (define (append-memory x)
      (set! memory (cons x memory)))
    (define (get-controller)
      controller)
    (define (append-controller x)
      (set! controller (append controller (list x))))
    (lambda (x . args)
      (apply
       (case x
	 ((get-memory) get-memory)
	 ((append-memory) append-memory)
	 ((get-controller) get-controller)
	 ((append-controller) append-controller)
	 ((get-file) get-file)
	 (else #f))
       args))))

(define (sasm-interp-symbolic-pointer? obj)
  (or (symbol? obj)
      (and (pair? obj) (symbol? (car obj)) (integer? (cdr obj)))))

(define (sasm-interp-symbolic-pointer-key obj)
  (if (symbol? obj)
      obj
      (car obj)))

(define (sasm-interp-symbolic-pointer-offset obj)
  (if (symbol? obj)
      0
      (cdr obj)))

(define (sasm-interp-add-pointer x y)
  (cond ((or (symbol? y) (pair? y))
         (add-pointer y x))
        ((symbol? x)
         (if (not (integer? y))
             (error "add-pointer: unable to add symbolic pointers" x y))
         (cons x y))
        ((and (pair? x) (symbol? (car x)) (integer? (cdr x)))
         (if (not (integer? y))
             (error "add-pointer: unable to add symbolic pointers" x y))
         (cons (car x) (+ (cdr x) y)))
        (else
         (+ x y))))

(define (memory-add addr value)
  (set! *memory* (cons (cons addr value)
                       *memory*)))

(define (memory-ref-area-size addr offset)
  (if (integer? addr)
      1
      (let ((entry (assoc (sasm-interp-symbolic-pointer-key addr)
                          *memory*)))
        (cond ((not entry)
               (error ";; Invalid memory address in memory-ref-area-size "
                      addr))
              ((vector? (cdr entry))
               (let ((outer-length (vector-length (cdr entry))))
                 (- outer-length offset)))
              ((and (pair? (cdr entry))
                    (integer? (car (cdr entry)))
                    (vector? (cdr (cdr entry))))
               (let ((the-vector (cdr (cdr entry)))
                     (the-offset (+ (car (cdr entry))
                                    (sasm-interp-symbolic-pointer-offset addr)
                                    offset)))
                 (- (vector-length the-vector) the-offset)))
              (else
               (error "Invalid memory entry " addr entry))))))

(define (memory-ref addr offset)
  (if (integer? addr)
      (begin (if (zero? addr)
                 (error "Dereference 0! " addr))
             (machine2-memory-ref *machine* (+ addr offset)))
      (let ((entry (assoc (sasm-interp-symbolic-pointer-key addr)
                          *memory*)))
        (cond ((not entry)
               (begin (display ";; Invalid memory address: ")
                      (display addr)
                      (newline)
                      (machine-dump-state *machine*)
                      (display ";; Contents of memory: ")
                      (newline)
                      (for-each (lambda (entry)
                                  (display ";;    ")
                                  (display entry)
                                  (newline))
                                *memory*)
                      (error "Invalid memory address: " addr)))
              ((vector? (cdr entry))
               (let ((the-vector (cdr entry))
                     (vector-offset (+ (sasm-interp-symbolic-pointer-offset addr)
                                       offset)))
                 (if (and (>= vector-offset 0) (< vector-offset (vector-length the-vector)))
                     (vector-ref (cdr entry) vector-offset)
                     (begin (sasm-machine-dump)
                            (error "Invalid vector offset in memory-ref [vector-direct]"
                                   addr
                                   offset
                                   vector-offset
                                   the-vector)))))
              ((and (pair? (cdr entry))
                    (integer? (car (cdr entry)))
                    (vector? (cdr (cdr entry))))
               (let ((the-vector (cdr (cdr entry)))
                     (the-offset (+ (car (cdr entry))
                                    (sasm-interp-symbolic-pointer-offset addr)
                                    offset)))
                 (if (and (>= the-offset 0) (< the-offset (vector-length the-vector)))
                     (vector-ref the-vector the-offset)
                     (begin (sasm-machine-dump)
                            (error "Invalid vector offset in memory-ref [vector-indirect]"
                                   addr
                                   offset
                                   (car (cdr entry))
                                   (sasm-interp-symbolic-pointer-offset addr)
                                   the-offset
                                   the-vector)))))
              (else
               (error "Invalid memory entry " entry))))))

(define (memory-set! addr offset value)
  (if (integer? addr)
      (begin (if (zero? addr)
                 (error "write to 0! " addr))
             (machine2-memory-set! *machine* (+ addr offset) value))
      (let ((entry (assoc (sasm-interp-symbolic-pointer-key addr)
                          *memory*)))
        (if (not entry)
            (error "Invalid memory address: " addr)
            (vector-set! (cdr entry)
                         (+ (sasm-interp-symbolic-pointer-offset addr)
                            offset)
                         value)))))

(define (controller-add code)
  (set! *controller* (append *controller* code)))

(define (malloc size)
  (make-vector size))

(define (store-array array index value)
  (memory-set! array index value))

(define (load-array array index)
  (memory-ref array index))

(define (destroy-registers machine)
  (for-each (lambda (reg)
              (machine-write-reg machine reg #f))
            *sasm-caller-destroy-registers*))

(define (sasm-interp-next-pc current-pc)
  (if (null? current-pc)
      current-pc
      (cdr current-pc)))

(define (call symbol)
  (define (@r name)
    (sasm-interp-spec-register name))
  (let ((current-pc (machine-read-reg *machine* 'pc))
        (ctx (machine2-get-current-call-context *machine*)))
    (define (read-string base-pointer length)
      (let ((str (make-string length)))
        (let loop ((index 0))
          (if (>= index length)
              str
              (begin (string-set! str index (integer->char (memory-ref base-pointer
                                                                       index)))
                     (loop (+ index 1)))))))
    (define (begin-native)
      (machine-push-arg *machine* (cons '$return-address ctx))
      (sasm-machine-set-call-context ctx)
      (machine-push-frame *machine*)
      (machine-push-locals *machine* 0))
    (define (end-native)
      (machine-pop-frame *machine*)
      (machine-pop-arg *machine*)
      (sasm-machine-set-call-context ctx)
      (machine-write-reg *machine* 'pc (sasm-interp-next-pc current-pc)))
    (define (call-proc-by-name symbol)
      (let ((pc (member (list 'label symbol) *controller*)))
        (if (not pc)
            (begin 
              (machine-dump-state *machine*)
              (error "Attempt to call non-existent function " symbol)))
        (machine-write-reg *machine* 'pc pc)
        (machine-push-arg *machine* (list '$return-address ctx (sasm-interp-next-pc current-pc)))
        (sasm-machine-set-call-context symbol)))
    (case symbol
      ((mj-malloc cp-mj-malloc)
       (begin-native)
       (destroy-registers *machine*)
       (machine-write-reg *machine* (@r 'accum) (make-vector (machine-get-arg *machine* 0)))
       (end-native))
      ((mj-system-out-println cp-mj-system-out-println) 
       (begin-native)
       (if (integer? (machine-get-arg *machine* 0))
           (display (bit-twoscomp (machine-get-arg *machine* 0))
                    (sasm-interp-output-port))
           (write (machine-get-arg *machine* 0)
                  (sasm-interp-output-port)))
       (newline (sasm-interp-output-port))
       (destroy-registers *machine*)
       (machine-write-reg *machine* (@r 'accum) (machine-get-arg *machine* 0))
       (end-native))
      ((mj-array-length cp-mj-array-length)
       (begin-native)
       (destroy-registers *machine*)
       (machine-write-reg *machine*
                          (@r 'accum)
                          (vector-length (machine-get-arg *machine* 0)))
       (end-native))
      ((scheme-rtl-get-stdout-handle)
       (begin-native)
       (machine-write-reg *machine* (@r 'accum) (sasm-interp-output-port))
       (end-native))
      ((scheme-rtl-get-stdin-handle)
       (begin-native)
       (machine-write-reg *machine* (@r 'accum) (sasm-interp-input-port))
       (end-native))
      ((scheme-rtl-open-input-file-handle)
       (begin-native)
       (machine-write-reg *machine*
                          (@r 'accum)
                          (open-input-file (read-string (machine-get-arg *machine* 0)
                                                        (machine-get-arg *machine* 1))))
       (end-native))
      ((scheme-rtl-close-file-handle)
       (begin-native)
       (let ((stream (machine-get-arg *machine* 0)))
         (cond ((input-port? stream) (close-input-port stream))
               ((output-port? stream) (close-output-port stream))
               (error "Not a scheme stream")))
       (end-native))
      ((scheme-rtl-open-output-file-handle)
       (begin-native)
       (machine-write-reg *machine*
                          (@r 'accum)
                          (open-output-file (read-string (machine-get-arg *machine* 0)
                                                         (machine-get-arg *machine* 1))))
       (end-native))
      ((scheme-rtl-read-byte)
       (begin-native)
       (let ((ch (read-char (machine-get-arg *machine* 0))))
         (let ((byte (if (eof-object? ch)
                         -1
                         (char->integer ch))))
           (machine-write-reg *machine* (@r 'accum) byte)))
       (end-native))
      ((scheme-rtl-write-byte)
       (begin-native)
       (write-char (integer->char (machine-get-arg *machine* 0))
                   (machine-get-arg *machine* 1))
       (end-native))
      ((scheme-rtl-runtime-type-error)
       (println "SASM Runtime Type Error")
       (begin-native)
       (let* ((arg0 (machine-get-arg *machine* 0))
              (arg1 (machine-get-arg *machine* 1)))
         (println "    " arg0)
         (println "    " arg1)
         (end-native))
       (sasm-machine-dump)
       (sasm-debugger-repl #f))
      ((scheme-rtl-runtime-argument-count-error)
       (println "SASM Argument Count Error")
       (begin-native)
       (let* ((arg0 (machine-get-arg *machine* 0))
              (arg1 (machine-get-arg *machine* 1)))
         (println "    " arg0)
         (println "    " arg1)
         (end-native))
       (sasm-machine-dump)
       (sasm-debugger-repl #f))
      (else
       (call-proc-by-name symbol)))
    (case symbol
      ((cp-mj-malloc cp-mj-system-out-println cp-mj-array-length
                     cp-rtl-read-byte cp-rtl-open-input-file-handle
                     cp-rtl-open-output-file-handle)
       (machine-pop-arg *machine*))
      ((cp-rtl-write-byte)
       (machine-pop-arg *machine*)
       (machine-pop-arg *machine*))
      )
    ))

(define (tail-call symbol n-next-args n-cur-args)
  (machine-pop-frame-tail-call *machine* n-next-args n-cur-args)
  (let ((pc (member (list 'label symbol) *controller*)))
    (if (not pc)
        (error "Attempt to tail-call non-existent function " symbol))
    (machine-write-reg *machine* 'pc pc)
    ))

(define (sasm-init)
  (set! *machine* (sasm-interp-make-machine))
  (machine-write-reg *machine* 'pc #f)
  (set! *memory* '())
  (set! *controller* '())
  (set! *main-entry* 'sasm-main-entry)
  (sasm-interp-symconst-machine-params)
  (machine-write-reg *machine* 'step-count 0))

(define-syntax sasm-dispatch
  (syntax-rules (:parent)
    ((_ :parent <parent> <label> ...)
     (let ((parent <parent>))
       (lambda args
         (case (car args)
           ((<label>) (apply <label> (cdr args))) ...
           (else (apply parent args))))))
    ((_ <label> ...)
     (lambda args
       (apply (case (car args)
                ((<label>) <label>) ...
                (else
                 (error (car args))))
              (cdr args))))
    )
  )

(define-syntax sasm@
  (syntax-rules ()
    ((_ <object> <symbol> <arg> ...)
     (<object> '<symbol> <arg> ...))))

;(define (make-a)
;  (define (add-1 x) (+ x 1))
;  (define (mul x y) (* x y))
;  (sasm-dispatch add-1 mul))

;(define (make-b)
;  (sasm-dispatch :parent (make-a) sqrt))

;(define a (make-a))
;(define b (make-b))

(define (sasm-make-symbol-table)
  (let ((symbol-table '()))
    (define (:enter x y)
      (set! symbol-table (cons (cons x y) symbol-table)))
    (define (:lookup x)
      (let ((entry (assoc x symbol-table)))
        (and entry
             (cdr entry))))
    (sasm-dispatch :enter :lookup)))

(define (sasm-symbol-table-lookup symtab global)
  (sasm@ symtab :lookup global))

(define (sasm-symbol-table-enter! symtab global value)
  (sasm@ symtab :enter global value))

(define (sasm-top-level-context)
  (let ((global-variables (sasm-make-symbol-table))
        (module-index 0))
    (define (:define-global! sym value)
      (sasm-symbol-table-enter! global-variables
                                sym
                                value))
    (define (:lookup-global sym)
      (sasm-symbol-table-lookup global-variables sym))
    (define (:next-module-name)
      (let ((result  (string-append "module" (number->string module-index))))
        (set! module-index (+ 1 module-index))
        result))
    (sasm-dispatch :define-global!
                   :lookup-global
                   :next-module-name)))

(define (sasm-file-context file parent-context)
  (define (:include-path)
    (path-base-dir file))
  (define (:module-name)
    file)
  (sasm-dispatch :parent parent-context
                 :module-name
                 :include-path))

(define (sasm-interp-digest-interactive-context)
  (define (:include-path)
    ".")
  (define (:module-name)
    "interactive")
  (sasm-dispatch :parent (sasm-top-level-context)
                 :module-name
                 :include-path))

(define (sasm-interp-digest-file-context fname)
  (sasm-file-context fname (sasm-top-level-context)))

(define (sasm-interp-digest-include-path context)
  (sasm@ context :include-path))

(define (sasm-interp-digest context code)
  (case (car code)
    ((class) 
     (memory-add (cadr code)
                 (list->vector (map cadr (cddr code)))))
    ((global)
     (memory-add (cadr code)
                 (cond ((null? (cddr code))
                        (vector 0))
                       ((and (= 1 (length (cddr code)))
                             (eqv? 'const (caaddr code))
                             (string? (cadr (caddr code))))
                        (let ((str (cadr (caddr code))))
                          (cons 2
                                (list->vector (append
                                               (list (string-length str)
                                                     0)
                                               (map char->integer
                                                    (string->list str)))))))
                       (else
                        (list->vector (map cadr (cddr code)))))))
    ((function)
     (controller-add (append `((label ,(cadr (assoc 'name (cdr code)))))
                             (cdr (assoc 'body (cdr code))))))
    ((entry)
     (set! *main-entry* (cadr code)))
    ((define-symconst)
     (sasm-symconst-alist-append (cdr code)))
    ((extern export class-info)
     'nothing)
    ((include)
     (let ((fname (build-path (sasm-interp-digest-include-path context)
                              (cadr code))))
       (sasm-load-file (sasm-interp-digest-file-context fname) fname)))
    (else
     (error "Bad SASM code " code))))

(define (make-timer)
  (if *sasm-interp-print-timings*
      (cons (get-internal-real-time) '())))

(define (reset-timer! timer)
  (if *sasm-interp-print-timings*
      (set-car! timer (get-internal-real-time))))

(define (print-timer-delta! timer)
  (if *sasm-interp-print-timings*
      (let* ((c (get-internal-real-time))
             (delta (- c (car timer))))
        (display delta)
        (newline)
        (set-car! timer c))))

(define the-timer (make-timer))

(define (sasm-load-program context raw-program)
  (define (next-timer! msg)
    (if *sasm-interp-print-timings*
        (begin (display ";;;      ")
               (display msg)
               (display " ")
               (print-timer-delta! the-timer))))
  (reset-timer! the-timer)
  (let* ((raw-ast (sasm-parse-program raw-program))
         (@@@not-a-var (next-timer! "parse"))
         (analyze-result (and raw-ast
                              (sasm-program-analyze-symbols!
                               (sasm@ context :module-name)
                               raw-ast))))
    (next-timer! "analyze")
    (cond
     ((not raw-ast)
      (error "SASM parse error in program"))
     ((not analyze-result)
      (error "One or more errors detected in SASM code; terminating"))
     (else
      (let* ((program (sasm-rewrite-ast raw-ast))
             (@@@not-a-var2 (next-timer! "rewrite"))
             (ast (sasm-parse-program program)))
        (next-timer! "reparse")
        (cond
         ((not ast)
          (error "Internal error: failed to parse rewritten code"))
         (else
          ;; Run a couple of analysis routines just to verify they don't
          ;; signal an error.
          (sasm-program-extern-symbols raw-ast)
          (next-timer! "extern-raw")
          (sasm-program-extern-symbols ast)
          (next-timer! "extern")
          (sasm-program-defined-symbols raw-ast)
          (next-timer! "defined-raw")
          (sasm-program-defined-symbols ast)
          (next-timer! "defined")
          (sasm-program-referenced-symbols raw-ast)
          (next-timer! "referenced-raw")
          (sasm-program-referenced-symbols ast)
          (next-timer! "referenced")
          (for-each (lambda (statement)
                      (sasm-interp-digest context statement))
                    program)
          (next-timer! "digest")
          program)))))))

(define (sasm-load-port context port)
  (if *sasm-interp-print-timings*
      (begin (display ";;;    sasm-load-port ")
             (reset-timer! the-timer)))
  (let loop ((current-statement (read port))
             (reverse-program '()))
    (if (eof-object? current-statement)
        (begin (print-timer-delta! the-timer)
               (sasm-load-program context (reverse reverse-program)))
        (loop (read port)
              (cons current-statement reverse-program)))))

(define (sasm-load-file context filename)
  (if *sasm-interp-print-timings*
      (begin (display ";;; sasm-load-file ")
             (display filename)
             (newline)))
  (call-with-input-file filename
    (lambda (port)
      (sasm-load-port context port))))

(define (sasm-execute)
  (machine-define-symconst! *machine* *sasm-assemble-symconst-alist*)
  (machine-write-reg *machine* 'pc '())
  ;; Call "main"
  (call *main-entry*)
  (eval-machine-iter *controller* *machine*))

;; debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-machine-set-call-context symbol)
  (let ((value (sasm-machine-get-call-context symbol)))
    (machine2-set-current-call-context *machine* value)))

(define (sasm-machine-get-call-context symbol)
  (let ((step-count (machine-read-reg *machine* 'step-count)))
    (if (zero? step-count)
        symbol
        (string->symbol (string-append (symbol->string symbol)
                                       "@"
                                       (number->string step-count))))))

(define (sasm-machine-step)
  (let ((count (machine-read-reg *machine* 'step-count)))
    (eval-machine-step *controller* *machine*)
    (machine-write-reg *machine* 'step-count (+ count 1))))

(define (sasm-machine-step-noisy)
  (display "Executing: ")
  (write (car (machine-read-reg *machine* 'pc)))
  (newline)
  (eval-machine-step *controller* *machine*)
  (machine-dump-state *machine*))

(define (sasm-machine-step-into symbol)
  (let ((current-pc (machine-read-reg *machine* 'pc))
        (ctx (machine2-get-current-call-context *machine*)))
    (define (call-proc-by-name symbol)
      (let ((pc (member (list 'label symbol) *controller*)))
        (if (not pc)
            (begin 
              (machine-dump-state *machine*)
              (error "Attempt to step into non-existent function " symbol)))
        (machine-write-reg *machine* 'pc pc)
        (machine-push-arg *machine* (list '$return-address ctx (sasm-interp-next-pc current-pc)))
        (sasm-machine-set-call-context symbol)))
    (call-proc-by-name symbol)))

(define (sasm-machine-step-over)
  (if (null? (machine-read-reg *machine* 'pc))
      (error "Unable to advance PC register -- already at sequence end"))
  (machine-write-reg *machine*
                     'pc
                     (cdr (machine-read-reg *machine* 'pc))))

(define (sasm-machine-dump-pc)
  (define (pre-instructions)
    (reverse (list-prefix (cdr (memq (car (machine-read-reg *machine* 'pc))
                                     (reverse *controller*)))
                          5)))
  (define (post-instructions)
    (list-prefix (machine-read-reg *machine* 'pc) 5))
  (let ((current-pc (machine-read-reg *machine* 'pc)))
    (if (not (null? current-pc))
        (for-each (lambda (x)
                    (if (eq? x (car (machine-read-reg *machine* 'pc)))
                        (display "> ")
                        (display "  "))
                    (write x)
                    (newline))
                  (append (pre-instructions)
                          (post-instructions)))
        #f)))

(define (sasm-machine-dump)
  (machine-dump-state *machine*)
  (sasm-machine-dump-pc))

(define (sasm-machine-dump-regs)
  (machine-dump-regs *machine*))

(define (sasm-machine-dump-stack)
  (machine-dump-stack *machine*))

(define (sasm-debugger-repl run-init-steps?)
  (if run-init-steps?
      (begin
        (set! *debug-mode* #t)
        (machine-define-symconst! *machine* *sasm-assemble-symconst-alist*)
        (machine-write-reg *machine* 'pc '())
        (sasm-machine-step-into *main-entry*))
      #f)
  ;; begin debugger repl
  (display "sasm> ")
  (let loop ((input (read (current-input-port))))
    (let ((cmd (cond ((symbol? input)
                      input)
                     ((pair? input)
                      (car input))
                     (else
                      #f)))
          (args (if (pair? input)
                    (cdr input)
                    '())))
      (define (dump-address address offset)
        (let* ((display-address (if (not (zero? offset))
                                    (if (number? address)
                                        (+ address offset)
                                        (string-append (symbol->string address)
                                                       "+"
                                                       (number->string offset)))
                                    address))
               (value (memory-ref address offset)))
          (if (number? value)
              (println " " display-address ":  " value "  (0x"
                       (number->string value 16)
                       ")")
              (println " " display-address ":  " value))))
      (define (next-command)
        (display "sasm> ")
        (loop (read (current-input-port))))
      (define (check-args x)
        (= (length args) x))
      (define (fail-args message)
        (display "Incorrect arguments for command ")
        (display cmd)
        (display " ")
        (display args)
        (newline)
        (display "Usage: ")
        (display cmd)
        (display " ")
        (display message)
        (newline))
      (define (previous-label)
        (let loop ((current-instruction *controller*)
                   (current-label #f))
          (cond ((null? current-instruction)
                 current-label)
                ((eq? current-instruction (machine-read-reg *machine* 'pc))
                 current-label)
                ((eqv? (caar current-instruction) 'label)
                 (loop (cdr current-instruction) (car current-instruction)))
                (else
                 (loop (cdr current-instruction) current-label)))))
      (case cmd
        ((previous-label pl)
         (if (check-args 0)
             (let ((result (previous-label)))
               (display result)
               (newline))
             (fail-args ""))
         (next-command))
        ((step s)
         (cond ((null? args)
                (sasm-machine-step))
               ((= 1 (length args))
                (let loop ((index 0))
                  (if (and (not (null? (machine-read-reg *machine* 'pc)))
                           (<= index (car args)))
                      (begin (sasm-machine-step)
                             (loop (+ index 1))))))
               (else
                (fail-args "[count]")))
         (sasm-machine-dump)
         (next-command))
        ((step-over so)
         (let ((starting-pc (machine-read-reg *machine* 'pc)))
           (if (not (null? starting-pc))
               (let ((ending-pc (cdr starting-pc)))
                 (let loop ((current-pc (machine-read-reg *machine* 'pc)))
                   (if (and (not (null? current-pc))
                            (not (eq? ending-pc current-pc)))
                       (begin (sasm-machine-step)
                              (loop (machine-read-reg *machine* 'pc))))))))
         (sasm-machine-dump)
         (next-command))
        ((read-reg)
         (if (check-args 1)
             (begin
               (display (machine-read-reg *machine* (list-ref args 0)))
               (newline))
             (fail-args "<register-name>"))
         (next-command))
        ((write-reg)
         (if (check-args 2)
             (machine-write-reg *machine*
                                (list-ref args 0)
                                (list-ref args 1))
             (fail-args "<register-name> <value>"))
         (next-command))
        ((dump d)
         (cond ((null? args)
                (display "entry: ")
                (display *main-entry*)
                (newline)
                (sasm-machine-dump))
               ((= 1 (length args))
                (dump-address (car args) 0))
               ((= 2 (length args))
                (let ((count (list-ref args 1))
                      (pointer (list-ref args 0)))
                  (let loop ((index 0))
                    (if (< index count)
                        (begin (dump-address pointer index)
                               (loop (+ index 1)))))))
               (else
                (fail-args "[address] [count]")))
         (next-command))
        ((call)
         (if (check-args 1)
             (begin (sasm-machine-step-into (car args))
                    (sasm-interp-iter-until-return))
             ;(sasm-interp-machine-iter *controller* *machine*)
             (fail-args "<function-symbol>"))
         (next-command))
        ((step-into)
         (if (check-args 1)
             (sasm-machine-step-into (car args))
             (fail-args "<function-symbol>"))
         (next-command))
        ((step-until su)
         (if (check-args 1)
             (let ((ending-insn `(label ,(car args))))
               (let loop ((current-pc (machine-read-reg *machine* 'pc)))
                 (if (and (not (null? current-pc))
                          (not (equal? ending-insn (car current-pc))))
                     (begin (sasm-machine-step)
                            (loop (machine-read-reg *machine* 'pc))))))
             (fail-args "<label-symbol>"))
         (sasm-machine-dump)
         (next-command))
        ((quit q)
         (machine-dump-state *machine*))
        (else
         (display "Invalid command ")
         (display cmd)
         (newline)
         (next-command))))))

;; command-line processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-run context port)
  (sasm-load-port context port)
  (sasm-execute))

(define (load-arg idx)
  (machine-get-arg *machine* idx))

(define (store-arg idx val)
  (machine-set-arg *machine* idx val))

(define (load-local idx)
  (machine-get-local *machine* idx))

(define (store-local idx val)
  (machine-set-local *machine* idx val))

(define (reserve-locals n)
  (machine-push-locals *machine* n))

;; The following functions are PLT extensions, I will need to write my
;; own eventually for the interpreter.  These are currently limited to
;; 32-bit.

(define (bit-fixneg x)
  (if (negative? x)
      (bitwise-and (bitwise-not (- (- x) 1)) #xffffffff)
      x))

(define (bit-twoscomp x)
  (if (not (zero? (bitwise-and x #x80000000)))
      (- (bitwise-and (+ (bitwise-not (bitwise-and x #xffffffff)) 1)
                      #xffffffff))
      (bitwise-and x #xffffffff)))

(define (bit-xor x y)
  (bit-twoscomp (bitwise-xor (bit-fixneg x) y)))

(define (bit-or x y)
  (bit-twoscomp (bitwise-ior (bit-fixneg x) y)))

(define (bit-and x y)
  (bit-twoscomp (bitwise-and (bit-fixneg x) y)))

(define (bit-not x)
  (bit-twoscomp (bitwise-not x)))

(define (bit-rshift x y)
  (bit-twoscomp (arithmetic-shift (bit-fixneg x) (- (bitwise-and y 31)))))

(define (bit-arith-rshift x y)
  (let ((msb (bit-and (bit-fixneg x) #x80000000)))
    (let loop ((result (bitwise-and (bit-fixneg x) #xffffffff))
               (iter (bitwise-and y 31)))
      (if (zero? iter)
          (bit-twoscomp result)
          (loop (bit-or (bit-rshift result 1) msb)
                (- iter 1))))))

(define (bit-lshift x y)
  (bit-twoscomp (arithmetic-shift (bit-fixneg x) (bitwise-and y 31))))

;; Install interpreter extensions

(install-operation 'debug-out
                   (lambda args
                     (define (display-helper x)
                       (if (machine-valid-return-address? x)
                           (display `($return-address ,(machine-return-address-context x)) (sasm-interp-output-port))
                           (display x (sasm-interp-output-port))))
                     (let ((step-count (machine-read-reg *machine* 'step-count)))
                       (if (not (zero? step-count))
                           (begin (display-helper "@step-count ")
                                  (display-helper step-count)
                                  (display-helper ": ")))
                       (for-each display-helper args)
                       (newline (sasm-interp-output-port)))))
(install-operation 'debug-print (lambda args (for-each display args)))
(install-operation 'dump (lambda (x) (write x) (newline)))
(install-operation 'dump-state (lambda () (machine-dump-state *machine*)))
(install-operation 'bit-xor bit-xor)
(install-operation 'bit-or bit-or)
(install-operation 'bit-and bit-and)
(install-operation 'bit-not bit-not)
(install-operation 'bit-rshift bit-rshift)
(install-operation 'hyg-bit-rshift bit-rshift)
(install-operation 'bit-arith-rshift bit-arith-rshift)
(install-operation 'hyg-bit-arith-rshift bit-arith-rshift)
(install-operation 'bit-lshift bit-lshift)
(install-operation 'hyg-bit-lshift bit-lshift)
(install-operation 'left-shift bit-lshift)
(install-operation 'add (lambda (x y)
                          (define (symbolic-add x y)
                            (if (and (symbol? x)
                                     (integer? y))
                                (cons x y) ; adding offset to symbolic pointer
                                (+ x y)))
                          (if (symbol? y)
                              (symbolic-add y x)
                              (symbolic-add x y))))
(install-operation 'add-pointer sasm-interp-add-pointer)

(define (wrap-arithmetic-operation op)
  (lambda (x y)
    (if (or (not (number? x))
            (not (number? y)))
        (begin (sasm-machine-dump)
               (error "Invalid arguments to operation" op x y)))
    (op x y)))

(install-operation 'mul (wrap-arithmetic-operation *))
(install-operation 'hyg-mul (wrap-arithmetic-operation *)) ;; hygienic multiply
(install-operation 'sub (wrap-arithmetic-operation -))
(install-operation 'sub-pointer (wrap-arithmetic-operation -))
(install-operation 'less-than (wrap-arithmetic-operation (lambda (x y) (if (< x y) 1 0))))
(install-operation 'greater-than (wrap-arithmetic-operation (lambda (x y) (if (> x y) 1 0))))
(install-operation 'less-than-or-equal (wrap-arithmetic-operation (lambda (x y) (if (<= x y) 1 0))))
(install-operation 'greater-than-or-equal (wrap-arithmetic-operation (lambda (x y) (if (>= x y) 1 0))))

(install-operation 'equal-to (lambda (x y) (if (eqv? x y) 1 0)))
(install-operation 'nonzero (lambda (x) (if (number? x)
                                            (if (zero? x) 0 1)
                                            (if (and (symbol? x)
                                                     (not (eqv? '$invalid-memory x)))
                                                1
                                                (error "unexpected object in nonzero operation" x)))))
(install-operation 'load-arg load-arg)
(install-operation 'store-arg store-arg)
(install-operation 'load-local load-local)
(install-operation 'reserve-locals reserve-locals)
(install-operation 'push-frame (lambda () (machine-push-frame *machine*)))
(install-operation 'pop-frame (lambda () (machine-pop-frame *machine*)))
(install-operation 'store-local store-local)
(install-operation 'store-array store-array)
(install-operation 'load-array load-array)
(install-operation 'call call)
(install-operation 'this-call call)
(install-operation 'tail-call tail-call)
(install-operation 'side-effect (lambda () #f))
(install-operation 'use-garbage (lambda (x) #f))

;;
;; Interpreter hook operations which allow the implementation of
;; garbage collection in the interpreter.  See
;; rtl/interp-rtlheap.sasm.
;;

(define (interp-gc-auxiliary-stack)
  (machine2-auxiliary-stack *machine*))

(define (interp-gc-global-addrs)
  (list->vector (map car *memory*)))

(define (interp-gc-test-symbolic-pointer addr)
  (if (and (or (symbol? addr)
               (and (pair? addr)
                    (symbol? (car addr))))
           (not (machine-valid-return-address? addr))
           (assoc (sasm-interp-symbolic-pointer-key addr)
                  *memory*))
      1
      0))

(define (interp-gc-memory-area-size addr)
  (if (vector? addr)
      (vector-length addr)
      (memory-ref-area-size addr 0)))

(define (interp-gc-memory-ref addr offset)
  (if (vector? addr)
      (vector-ref addr offset)
      (memory-ref addr offset)))

(install-operation 'interp-gc-auxiliary-stack interp-gc-auxiliary-stack)
(install-operation 'interp-gc-global-addrs interp-gc-global-addrs)
(install-operation 'interp-gc-test-symbolic-pointer interp-gc-test-symbolic-pointer)
(install-operation 'interp-gc-memory-area-size interp-gc-memory-area-size)
(install-operation 'interp-gc-memory-ref interp-gc-memory-ref)
(install-operation 'interp-gc-not-a-number
                   (lambda (x) (if (or (symbol? x)
                                       ;; handles $return-address, $invalid-memory
                                       (machine-valid-return-address? x)
                                       (input-port? x)
                                       (output-port? x))
                                   1
                                   0)))


(sasm-init)
(let ((debug-mode #f))
  (cond ((= 1 (vector-length *argv*))
         (sasm-run (sasm-interp-digest-interactive-context) (current-input-port)))
        ((> (vector-length *argv*) 1)
         (for-each
          (lambda (arg)
            (cond
             ((starts-with? arg "--out=")
              (set! *sasm-interp-output-port*
                    (open-output-file (string-strip-prefix arg
                                                           "--out="))))
             ((string=? arg "--debug")
              (set! debug-mode #t))
             (else
              (sasm-load-file (sasm-interp-digest-file-context arg)
                              arg))))
          (cdr (vector->list *argv*)))
         (if debug-mode
             (begin (display "sasm interactive debugger 0.19a")
                    (newline)
                    (sasm-debugger-repl #t))
             (sasm-execute)))
        (else
         (error "Invalid command line " *argv*))))

(if *sasm-interp-output-port*
    (close-output-port *sasm-interp-output-port*))
