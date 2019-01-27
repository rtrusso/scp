(need build/rules)

(define output-root "out/")

(define (out . args)
  (path-append output-root (apply string-append args)))

(define (scm-command command)
  (string-append "scm -b -r5 -q -lrun-scm.scm "
                 command))

(define (scm-interp-command input output)
  (scm-command (string-append "scheme-interpreter.scm --conspiracy --run="
                              input
                              " --out="
                              output)))

(define (scm-ut-command ut-name)
  (scm-interp-command (string-append "tests/" ut-name ".ss")
                      (out ut-name "-unittest.actual")))

(define (scm-ut-rule ut-name)
  (new-rule (list (out ut-name "-unittest.actual"))
            (list (string-append "tests/" ut-name ".ss")
                  "scheme-interpreter.scm")
            (list (scm-ut-command ut-name))))

(define (diff-command before after output)
  (string-append "diff --strip-trailing-cr "
                 before
                 " "
                 after
                 " >"
                 output))

(define (ut-diff-command ut-name)
  (diff-command (out ut-name "-unittest.actual")
                (string-append "tests/baseline/" ut-name "-unittest.actual")
                (out ut-name "-unittest.test")))


(define (scm-ut-diff-rule ut-name)
  (new-rule (list (out ut-name "-unittest.test"))
            (list (string-append "tests/baseline/" ut-name "-unittest.actual")
                  (out ut-name "-unittest.actual"))
            (list (ut-diff-command ut-name))))

(define (new-unit-test-rules ut-name)
  (list (scm-ut-diff-rule ut-name)
        (scm-ut-rule ut-name)))

(define (compile-java-command class-name)
  (scm-command (string-append "java-compiler.scm --main="
                              class-name
                              " rtl/JavaRtl.java tests/"
                              class-name
                              ".java --out="
                              (out class-name ".sasm"))))

(define (sasm-opt-command sasm-input)
  (scm-command (string-append "sasm-opt.scm "
                              sasm-input
                              " --out="
                              sasm-input
                              "-opt")))

(define (java-sasm-opt-command class-name)
  (sasm-opt-command (out class-name ".sasm")))

(define (new-compile-java-rule class-name)
  (list 
   (new-rule (list (out class-name ".sasm"))
             (list "rtl/JavaRtl.java"
                   (string-append "tests/" class-name ".java"))
             (list (compile-java-command class-name)))
   (new-rule (list (out class-name ".sasm-opt"))
             (list (out class-name ".sasm"))
             (list (java-sasm-opt-command class-name)))
   ))

(define (map-rules function objects)
  (let loop ((objects objects)
             (rules '()))
    (if (null? objects)
        rules
        (loop (cdr objects)
              (append rules (function (car objects)))))))

(define unit-tests
  (list "list"
        "fastset"
        "genparse"
        "genproc"
        "pat"
        "regex"
        "sasm-insn"))

(define ut-rules
  (map-rules new-unit-test-rules unit-tests))

(define java-class-names
  (list "Arrays"
        "Arrays"
        "BinarySearch"
        "BinaryTree"
        "Bitwise"
        "BubbleSort"
        "CharString"
        "Count"
        "CtorTest"
        "Factorial"
        "LinearSearch"
        "LinkedList"
        "Messy"
        "MyFactorial"
        "NumberToString"
        "ObjArray"
        "OpEquals"
        "OverrideTest"
        "QuickSort"
        "Rectangles"
        "StaticMembers"
        "StaticMethods"
        "SubExp"
        "TreeVisitor"
        "TwoArgs"
        ))

(define java-rules
  (map-rules new-compile-java-rule java-class-names))

(define project
  (new-project
   (append ut-rules
           java-rules
           '())))

(if (not (fs-exists? (read-fs "out")))
    (begin (display ";; creating output directory")
           (newline)
           (create-directory "out")))
(build-project project)
