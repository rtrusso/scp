;; generated by sasm-opt 03-11-2018 0.1
(extern mm-heap-global-base)

(extern mm-heap-global-limit)

(extern mm-heap-assoc)

(extern mm-heap-sweep-area)

(extern root-heap-pointer)

(extern rtl-gc-mark-pointer)

(extern cp-rtl-mark-all-roots)

(extern mj-system-out-println)

(extern mj-system-out-println-string)

(extern mm-rtl-heap-begin-mark-global-vars)

(extern mm-rtl-heap-end-mark-global-vars)

(extern gc-root-stack-limit)

(extern gc-root-stack-base)

(extern gc-heap-base)

(extern gc-heap-limit)

(export gc-invoke)

(extern gc-sweep)

(extern cp-rtl-mark-all-roots)

(function
  (name gc-invoke)
  (locals 0)
  (body
   (push (sys ebp))
   (assign (sys ebp) (sys esp))
   (perform (op reserve-locals) (const 0))
   (perform (op call) (const cp-rtl-mark-all-roots))
   (perform (op call) (const gc-sweep))
   (assign (sys esp) (sys ebp))
   (pop (sys ebp))
   (return (const 0))
   ))

