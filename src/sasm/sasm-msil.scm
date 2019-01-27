;; sasm-msil.scm
;; MSIL backend for SASM

;; The MSIL backend needs some extra, high-level information
;; that the other traditional assembly backends don't need.
;; There's no use in reconstructing it here.  It will just
;; be passed in.
;;
;; - For each class:
;;   - The parent class (or none at all which implies [mscorlib]System.Object
;;   - The names and types of any instance vars
;;   - The names and types of any static vars
;;   - The names and signatures of any instance methods
;;   - The names and signatures of any static methods
;; 
;; - For each method:
;;   - The signature of each method
;;   - The number and types of local vars <-- Not really, ilasm will compute it

