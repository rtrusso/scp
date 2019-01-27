(define (fsm? obj)
  (genproc-invoke 'fsm? obj))

(define (fsm.input! fsm obj)
  (genproc-invoke 'fsm.input! fsm obj))

(define (fsm.reset! fsm)
  (genproc-invoke 'fsm.reset! fsm))

(define (fsm.done? fsm)
  (genproc-invoke 'fsm.done? fsm))

(define (fsm.failed? fsm)
  (genproc-invoke 'fsm.failed? fsm))

