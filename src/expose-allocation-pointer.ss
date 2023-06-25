(library (expose-allocation-pointer)
  (export expose-allocation-pointer)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  ;; this is just a sample allocation that moves allocation pointer
  (define-pass expose-allocation-pointer : L17 (x) -> L17a ()
    (definitions
      (define allocation-pointer-register 'rdx))
    (Effect : Effect (x) -> Effect ()
	    [(set! ,var (alloc ,triv))
	     `(begin
		(set! ,var ,allocation-pointer-register)
		(set! ,allocation-pointer-register
                      (+ ,allocation-pointer-register ,triv)))])))
