(library (expose-frame-var)
  (export expose-frame-var)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass expose-frame-var : L24 (x) -> L25 ()
    (definitions
      (define frame-pointer-register 'rbp)
      (define word-shift 3))
    (frame-var : frame-var (x) -> Memory ()
	       (let [(offset (+ (ash (frame-var->index x) word-shift)))]
		 `(disp ,frame-pointer-register ,offset)))))
