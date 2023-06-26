(library (compiler optimize-direct-call)
  (export optimize-direct-call)

  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass optimize-direct-call : L5a (x) -> L5a ()
    (Expr : Expr (x) -> Expr ()
	  [((lambda (,x* ...) ,[body]) ,[e*] ...)
	   (if (eq? (length x*) (length e*))
	       `(let [(,x* ,e*) ...] ,body))])))
