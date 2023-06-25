(library (compiler optimize-direct-call)
  (export optimize-direct-call)

  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass optimize-direct-call : L5 (x) -> L5 ()
    (Expr : Expr (x) -> Expr ()
	  [((lambda (,uv* ...) ,[body]) ,[e*] ...)
	   (if (eq? (length uv*) (length e*))
	       `(let [(,uv* ,e*) ...] ,body))])))
