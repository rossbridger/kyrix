(library (compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass remove-anonymous-lambda : L5 (x) -> L5 ()
    (Expr : Expr (x) -> Expr ()
	  (definitions
	    (define translate-let-rhs
	      (lambda (x)
		(nanopass-case (L5 Expr) x
			       [(lambda (,uv* ...) ,body)
				`(lambda (,uv* ...) ,(Expr body))]
			       [else (Expr x)]))))
	  [(letrec ([,uv* (lambda (,uv1* ...) ,[body])]) ,[e])
	   `(letrec ([,uv* (lambda (,uv1* ...) ,body)]) ,e)]
	  [(lambda (,uv* ...) ,[body])
	   (let ([anon (unique-name 'anon)])
	     `(letrec ([,anon (lambda (,uv* ...) ,body)]) ,anon))]
	  [(let ([,uv* ,e*] ...) ,[body])
	   `(let ([,uv* ,(map translate-let-rhs e*)] ...) ,body)]))
  )
