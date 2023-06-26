(library (compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass remove-anonymous-lambda : L5a (x) -> L5a ()
    (Expr : Expr (x) -> Expr ()
	  (definitions
	    (define translate-let-rhs
	      (lambda (x)
		(nanopass-case (L5a Expr) x
			       [(lambda (,fml* ...) ,body)
				`(lambda (,fml* ...) ,(Expr body))]
			       [else (Expr x)]))))
	  [(letrec ([,x* (lambda (,fml* ...) ,[body])]) ,[e])
	   `(letrec ([,x* (lambda (,fml* ...) ,body)]) ,e)]
	  [(lambda (,fml* ...) ,[body])
	   (let ([anon (unique-name 'anon)])
	     `(letrec ([,anon (lambda (,fml* ...) ,body)]) ,anon))]
	  [(let ([,x* ,e*] ...) ,[body])
	   `(let ([,x* ,(map translate-let-rhs e*)] ...) ,body)]))
  )
