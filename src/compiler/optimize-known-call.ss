(library (compiler optimize-known-call)
  (export optimize-known-call)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass optimize-known-call : L8a (x) -> L8a ()
    (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ())
    (FreeBody : FreeBody (x env) -> FreeBody ())
    (ClosureBody : ClosureBody (x env) -> ClosureBody ()
		 [(closures ([,x* ,l* ,f** ...] ...) ,body)
		  (let ([env (append (map cons x* l*) env)])
		    `(closures ([,x* ,l* ,f** ...] ...) ,(Expr body env)))])
    (Expr : Expr (x [env '()]) -> Expr ()
	  [(,x ,[e*] ...)
	   (cond
	    [(assq x env) => (lambda (as) `(,(cdr as) ,e* ...))]
	    [else `(,x ,e* ...)])])))
