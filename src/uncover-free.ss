(library (uncover-free)
  (export uncover-free)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (sanitize-binding-forms))

  (define-language L7
    (extends L6)
    (FreeBody (fbody)
	      (+ (free (x* ...) body)))
    (LambdaExpr (le)
		(- (lambda (fml* ...) body))
		(+ (lambda (fml* ...) fbody))))

  ;; Interestingly, this pass is right in the nanopass user guide.
  ;; But I haven't found a documentation on this pass, so I will
  ;; just copy this pass verbatim.
  (define-pass uncover-free : L6 (x) -> L7 ()
    (Expr : Expr (x) -> Expr ('())
	  [(quote ,c) (values `(quote ,c) '())]
	  [,x (values x (list x))]
	  [(let ([,x* ,[e* free**]] ...) ,[e free*])
	   (values `(let ([,x* ,e*] ...) ,e)
		   (apply union (difference free* x*) free**))]
	  [(letrec ([,x* ,[le* free**]] ...) ,[body free*])
	   (values `(letrec ([,x* ,le*] ...) ,body)
		   (difference (apply union free* free**) x*))]
	  [(if ,[e0 free0*] ,[e1 free1*] ,[e2 free2*])
	   (values `(if ,e0 ,e1 ,e2) (union free0* free1* free2*))]
	  [(begin ,[e* free**] ... ,[e free*])
	   (values `(begin ,e* ... ,e) (apply union free* free**))]
	  [(,pr ,[e* free**]...)
	   (values `(,pr ,e* ...) (apply union free**))]
	  [(,[e free*] ,[e* free**] ...)
	   (values `(,e ,e* ...) (apply union free* free**))])
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr (free*)
		[(lambda (,x* ...) ,[body free*])
		 (let ([free* (difference free* x*)])
		   (values `(lambda (,x* ...) (free (,free* ...) ,body)) free*))])
    (Program : Program (x) -> Program ()
	     [,e
	      (let-values ([(e free*) (Expr e)])
		(unless (null? free*) (error who "found unbound variables" free*))
		e)]))
  )
