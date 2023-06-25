(library (compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  ;; I have been too sleepy to test this code (it's 12:48 am atm), so I put it as-is.
  (define-pass introduce-procedure-primitives : L8 (x) -> L9 ()
    (definitions
      (define (closure-ref x cp free*)
	(with-output-language (L9 Expr)
			      (let loop ([i 0] [free* free*])
				(cond
				 [(null? free*) x]
				 [(eq? x (car free*)) `(procedure-ref cp (quote ,i))]
				 [else (loop (+ i 1) (cdr free*))])))))
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x ,fml* ...) (bind-free (,f* ...) ,[body x f* -> body]))
		 `(lambda (,x ,fml* ...) ,body)])
    (Expr : Expr (x [cp #f] [free* '()]) -> Expr ()
	  [,x (closure-ref x cp free*)]
	  [(,pr ,[e*] ...) `(,pr ,e* ...)]
	  [(,l ,[e*] ...) `(,l ,e* ...)]
	  [(,[e] ,[e*] ...) `((procedure-code ,e) ,e* ...)])
    (ClosureBody : ClosureBody (x cp free*) -> Expr ()
		 (definitions
		   (define (generate-procedure-sets x* f**)
		     (map (lambda (x f*)
			    (let loop ([f* f*] [i 0] [e* '()])
			      (if (null? f*)
				  e*
				  (loop (cdr f*)
					(+ i 1)
					(cons
					 `(procedure-set!
					   ,x (quote ,i)
					   ,(closure-ref (car f*) cp free*))
					 e*)))))
			  x* f**)))
		 [(closures ([,x* ,l* ,f** ...] ...) ,body)
		  (let ([size* (map length f**)])
		    `(let ([,x* (make-procedure ,l* (quote size*))] ...)
		       (begin
			 ,(generate-procedure-sets x* f**) ...
			 ,[body cp free*])))]))
  )
