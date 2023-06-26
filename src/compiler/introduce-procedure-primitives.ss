(library (compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  ;; I have rewritten this pass to allow for set! expr
  (define-pass introduce-procedure-primitives : L8a (x) -> L9a ()
    (definitions
      (define (index x lst)
	(let loop ([i 0] [lst lst])
	  (cond
	   ([null? lst] #f)
	   ([eq? (car lst) x] i)
	   (else (loop (+ i 1) (cdr lst)))))))
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x ,fml* ...) (bind-free (,f* ...) ,[body x f* -> body]))
		 `(lambda (,x ,fml* ...) ,body)])
    (Expr : Expr (x [cp #f] [free* '()]) -> Expr ()
	  [(set! ,x ,[e]) (let ([i (index x free*)])
			    (if i ;; x is in the closure pointer
				`(procedure-set! cp (quote ,i) ,e)
				`(set! ,x ,e)))]
	  [,x (let ([i (index x free*)])
		(if i `(procedure-ref cp (quote ,i)) x))]
	  [(,pr ,[e*] ...) `(,pr ,e* ...)]
	  [(,l ,[e*] ...) `(,l ,e* ...)]
	  [(,[e] ,[e*] ...) `((procedure-code ,e) ,e* ...)])
    (ClosureBody : ClosureBody (x cp free*) -> Expr ()
		 (definitions
		   (define (closure-ref f cp free*)
		     (let ([i (index x free*)])
		       (if i `(procedure-ref cp (quote ,i)) x)))
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
		 [(closures ([,x* ,l* ,f** ...] ...) ,[body])
		  (let ([size* (map length f**)])
		    `(let ([,x* (make-procedure ,l* (quote size*))] ...)
		       (begin
			 ,(generate-procedure-sets x* f**) ...
			 ,body)))]))
  )
