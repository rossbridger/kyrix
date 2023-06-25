(library (compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass sanitize-binding-forms : L5 (x) -> L6 ()
		     (definitions
		       (define classify
			 (lambda (uv* e*)
			   (let loop ([uv* uv*] [e* e*] [f* '()] [fml** '()] [body* '()] [x* '()] [rhs* '()])
			     (if (null? uv*)
				 (values f* fml** body* x* rhs*)
				 (nanopass-case (L5 Expr) (car e*)
						[(lambda (,fml* ...) ,body)
						 (loop (cdr uv*) (cdr e*) (cons (car uv*) f*) (cons fml* fml**) (cons body body*) x* rhs*)]
						[else
						 (loop (cdr uv*) (cdr e*) f* fml** body* (cons (car uv*) x*) (cons (car e*) rhs*))]))))))
		     (Expr : Expr (x) -> Expr ()
			   (definitions
			     (define build-letrec
			       (lambda (f* fml** body* body)
				 (if (null? f*)
				     body
				     `(letrec ([,f* (lambda (,fml** ...) ,body*)] ...) ,body))))
			     (define build-let
			       (lambda (x* e* body)
				 (if (null? x*)
				     body
				     `(let ([,x* ,e*] ...) ,body)))))
			   [(let ([,uv* ,e*] ...) ,[body])
			    (let-values ([(f* fml** body* x* rhs*) (classify uv* e*)])
			      (build-letrec f* fml** (map Expr body*)
					    (build-let x* (map Expr rhs*) body)))]))
	  )
