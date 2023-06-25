(library (convert-assignments)
  (export convert-assignments)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  ;; introduced box primitive (srfi 111) to replace cons
  (define-pass convert-assignments : L4 (x) -> L5 ()
    (AssignedBody : AssignedBody (x boxed*) -> Expr ()
		  (definitions
		    (define build-let
		      (lambda (x* e* body)
			(if (null? x*)
			    body
			    `(let ([,x* ,e*] ...) ,body)))))
		  [(assigned (,uv* ...) ,body)
		   (let* ([x* (map unique-name uv*)]
			  [e* (map (lambda (uv)
				     `(box ,uv)) uv*)])
		     (build-let x* e* (Expr body
					    (append (map cons uv* x*) boxed*))))])
    (Expr : Expr (x (boxed* '())) -> Expr ()
	  [(let ([,uv* ,[e*]] ...) ,abody)
	   `(let ([,uv* ,e*] ...) ,(AssignedBody abody boxed*))]
	  [(lambda (,uv* ...) ,abody)
	   `(lambda (,uv* ...) ,(AssignedBody abody boxed*))]
	  [(set! ,uv ,[e])
	   (let ([boxed (assq uv boxed*)])
	     (if boxed
		 `(set-box! ,(car boxed) ,e)
		 (error who "~a is not boxed and can not be converted" uv)))]
	  [,uv (let ([boxed (assq uv boxed*)])
		 (if boxed
		     `(unbox ,(car boxed))
		     uv))]
	  ))
  )
