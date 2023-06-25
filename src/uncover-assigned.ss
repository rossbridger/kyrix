(library (uncover-assigned)
  (export L3 unparse-L3 uncover-assigned)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass uncover-assigned : L2 (x) -> L3 ()
    (Expr : Expr (x) -> Expr ('())
	  [(quote ,c) (values `(quote ,c) '())]
	  [(set! ,uv ,[e as]) (values `(set! ,uv ,e) (set-cons uv as))]
	  [(letrec ([,uv* ,[e* e-as*]] ...) ,[body body-as])
	   (let* ([as* (union (apply union e-as*) body-as)]
		  [assigned-uv* (intersection uv* as*)])
	     (values `(letrec ([,uv* ,e*] ...) (assigned (,assigned-uv* ...) ,body))
		     (difference as* uv*)))]
	  [(let ([,uv* ,[e* e-as*]] ...) ,[body body-as])
	   (let ([assigned-uv* (intersection uv* body-as)])
	     (values `(let ([,uv* ,e*] ...) (assigned (,assigned-uv* ...) ,body))
		     (union (apply union e-as*) (difference body-as uv*))))]
	  [(lambda (,uv* ...) ,[body as])
	   (let ([assigned-uv* (intersection uv* as)])
	     (values `(lambda (,uv* ...) (assigned (,assigned-uv* ...) ,body)) (difference as uv*)))]
	  [(begin ,[e* as*] ... ,[e as])
	   (values `(begin ,e* ... ,e) (union as (apply union as*)))]
	  [(,[e as] ,[e* as*] ...)
	   (values `(,e ,e* ...) (union as (apply union as*)))])
    (let-values ([(x as) (Expr x)])
      (if (null? as)
	  x
	  (error who "found one or more unbound variables" as))))
  )
