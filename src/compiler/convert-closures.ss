(library (compiler convert-closures)
  (export convert-closures)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))
  
  (define-pass convert-closures : L7a (x) -> L8a ()
    (definitions
      (define (make-cp x)
	(unique-name 'cp)))
    (Expr : Expr (x) -> Expr ()
	  [(letrec ([,x* (lambda (,fml** ...) (free (,x** ...) ,[body*]))] ...) ,[body])
	   (let ([l* (map unique-label x*)]
		 [cp* (map make-cp x*)])
	     `(letrec ([,l* (lambda (,cp* ,fml** ...)
			      (bind-free (,x** ...) ,body*))] ...)
		(closures ([,x* ,l* ,x** ...] ...) ,body)))]
	  [(,x ,[e*] ...) `(,x ,x ,e* ...)]
	  [(,pr ,[e*] ...) `(,pr ,e* ...)]
	  [(,[e] ,[e*] ...)
	   (let ([t (unique-name 'tmp)])
	     `(let ([,t ,e]) (,t ,t ,e* ...)))]))
  )
