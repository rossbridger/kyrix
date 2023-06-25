(library (compiler convert-complex-datum)
  (export convert-complex-datum)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))


  ;; TODO: record the datums into another secton of the program,
  ;; so that fasl recording can serialize them.
  (define-pass convert-complex-datum : L1 (x) -> L2 ()
    (definitions
      (define const-x* '())
      (define const-e* '())
      (define datum->expr
	(with-output-language (L2 Expr)
			      (lambda (d)
				(printf "converting ~s\n" d)
				(cond
				 [(pair? d) `(cons ,(datum->expr (car d)) ,(datum->expr (cdr d)))]
				 [(vector? d)
				  (let ([n (vector-length d)])
				    (if (fxzero? n)
					`(make-vector (quote 0))
					(let ([t (unique-name 'tmp)])
					  `(let ([,t (make-vector (quote ,n))])
					     (begin
					       ,(do ([i (fx- n 1) (fx- i 1)]
						     [ls '() (cons `(vector-set! ,t (quote ,i) ,(datum->expr (vector-ref d i))) ls)])
						    ((< i 0) ls)) ... ,t)))))]
				 [else `(quote ,d)])))))
    (Expr : Expr (compiler ir) -> Expr ()
	  [(quote ,d)
	   (guard (not (constant? d)))
	   (let ([t (unique-name 'tmp)])
	     (set! const-x* (cons t const-x*))
	     (set! const-e* (cons (datum->expr d) const-e*))
	     t)])
    (let ([x (Expr x)])
      (if (null? const-x*)
	  x
	  `(let ([,const-x* ,const-e*] ...) ,x)))))
