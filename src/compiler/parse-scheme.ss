(library (compiler parse-scheme)
  (export parse-scheme)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass parse-scheme : Lsrc (x) -> L1 ()
    (definitions
      (define (extend-env env var uvar)
	(cons (cons var uvar) env))
      (define (extend-env* env var* uvar*)
	(append (map cons var* uvar*) env))
      (define (lookup env var)
	(cond
	 [(assq var env) => cdr]
	 [else (errorf who "unbound identifier ~s" var)]))
      (define (check-unique ls what)
	(let loop ([ls ls])
          (cond
           [(null? ls) (values)]
           [else
            (if (memq (car ls) (cdr ls))
		(errorf who "Duplicate ~s found at ~s" what ls)
		(loop (cdr ls)))]))))
    (Expr : Expr (ir [env '()]) -> Expr ()
	  [(quote ,d) `(quote ,d)]
	  [,c `(quote ,c)]
	  [,x
	   (cond
	    [(assq x env) => cdr]
	    [(primitive? x) x]
	    [else (errorf who "unbound identifier ~s" x)])]
	  [(set! ,x ,[e]) `(set! ,(lookup env x) ,e)]
	  [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
	  [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]
	  [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
	  [(letrec ([,x* ,e*] ...) ,body* ... ,body)
	   (begin
	     (check-unique x* 'bindings)
	     (let* ([uvar* (map unique-name x*)]
		    [new-env (extend-env* env x* uvar*)]
		    [Expr* (lambda (x) (Expr x new-env))])
	       `(letrec ([,uvar* ,(map Expr* e*)] ...)
		  (begin ,(map Expr* body*) ... ,(Expr* body)))))]
	  [(let ([,x* ,[e*]] ...) ,body* ... ,body)
	   (begin
	     (check-unique x* 'bindings)
	     (let* ([uvar* (map unique-name x*)]
		    [new-env (extend-env* env x* uvar*)]
		    [Expr* (lambda (x) (Expr x new-env))])
	       `(let ([,uvar* ,e*] ...)
		  (begin ,(map Expr* body*) ... ,(Expr* body)))))]
	  [(lambda (,x* ...) ,body* ... ,body)
	   (begin
	     (check-unique x* 'parameters)
	     (let* ([uvar* (map unique-name x*)]
		    [new-env (extend-env* env x* uvar*)]
		    [Expr* (lambda (x) (Expr x new-env))])
	       `(lambda (,uvar* ...) (begin ,(map Expr* body*) ... ,(Expr* body)))))]
	  [(and) `(quote #t)]
	  [(and ,[e] ,[e*] ...)
	   (let f ([e e] [e* e*])
	     (if (null? e*)
		 e
		 `(if ,e ,(f (car e*) (cdr e*)) (quote #f))))]
	  [(or) `(quote #f)]
	  [(or ,[e] ,[e*] ...)
	   (let f ([e e] [e* e*])
             (if (null? e*)
		 e
		 (let ([t (unique-name 'tmp)])
		   `(let ([,t ,e]) (if ,t ,t ,(f (car e*) (cdr e*)))))))]
	  [(not ,[e]) `(if ,e '#f '#t)]
	  [(,[e] ,[e*] ...) `(,e ,e* ...)]
	  [else (errorf who "Invalid Expression ~s" ir)]))
  )
