(library (compiler purify-letrec)
  (export purify-letrec)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  ;; I took this pass from Andy Keep's Scheme to C compiler (https://github.com/akeep/scheme-to-c)
  (define-pass purify-letrec : L3 (x) -> L5a ()
    (definitions
      (define (simple? e bound* assigned*)
	(define (simple*? e)
	  (nanopass-case (L4 Expr) e
			 [(quote ,c) #t]
			 [,uv (not (or (memq uv bound*) (memq x assigned*)))]
			 [(,pr ,e* ...) (for-all simple*? e*)]
			 [(begin ,e* ... ,e) (and (for-all simple*? e*) (simple*? e))]
			 [(if ,e0 ,e1 ,e2) (and (simple*? e0) (simple*? e1) (simple*? e2))]
			 [else #f]))
	(simple*? e))
      (define (lambda? e)
	(nanopass-case (L4 Expr) e
		       [(lambda (,uv* ...) ,abody) #t]
		       [else #f]))
      ;; classify letrec expressions into simple, lambda and complex and return (uv . e)
      (define (classify uv* e* assigned*)
	(let loop ([e* e*] [x* uv*] [es* '()] [el* '()] [ec* '()] [xs* '()] [xl* '()] [xc* '()])
	  (cond
	   [(null? e*) (values es* el* ec* xs* xl* xc*)]
	   [(and (not (memq (car x*) assigned*)) (lambda? (car e*)))
	    (loop (cdr e*) (cdr x*) es* (cons (car e*) el*) ec* xs* (cons (car x*) xl*) xc*)]
	   [(and (not (memq (car x*) assigned*)) (simple? (car e*) uv* assigned*))
	    (loop (cdr e*) (cdr x*) (cons (car e*) es*) el* ec* (cons (car x*) xs*) xl* xc*)]
	   [else
	    (loop (cdr e*) (cdr x*) es* el* (cons (car e*) ec*) xs* xl* (cons (car x*) xc*))]))))
    (Expr : Expr (x) -> Expr ()
	  (definitions
	    (define build-let
              (lambda (x* e* body)
		(if (null? x*)
                    body
                    `(let ([,x* ,e*] ...) ,body))))
            (define build-letrec
              (lambda (x* e* body)
		(if (null? x*)
                    body
                    `(letrec ([,x* ,e*] ...) ,body))))
	    (define build-begin
              (lambda (e* e)
		(nanopass-case (L4 Expr) e
			       [(begin ,e1* ... ,e) 
				(build-begin (append e* e1*) e)]
			       [else
				(if (null? e*)
				    e
				    (let loop ([e* e*] [re* '()])
				      (if (null? e*)
					  `(begin ,(reverse re*) ... ,e)
					  (let ([e (car e*)])
					    (nanopass-case (L4 Expr) e
							   [(begin ,e0* ... ,e0)
							    (loop (append e0* (cons e0 (cdr e*))) re*)]
							   [else (loop (cdr e*) (cons (car e*) re*))])))))]))))
	  [(letrec ([,uv* ,[e*]] ...) (assigned (,uv0* ...) ,[body]))
	   (let-values ([(es* el* ec* xs* xl* xc*) (classify uv* e* uv0*)])
	     (build-let xs* es*
			(build-let xc* (map (lambda (x) `(void)) xc*)
				   (build-begin
				    (map (lambda (xc ec) `(set! ,xc ,ec)) xc* ec*)
				    body))))])))
