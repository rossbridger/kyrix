(library (expose-basic-blocks)
  (export expose-basic-blocks)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass expose-basic-blocks : L26 (x) -> L27 ()
    (definitions
      (define (then-label) (unique-label 'c))
      (define (else-label) (unique-label 'a))
      (define (bind-function label statement)
	(list (cons label statement))))
    (Pred : Pred (x true-label false-label) -> Tail (x)
	  [(true) (values `(,true-label) '())]
	  [(false) (values `(,false-label) '())]
	  [(begin ,e* ... ,p)
	   (let*-values ([(p-code p-bindings) (Pred p true-label false-label)]
			 [(e-code e-bindings) (Effect* e* (list p-code))])
	     (values e-code (append e-bindings p-bindings)))]
	  [(,relop ,triv1 ,triv2)
	   (values `(if (,relop ,triv1 ,triv2) (,true-label) (,false-label)) '())]
	  [(if ,p0 ,p1 ,p2)
	   (let ([conseq-label (then-label)]
		 [alt-label (else-label)])
	     (let-values ([(p-code p-bindings) (Pred p0 conseq-label alt-label)]
			  [(c-code c-bindings) (Pred p1 true-label false-label)]
			  [(a-code a-bindings) (Pred p2 true-label false-label)])
	       (values p-code (append (bind-function conseq-label c-code)
				      (bind-function alt-label a-code)
				      p-bindings c-bindings a-bindings))))])
    
    (Tail : Tail (x) -> Tail (x)
	   [(if ,p ,[t1 cbindings] ,[t2 abindings])
	    (let ([conseq-label (then-label)]
		  [alt-label (else-label)])
	      (let-values ([(code bindings) (Pred p conseq-label alt-label)])
		(values code
			(append cbindings abindings bindings
				(bind-function conseq-label t1)
				(bind-function alt-label t2)))))]
	   [(begin ,e* ... ,[t binding])
	    (let-values ([(code bindings) (Effect* e* (list t))])
	      (values code (append binding bindings)))]
	   [(,triv) (values `(,triv) '())])
    (Program : Program (x) -> Program ()
	     [(letrec ([,l* (lambda () ,[tbody* bindings*])] ...) ,[tbody binding])
	      (let ([bindings (car (append binding bindings*))])
		(let ([l* (append l* (map car bindings))]
		      [tbody* (append tbody* (map cdr bindings))])
		  `(letrec ((,l* (lambda () ,tbody*)) ...) ,tbody)))])

    (Effect : Effect (x before* after*) -> Statement (x)
	    [(if ,p0 ,e1 ,e2)
	     (let ([conseq-label (then-label)]
		   [alt-label (else-label)]
		   [jmp-label (unique-label 'j)])
	       (let*-values
		   ([(p-code p-bindings) (Pred p0 conseq-label alt-label)]
		    [(c-code c-bindings) (Effect e1 '() (list `(,jmp-label)))]
		    [(a-code a-bindings) (Effect e2 '() (list `(,jmp-label)))]
		    [(e-code e-bindings) (Effect* before* (list p-code))])
		 (values e-code (append (bind-function conseq-label c-code)
					(bind-function alt-label a-code)
					(bind-function jmp-label (if (null? after*)
								     '()
								     (make-begin after*)))
					p-bindings c-bindings a-bindings e-bindings))))]
	    [(set! ,[loc] ,[rhs])
	     (Effect* before* (append (list `(set! ,loc ,rhs)) after*))]
	    [(begin ,e* ...)
	     (let*-values
		 ([(e-code e-bindings) (Effect* e* after*)]
		  [(code b-bindings) (Effect* before* (list e-code))])
	       (values code (append e-bindings b-bindings)))]
	    [(nop) (Effect* before* after*)])
    (make-begin : * (code*) -> Tail ()
		(let ([t (car (reverse code*))])
		  (let ([code* (reverse (cdr (reverse code*)))])
		    `(begin ,code* ... ,t))))
    (Effect* : * (ef* code*) -> Tail (x)
	     (if (null? ef*)
		 (values (make-begin code*) '())
		 (let ([ef* (reverse ef*)])
		   (Effect (car ef*) (reverse (cdr ef*)) code*))))))
