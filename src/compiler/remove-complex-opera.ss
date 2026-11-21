(library (compiler remove-complex-opera)
  (export remove-complex-opera*)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  ;; Variables assigned by set! anywhere inside an L15 Effect.
  (define-pass assigned-in : L15 (ir) -> * (x*)
    (Effect : Effect (ir) -> * (x*)
	    [(set! ,x ,v) (set-cons x (Value v))]
	    [(if ,p0 ,e1 ,e2) (union (Pred p0) (Effect e1) (Effect e2))]
	    [(begin ,e* ... ,e) (apply union (Effect e) (map Effect e*))]
	    [else '()])
    (Value : Value (ir) -> * (x*)
	   [(if ,p0 ,v1 ,v2) (union (Pred p0) (Value v1) (Value v2))]
	   [(begin ,e* ... ,v) (apply union (Value v) (map Effect e*))]
	   [else '()])
    (Pred : Pred (ir) -> * (x*)
	  [(if ,p0 ,p1 ,p2) (union (Pred p0) (Pred p1) (Pred p2))]
	  [(begin ,e* ... ,p) (apply union (Pred p) (map Effect e*))]
	  [else '()])
    (Effect ir))

  ;; A-normalization: every operand of alloc, mref, mset!, binop, relop
  ;; and call must be a Triv.  A complex operand v is evaluated into a
  ;; fresh local (set! tmp v) ahead of the operation.  Operands keep
  ;; their left-to-right order: a variable operand is copied to a
  ;; temporary when a later operand's hoisted effects assign it.  The
  ;; right-hand side of set! may stay complex; flatten-set! handles it.
  (define-pass remove-complex-opera* : L14 (x) -> L15 ()
    (definitions
      (define new-local* '())
      (define (make-tmp)
	(let ([tmp (unique-name 'tmp)])
	  (set! new-local* (cons tmp new-local*))
	  tmp))
      ;; trivialize each operand in order; returns the Trivs and the
      ;; Effects that must run before they are used.
      (define (trivialize* v*)
	(if (null? v*)
	    (values '() '())
	    (let*-values ([(triv e1*) (trivialize (car v*))]
			  [(triv* e2*) (trivialize* (cdr v*))])
	      (if (and (uvar? triv)
		       (exists (lambda (e) (memq triv (assigned-in e))) e2*))
		  (let ([tmp (make-tmp)])
		    (values (cons tmp triv*)
			    (append e1*
				    (list (with-output-language (L15 Effect)
					    `(set! ,tmp ,triv)))
				    e2*)))
		  (values (cons triv triv*) (append e1* e2*))))))
      (define-syntax with-trivs
	(syntax-rules ()
	  [(_ nt ([triv v] ...) body)
	   (let-values ([(triv* e*) (trivialize* (list v ...))])
	     (let-values ([(triv ...) (apply values triv*)])
	       (let ([b body])
		 (if (null? e*)
		     b
		     (with-output-language (L15 nt) `(begin ,e* (... ...) ,b))))))])))
    (trivialize : Value (x) -> Triv (e*)
		[,triv (values triv '())]
		[(begin ,[e*] ... ,v)
		 (let-values ([(triv e2*) (trivialize v)])
		   (values triv (append e* e2*)))]
		[else
		 (let ([v (Value x)] [tmp (make-tmp)])
		   (values tmp
			   (list (with-output-language (L15 Effect) `(set! ,tmp ,v)))))])
    (Value : Value (x) -> Value ()
	   [(alloc ,v) (with-trivs Value ([t v]) `(alloc ,t))]
	   [(mref ,v1 ,v2) (with-trivs Value ([t1 v1] [t2 v2]) `(mref ,t1 ,t2))]
	   [(,binop ,v1 ,v2) (with-trivs Value ([t1 v1] [t2 v2]) `(,binop ,t1 ,t2))]
	   [(call ,v ,v* ...)
	    (let-values ([(t* e*) (trivialize* (cons v v*))])
	      (let ([c `(call ,(car t*) ,(cdr t*) ...)])
		(if (null? e*) c `(begin ,e* ... ,c))))])
    (Effect : Effect (x) -> Effect ()
	    [(mset! ,v1 ,v2 ,v3)
	     (with-trivs Effect ([t1 v1] [t2 v2] [t3 v3]) `(mset! ,t1 ,t2 ,t3))]
	    [(call ,v ,v* ...)
	     (let-values ([(t* e*) (trivialize* (cons v v*))])
	       (let ([c `(call ,(car t*) ,(cdr t*) ...)])
		 (if (null? e*) c `(begin ,e* ... ,c))))])
    (Pred : Pred (x) -> Pred ()
	  [(,relop ,v1 ,v2) (with-trivs Pred ([t1 v1] [t2 v2]) `(,relop ,t1 ,t2))])
    (Tail : Tail (x) -> Tail ()
	  [(alloc ,v) (with-trivs Tail ([t v]) `(alloc ,t))]
	  [(mref ,v1 ,v2) (with-trivs Tail ([t1 v1] [t2 v2]) `(mref ,t1 ,t2))]
	  [(,binop ,v1 ,v2) (with-trivs Tail ([t1 v1] [t2 v2]) `(,binop ,t1 ,t2))]
	  [(call ,v ,v* ...)
	   (let-values ([(t* e*) (trivialize* (cons v v*))])
	     (let ([c `(call ,(car t*) ,(cdr t*) ...)])
	       (if (null? e*) c `(begin ,e* ... ,c))))])
    (Body : Body (x) -> Body ()
	  [(locals (,local* ...) ,tbody)
	   (fluid-let ([new-local* '()])
	     (let ([tbody (Tail tbody)])
	       `(locals (,(append local* (reverse new-local*)) ...) ,tbody)))]))
  )
