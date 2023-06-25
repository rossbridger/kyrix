(library (remove-complex-opera)
  (export remove-complex-opera*)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  ;; need to remove (nop) from the sequences
  (define-pass remove-complex-opera* : L14 (x) -> L15 ()
    (definitions
      ;; an opportunistic implementation: use this variable
      ;; to collect the temporaries allocated during the
      ;; transformation process.
      (define new-temps '())
      (define (make-tmp)
	(let ([tmp (unique-name 'tmp)])
	  (set! new-temps (cons tmp new-temps))
	  tmp)))
    ;; this is a superfluous version
    (Value : Value (x) -> Value ()
	   [(alloc ,[e triv]) `(begin ,e (alloc ,triv))]
	   [(,binop ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (,binop ,triv1 ,triv2))]
	   [(mref ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (mref ,triv1 ,triv2))]
	   [(if ,[p0] ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (if ,p0 ,triv1 ,triv2))]
	   [(begin ,[e*] ... ,[e triv]) `(begin ,e* ... ,e ,triv)]
	   [(call ,[e triv] ,[e* triv*] ...) `(begin ,(cons e e*) ... (call ,triv ,triv* ...))])
    (Effect : Effect (x) -> Effect ()
	    [(set! ,x ,[e triv]) `(begin ,(list e) ... (set! ,x ,triv))]
	    [(mset! ,[e1 triv1] ,[e2 triv2] ,[e3 triv3]) `(begin ,(list e1 e2 e3) ... (mset! ,triv1 ,triv2 ,triv3))]
	    [(call ,[e triv] ,[e* triv*] ...) `(begin ,(cons e e*) ... (call ,triv ,triv* ...))])
    (Pred : Pred (x) -> Pred ()
	  [(,relop ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (,relop ,triv1 ,triv2))])
    (Tail : Tail (x) -> Tail ()
	  [(mref ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (mref ,triv1 ,triv2))]
	  [(call ,[e triv] ,[e* triv*] ...) `(begin ,(cons e e*) ... (call ,triv ,triv* ...))]
	  [(,binop ,[e1 triv1] ,[e2 triv2]) `(begin ,(list e1 e2) ... (,binop ,triv1 ,triv2))]
	  [(alloc ,[e triv]) `(begin ,e (alloc ,triv))])
    ;; this is when we need a triv as args
    (trivialize-arg : Value (v) -> Effect (x)
		    [,triv (values `(nop) triv)]
		    [(alloc ,[e triv])
		     (let ([tmp (make-tmp)])
		       (values `(begin ,e (set! ,tmp (alloc ,triv))) tmp))]
		    [(,binop ,[e1 triv1] ,[e2 triv2])
		     (let ([tmp (make-tmp)])
		       (values `(begin ,(list e1 e2) ... (set! ,tmp (,binop ,triv1 ,triv2))) tmp))]
		    [(mref ,[e1 triv1] ,[e2 triv2])
		     (let ([tmp (make-tmp)])
		       (values `(begin ,(list e1 e2) ... (set! ,tmp (mref ,triv1 ,triv2))) tmp))]
		    [(if ,[p0] ,[e1 triv1] ,[e2 triv2])
		     (let ([tmp (make-tmp)])
		       (values `(begin ,(list e1 e2) ... (set! ,tmp (mref ,triv1 ,triv2))) tmp))]
		    [(begin ,[e*] ... ,[e triv])
		     (values `(begin ,e* ... ,e) triv)]
		    [(call ,[e triv] ,[e* triv*] ...)
		     (let ([tmp (make-tmp)])
		       (values `(begin ,(cons e e*) ... (set! ,tmp (call ,triv ,triv* ...))) tmp))])
    (Body : Body (x) -> Body ()
	  [(locals (,x* ...) ,[tbody])
	   (let ([x* (append x* new-temps)])
	     (set! new-temps '())
	     `(locals (,x* ...) ,tbody))]))
  )
