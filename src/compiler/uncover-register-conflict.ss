(library (compiler uncover-register-conflict)
  (export uncover-register-conflict)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass %uncover-register-conflict : L21 (x ct) -> * (live*)
    (definitions
      ;; It updates the conflict list based on new conflicts
      (define add-conflicts!
	(lambda (ct lhs live*)
	  (define add-conflict!
            (lambda (var1 var2)
              (let ([a (assq var1 ct)])
		(set-cdr! a (set-cons var2 (cdr a))))))
	  (when (uvar? lhs)
            (for-each
             (lambda (live) (add-conflict! lhs live))
             live*))
	  (for-each
	   (lambda (live) (when (uvar? live) (add-conflict! live lhs)))
	   live*)))
      (define Effect*
	(lambda (e* live*)
	  (let loop ([re* (reverse e*)] [live* live*])
	    (cond
             [(null? re*) live*]
             [else (loop (cdr re*) (Effect (car re*) live*))])))))
    (Triv : Triv (x) -> * (live*)
	  [,x (list x)]
	  [,fv (list fv)]
	  [else '()])
    (Rhs : Rhs (x) -> * (live*)
	 [,triv (Triv triv)]
	 [(,binop ,triv1 ,triv2) (union (Triv triv1) (Triv triv2))]
	 [(alloc ,triv) (Triv triv)]
	 [(mref ,triv1 ,triv2) (union (Triv triv1) (Triv triv2))])
    (Effect : Effect (x live*) -> * (live*)
	    [(nop) live*]
            [(if ,p0 ,e1 ,e2) (Pred p0 (Effect e1 live*) (Effect e2 live*))]
            [(begin ,e* ... ,e) (Effect* e* (Effect e live*))]
            [(set! ,var ,rhs)
	     (begin
	       (add-conflicts! ct var live*)
	       (union (Rhs rhs) (remq var live*)))]
	    [(mset! ,triv1 ,triv2 ,triv3) (union (Triv triv1) (Triv triv2) (Triv triv3) live*)]
	    [(return-point ,l ,tbody)
	     (union (Tail tbody) live*)])
    (Pred : Pred (x t-live* f-live*) -> * (live*)
	  [(if ,p0 ,p1 ,p2) (union t-live* f-live* (Pred p0 (Pred p1 t-live* f-live*) (Pred p2 t-live* f-live*)))]
	  [(begin ,e* ... ,pbody) (Effect* e* (Pred pbody t-live* f-live*))]
	  [(,relop ,triv1 ,triv2) (union t-live* f-live* (Triv triv1) (Triv triv2))]
	  [(true) t-live*]
	  [(false) f-live*])
    (Tail : Tail (x) -> * (live*)
	  [(begin ,e* ... ,tbody) (Effect e* (Tail tbody))]
	  [(if ,p0 ,t1 ,t2) (Pred p0 (Tail t1) (Tail t2))]
	  [(call ,triv ,loc* ...) (union (Triv triv) (filter frame-var? loc*))])
    (Tail x))

  (define-pass uncover-register-conflict : L20 (x) -> L21 ()
    (Body : Body (x) -> Body ()
	  [(locals (,local* ...)
		   (ulocals (,ulocal* ...)
			    (locate ([,x* ,fv*] ...)
				    (frame-conflict ([,y* ,var** ...] ...) ,[tbody]))))
	   (let* ([ct (map (lambda (x) (cons x '())) (append local* ulocal*))]
		  [live* (%uncover-register-conflict tbody ct)])
	     (if (not (null? (filter (lambda (x) (uvar? x)) live*)))
		 (error who "malformed input"))
	     `(locals (,local* ...)
		      (ulocals (,ulocal* ...)
			       (locate ([,x* ,fv*] ...)
				       (frame-conflict ([,y* ,var** ...] ...)
						       (register-conflict ((,(map car ct) ,(map cdr ct) ...) ...) ,tbody))))))])))
