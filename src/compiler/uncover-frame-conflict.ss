(library (compiler uncover-frame-conflict)
  (export uncover-frame-conflict)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass %uncover-frame-conflict : L18 (x ct) -> * (live* call-live*)
    (definitions
      (define call-live* '())
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
	     (begin
	       (set! call-live* (union live* call-live*))
	       (union (Tail tbody) live*))])
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
    (let ([live* (Tail x)])
      (values live* call-live*)))

  (define-pass uncover-frame-conflict : L17a (x) -> L18 ()
    (Body : Body (x) -> Body ()
	  [(locals (,x* ...) (new-frames ((,nfv** ...) ...) ,[tbody]))
	   (let* ([ct (map list x*)])
	     (let-values ([(live* call-live*) (%uncover-frame-conflict tbody ct)])
	       (unless (null? live*)
		 (error who "found varibales ~s live on entry" live*))
	       (let ([spill* (filter uvar? call-live*)]
		     [y* (map car ct)]
		     [var** (map cdr ct)])
		 `(locals (,(difference x* spill*) ...)
			  (new-frames ((,nfv** ...) ...)
				      (spills (,spill* ...)
					      (frame-conflict ((,y* ,var** ...) ...)
							      (call-live (,call-live* ...) ,tbody))))))))]))
  )
