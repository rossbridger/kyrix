(library (fold-common-expression)
  (export fold-common-expression)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  ;; This pass performs common subexpression elimination
  ;; only works on SSA form.
  ;; it works the following:
  ;; (set! x1 i) replace x1 with i
  ;; (set! x1 x2) replace x1 with x2
  ;; (binop i1 i2) replace with its result
  ;; (relop i1 i2) returns (true) or (false)
  (define-pass fold-common-expression : L14 (x) -> L14 ()
    (definitions
      (define sra (lambda (x n) (ash x (- n))))
      (define binop-map
	'(['+ . +]
	  ['- . -]
	  ['* . *]
	  ['logand logand]
	  ['logor logor]
	  ['sra sra]))
      (define relop-map
	'(['< . <]
	  ['<= . <=]
	  ['= . =]
	  ['> . >]
	  [>= . >=]))
      (define env '()))
    (Effect : Effect (ir) -> Effect ()
	    [(set! ,x ,[v])
	     (begin
	       (if (or (int64? v) (label? v) (uvar? v))
		   (set! env (cons (cons x v) env)))
	       ir)])
    (Value : Value (ir) -> Value ()
	   [,triv (cond
		   [(and (uvar? triv) (assq triv env)) => cdr]
		   [else ir])]
	   [(,binop ,[v1] ,[v2])
	    (if (and (int64? v1) (int64? v2))
		((cdr (assq binop binop-map) v1 v2))
		`(binop ,v1 ,v2))])
    (Tail : Tail (ir) -> Value ()
	  [,triv (cond
		  [(and (uvar? triv) (assq triv env)) => cdr]
		  [else ir])]
	  [(,binop ,[v1] ,[v2])
	   (if (and (int64? v1) (int64? v2))
	       ((cdr (assq binop binop-map) v1 v2))
	       `(binop ,v1 ,v2))])
    (Pred : Pred (ir) -> Pred ()
	  [(,relop ,[v1] ,[v2])
	   (if (and (int64? v1) (int64? v2))
	       (if ((cdr (assq relop relop-map)) v1 v2) `(true) `(false))
	       `(relop ,v1 ,v2))])
    (Body : Body (ir) -> Body ()
	  [(locals (,local* ...) ,[tbody])
	   (begin
	     (set! env '())
	     `(locals (,local* ...) ,tbody))])
    )
  )
