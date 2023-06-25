(library (impose-calling-conventions)
  (export impose-calling-conventions)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass impose-calling-conventions : L16 (x) -> L17 ()
    (definitions
      (define new-frame-var** '()) ;; good thing the compiler is not parallel (yet?)
      (define caller-saved-registers registers)
      (define parameter-registers '(r8 r9))
      (define frame-pointer-register 'rbp)
      (define return-value-register 'rax)
      (define return-address-register 'r15)
      (define (locate-arguments x*)
	(let loop ([alist '()] [reg parameter-registers] [x* x*])
	  (cond
	   [(null? x*) alist]
	   [(null? reg) (loop (cons (unique-name 'nfv) alist) reg (cdr x*))]
	   [else (loop (cons (car parameter-registers) alist) (cdr reg) (cdr x*))]))))
    (Tail : Tail (x rp) -> Tail ()
	  [(call ,triv ,triv* ...)
	   (let ([param* (locate-arguments triv*)])
	     `(begin
		(set! ,param* ,triv*) ...
		(set! ,return-address-register ,rp)
		(call ,triv frame-pointer-register ,return-address-register ,param* ...)))]
	  [,triv
	   `(begin
	      (set! ,return-value-register ,triv)
	      (call ,rp ,frame-pointer-register ,return-value-register))]
	  [(,binop ,triv1 ,triv2) ;; this feels redundant.
	   `(begin
	      (set! ,return-value-register (,binop ,triv1 ,triv2))
	      (call ,rp ,frame-pointer-register ,return-value-register))]
	  [(alloc ,triv)
	   `(begin
	      (set! ,return-value-register (alloc ,triv))
	      (call ,rp ,frame-pointer-register ,return-value-register))]
	  [(mref ,triv1 ,triv2)
	   `(begin
	      (set! ,return-value-register (mref ,triv1 ,triv2))
	      (call ,rp ,frame-pointer-register ,return-value-register))]
	  
	  )
    (Effect : Effect (x) -> Effect ()
	    (definitions
	      (define (generate-nontail-call rator rand*)
		(let ([rp-label (unique-label 'rp)]
		      [param* (locate-arguments rand*)])
		  (set! new-frame-var** (cons (filter uvar? param*) new-frame-var**))
		  `(return-point ,rp-label
				 (begin
				   (set! ,param* ,rand*) ...
				   (set! ,return-address-register ,rp-label)
				   (call ,rator ,frame-pointer-register ,return-address-register ,param* ...))))))
	    [(set! ,x (call ,triv ,triv* ...))
	     `(begin ,(generate-nontail-call triv triv*)
		     (set! ,x ,return-value-register))]
	    [(set! ,x ,rhs) `(set! ,x ,rhs)]
	    [(call ,triv ,triv* ...) (generate-nontail-call triv triv*)])
    (Body : Body (x [fml* '()]) -> Body ()
	  (definitions
	    (define (locate-parameters fml*)
	      (let loop ([loc '()] [fml* fml*] [reg* parameter-registers] [i 0])
		(cond
		 [(null? fml*) loc]
		 [(null? reg*) (loop (cons (index->frame-var i) loc) (cdr fml*) reg* (+ i 1))]
		 [else (loop (cons (car reg*) loc) (cdr fml*) (cdr reg*) i)]))))
	  [(locals (,x* ...) ,tbody)
	   (let ([rp (unique-name 'rp)])
	     (set! new-frame-var** '())
	     (set! tbody (Tail tbody rp))
	     `(locals
	       (,x* ... ,rp ,fml* ... ,(map append new-frame-var**) ...)
	       (new-frames
		((,new-frame-var** ...) ...)
		(begin
		  (set! ,rp ,return-address-register)
		  (set! ,fml* ,(locate-parameters fml*)) ...
		  ,tbody))))])
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x* ...) ,body)
		 `(lambda () ,(Body body x*))])))
