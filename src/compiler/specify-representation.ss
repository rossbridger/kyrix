(library (compiler specify-representation)
  (export specify-representation)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass %mark-tail-position : L12a (x) -> L12a ()
    (Tail : Value (x) -> Tail ()
	  [,triv triv]
	  [(mref ,v1 ,v2) `(mref ,v1 ,v2)]
	  [(call ,v ,v* ...) `(call ,v ,v* ...)]
	  [(,binop ,v1 ,v2) `(,binop ,v1 ,v2)]
	  [(alloc ,v) `(alloc ,v)]
	  [(if ,p0 ,[t1] ,[t2]) `(if ,p0 ,t1 ,t2)]
	  [(begin ,e* ... ,[tbody]) `(begin ,e* ... ,tbody)]
	  [(let ([,x* ,v*] ...) ,[tbody]) `(let ([,x* ,v*] ...) ,tbody)])
    (Tail x))
  
  (define-pass specify-representation : L11a (x) -> L12a ()
    (definitions
      (define sra (lambda (x n) (ash x (- n))))
      ;; definition for the binary rep of constants.
      (define fixnum-bits 61)
      (define shift-fixnum 3)
      (define mask-fixnum #b111)
      (define tag-fixnum #b000)

      (define mask-pair #b111)
      (define tag-pair #b001)
      (define size-pair 16)
      (define disp-car 0)
      (define disp-cdr 8)

      (define mask-vector #b111)
      (define tag-vector  #b011)
      (define disp-vector-length 0)
      (define disp-vector-data 8)

      (define mask-procedure #b111)
      (define tag-procedure  #b010)
      (define disp-procedure-code 0)
      (define disp-procedure-data 8)

      (define mask-boolean   #b11110111)
      (define tag-boolean    #b00000110)

      (define $false         #b00000110)
      (define $true          #b00001110)
      (define $nil           #b00010110)
      (define $void          #b00011110)

      (define value-prims
	'((* . 2) (+ . 2) (- . 2) (car . 1) (cdr . 1) (cons . 2)
	  (make-vector . 1) (vector-length . 1) (vector-ref . 2)
	  (void . 0)))
      (define pred-prims
	'((< . 2) (<= . 2) (= . 2) (>= . 2) (> . 2) (boolean? . 1)
	  (eq? . 2) (fixnum? . 1) (null? . 1) (pair? . 1)
	  (vector? . 1)))
      (define effect-prims '((set-car! . 2) (set-cdr! . 2) (vector-set! . 3)))
      (define (convert-constant c)
	(cond [(null? c) $nil]
	      [(eq? c #t) $true]
	      [(eq? c #f) $false]
	      [(fixnum? c)
	       (ash c shift-fixnum)])))
    (Pred : Pred (x) -> Pred ()
	  (definitions
	    (define (convert-pred-prims pr v*)
	      (case pr
		[(< <= = >= >) `(,pr ,(car v*) ,(cadr v*))]
		[(eq?) `(= ,(car v*) ,(cadr v*))]
		[(boolean?) `(= (logand ,(car v*) ,mask-boolean) ,tag-boolean)]
		[(vector?)
		 `(= (logand ,(car v*) ,mask-vector) ,tag-vector)]
		[(fixnum?)
		 `(= (logand ,(car v*) ,mask-fixnum) ,tag-fixnum)]
		[(null?) (`(= ,(car v*) $nil))]
		[(pair?)
		 `(= (logand ,(car v*) ,mask-pair) ,tag-pair)]
		[else (error who "Unknown pred prim ~a" pr)])))
	  [(,pred-pr ,[v*] ...)
	   (let ([arity (cdr (assq pred-pr pred-prims))])
	     (if (eq? (length v*) arity)
		 (convert-pred-prims pred-pr v*)
		 (error who "~a: arity mismatch: expected ~a, got ~a" pred-pr arity v*)))])
    (Effect : Effect (x) -> Effect ()
	    (definitions
	      (define (convert-effect-prims pr v*)
		(case pr
		  [(set-car!) `(mset! ,(car v*) ,(- disp-car tag-pair) ,(cadr v*))]
		  [(set-cdr!) `(mset! ,(car v*) ,(- disp-cdr tag-pair) ,(cadr v*))]
		  [(vector-set!)
		   (if (integer? (cadr v*))
		       `(mset! ,(car v*) ,(+ (- disp-vector-data tag-vector) (cadr v*)) ,(caddr v*))
		       `(mset! ,(car v*) (+ ,(- disp-vector-data tag-vector) ,(cadr v*)) ,(caddr v*)))]
		  [else (error who "Unknown effect prim ~a" pr)])))
	    [(,ef-pr ,[v*] ...)
	     (let ([arity (cdr (assq ef-pr effect-prims))])
	       (if (eq? (length v*) arity)
		   (convert-effect-prims ef-pr v*)
		   (error who "~a: arity mismatch: expected ~a, got ~a" ef-pr arity v*)))])
    (Value : Value (x) -> Value ()
	   (definitions
	     (define (convert-value-prims pr v*)
	       (case pr
		 [(void) $void]
		 [(+ -) `(,pr ,(car v*) ,(cadr v*))]
		 [(*) (let ([a (car v*)]
			    [b (cadr v*)])
			(cond
			 [(integer? a) `(* ,b ,(sra a 3))]
			 [(integer? b) `(* ,a ,(sra b 3))]
			 [else `(* ,a (sra ,b 3))]))]
		 [(cons) (let ([first (unique-name 't)] [last (unique-name 't)] [pair (unique-name 't)])
			   `(let ([,first ,(car v*)] [,last ,(cadr v*)])
			      (let ([,pair (+ (alloc ,size-pair) ,tag-pair)])
				(begin
				  (mset! ,pair ,(- disp-car tag-pair) ,first)
				  (mset! ,pair ,(- disp-cdr tag-pair) ,last)
				  ,pair))))]
		 [(car) `(mref ,(car v*) ,(- disp-car tag-pair))]
		 [(cdr) `(mref ,(car v*) ,(- disp-cdr tag-pair))]
		 [(make-vector) (let ([size-var (unique-name 't)] [size (car v*)])
				  `(let ([,size-var (+ (alloc ,(if (integer? size) (+ disp-vector-data size) `(+ ,disp-vector-data ,size))) ,tag-vector)])
				     (begin
				       (mset! ,size-var ,(- disp-vector-length tag-vector) ,size)
				       ,size-var)))]
		 [(vector-length) `(mref ,(car v*) ,(- disp-vector-length tag-vector))]
		 [(vector-ref) (let ([value (cadr v*)])
				 (if (integer? value) 
				     `(mref ,(car v*) ,(+ (- disp-vector-data tag-vector) value))
				     `(mref ,(car v*) (+ ,(- disp-vector-data tag-vector) ,value))))]
		 [else (error who "Unknown value prim ~a" pr)])))
	   [(quote ,c) (convert-constant c)]
	   [(,val-pr ,[v*] ...)
	    (let ([arity (cdr (assq val-pr value-prims))])
	      (if (eq? (length v*) arity)
		  (convert-value-prims val-pr v*)
		  (error who "~a: arity mismatch: expected ~a, got ~a" val-pr arity v*)))])
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x* ...) ,[vbody]) `(lambda (,x* ...) ,(%mark-tail-position vbody))])
    (Program : Program (x) -> Program ()
	     [(letrec ([,l* ,[le*]] ...) ,[vbody]) `(letrec ([,l* ,le*] ...) ,(%mark-tail-position vbody))]))
)
