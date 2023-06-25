(library (compiler select-instructions)
  (export select-instructions)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))


  (define-pass select-instructions : L20 (x) -> L20 ()
    (definitions
      (define relop^
	(lambda (op)
	  (case op
	    ['> '<]
	    ['< '>]
	    ['<= '>=]
	    ['= '=])))
      (define (ur? x) (or (register? x) (uvar? x)))
      (define new-ulocal* '())
      (define int64-or-label?
	(lambda (x) (or (and (not (int32? x)) (int64? x)) (label? x))))
      (define (new-u)
	(let ([u (unique-name 't)])
          (set! new-ulocal* (cons u new-ulocal*))
          u)))
    (Effect : Effect (x) -> Effect ()
	    (definitions
	      (define (select-binop-1 var binop triv1 triv2)
		(cond
		 [(eq? var triv1) (select-binop-2 binop var triv2)]
		 [(and (eq? var triv2) (member binop '(+ * logor logand))) (select-binop-2 binop var triv1)]
		 [else (let ([u (new-u)])
			 `(begin (set! ,u ,triv1) ,(select-binop-2 binop u triv2) (set! ,var ,u)))]))
	      (define (select-binop-2 binop var triv) 
		(cond
		 [(and (member binop '(- + sra logor logand))
		       (or (int64-or-label? triv) (and (frame-var? var) (frame-var? triv))))
         	  (let ([u (new-u)])
         	    `(begin (set! ,u ,triv) (set! ,var (,binop ,var ,u))))]
        ;;; X2
		 [(and (eq? binop '*) (frame-var? var))
		  (let ([u (new-u)])
                    `(begin (set! ,u ,var) ,(select-binop-2 binop u triv) (set! ,var ,u)))]
        ;;; X1 for *
		 [(and (eq? binop '*) (ur? var) (int64-or-label? triv))
         	  (let ([u (new-u)])
         	    `(begin (set! ,u ,triv) (set! ,var (,binop ,var ,u))))]
		 [else `(set! ,var (,binop ,var ,triv))]))
	      (define (select-move var triv)
		(if (and (frame-var? var) (or (frame-var? triv) (int64-or-label? triv)))
			            ;;; X0
		    (let ([u (new-u)])
		      `(begin (set! ,u ,triv) (set! ,var ,u)))
		    `(set! ,var ,triv))))
	    [(set! ,var (,binop ,triv1 ,triv2)) (select-binop-1 var binop triv1 triv2)]
            [(set! ,var ,triv) (select-move var triv)])
    (Pred : Pred (x) -> Pred ()
	  (definitions
	    (define (select-relop relop x y) 
	      (cond
	       [(and (int32? x) (or (ur? y) (frame-var? y))) `(,(relop^ relop) ,y ,x)]
	       [(or (and (frame-var? x) (frame-var? y))
		    (and (int32? x) (int32? y))
		    (and (int64-or-label? x) (or (ur? y) (frame-var? y) (int32? y))))
		(let ([u (new-u)])
		  `(begin (set! ,u ,x) (,relop ,u ,y)))]
	       [(and (or (ur? x) (frame-var? x) (int32? x))
		     (int64-or-label? y))
		(let ([u (new-u)])
		  `(begin (set! ,u ,y) (,(relop^ relop) ,u ,x)))]
	       [(and (int64-or-label? x) (int64-or-label? y))
		(let ([u1 (new-u)] [u2 (new-u)])
		  `(begin (set! ,u1 ,x) (set! ,u2 ,y) (,relop ,u1 ,u2)))]
	       [else `(,relop ,x ,y)]))))
    (ULocalBody : ULocalBody (x) -> ULocalBody ()
		[(ulocals (,ulocal* ...) ,[locbody])
		 `(ulocals (,ulocal* ... ,new-ulocal* ...) ,locbody)])))
