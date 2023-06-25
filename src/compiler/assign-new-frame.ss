(library (compiler assign-new-frame)
  (export assign-new-frame)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass assign-new-frame : L19 (x) -> L20 ()
    (definitions
      (define frame-pointer-register 'rbp)
      
      (define align-shift 3) ; 8-byte words
      (define find-max
	;;This function is used to determine the size of the frame used by the function
	(lambda (ls)
	  (cond
	   [(null? ls) '-1]
	   [else (max (car ls) (find-max (cdr ls)))])))
      (define frame-size 
	(lambda (call-live* home*)
	  (let ([ls (map (lambda (x)
			   (if (frame-var? x) 
			       (frame-var->index x)
			       (frame-var->index (cadr (assq x home*))))) call-live*)])
	    (add1 (find-max ls))))))
    (Tail : Tail (x fs) -> Tail ())
    (Pred : Pred (x fs) -> Pred ())
    (Effect : Effect (x fs) -> Effect ()
	    [(return-point ,l ,[tbody])
	     `(begin
		(set! ,frame-pointer-register (+ ,frame-pointer-register ,(ash fs align-shift)))
		(return-point ,l ,tbody)
		(set! ,frame-pointer-register (- ,frame-pointer-register ,(ash fs align-shift))))])
    (Body : Body (x) -> Body ()
	  [(locals (,x* ...)
		   (new-frames ((,nfv** ...) ...)
			       (locate ([,y* ,fv*] ...)
				       (frame-conflict ((,z* ,var** ...) ...)
						       (call-live (,live* ...) ,[tbody])))))
	   (let ([fs (frame-size live* (map cons y* fv*))])
	     (define (do-assign var*)
	       (let f ([index fs] [ls var*] [rs '()])
		 (let ((fv (index->frame-var index)))
		   (cond
		    [(null? ls) rs]
		    [else (f (add1 index) (cdr ls) (cons (list (car ls) fv) rs))]))))
	     (let* ([rs* (apply append (map do-assign nfv**))]
		    [y* (append y* (map car rs*))]
		    [fv* (append fv* (map cadr rs*))])
	       `(locals (,(difference y* (apply append nfv**)) ...)
			(ulocals ()
				 (locate ([,y* ,fv*] ...)
					 (frame-conflict ((,z* ,var** ...) ...) ,tbody))))))])))
