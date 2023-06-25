(library (compiler pre-assign-frame)
  (export pre-assign-frame)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  ;; too tired to devise this pass, so i just copied.
  (define-pass pre-assign-frame : L18 (x) -> L19 ()
    (definitions
      (define find-used
	(lambda (conflict* home*)
	  (cond
	   [(null? conflict*) '()]
	   [(frame-var? (car conflict*)) 
	    (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
	   [(assq (car conflict*) home*) => 
	    (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
	   [else (find-used (cdr conflict*) home*)])))
      (define find-frame-var
	(lambda (used*)
	  (let f ([index 0])
	    (let ([fv (index->frame-var index)])
	      (if (memq fv used*) (f (+ index 1)) fv)))))
      (define find-homes
	(lambda (var* ct home*)
	  (if (null? var*)
	      home*
	      (let ([var (car var*)] [var* (cdr var*)])
	        (let ([conflict* (cdr (assq var ct))])
	          (let ([home (find-frame-var (find-used conflict* home*))])
	            (find-homes var* ct `((,var ,home) . ,home*)))))))))
    (SpillsBody : SpillsBody (x) -> LocateBody ()
		[(spills (,spill* ...) (frame-conflict ((,x* ,var** ...) ...) ,[lbody]))
		 (let ([home* (find-homes spill* (map cons x* var**) '())])
		   `(locate ([,(map car home*) ,(map cdr home*)] ...)
			    (frame-conflict ((,x* ,var** ...) ...) ,lbody)))])))
