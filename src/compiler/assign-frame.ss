(library (compiler assign-frame)
  (export assign-frame)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass assign-frame : L22 (x) -> L20 ()
    (definitions
      (define find-used
	(lambda (conflict* home*)
	  (cond
           [(null? conflict*) '()]
           [(frame-var? (car conflict*)) 
            (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
           [(assq (car conflict*) home*) => 
            (lambda (x) (set-cons (cdr x) (find-used (cdr conflict*) home*)))]
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
                    (find-homes var* ct (cons (cons var home) home*)))))))))
    (Body : Body (x) -> Body ()
	  [(locals (,local* ...)
		   (ulocals (,ulocal* ...)
			    (spills (,spill* ...)
				    (locate ([,x* ,fv*] ...)
					    (frame-conflict ([,y* ,var** ...] ...) ,[tbody])))))
           (let ([home* (find-homes spill* (map cons y* var**) (map cons x* fv*))])
             `(locals (,local* ...)
                      (ulocals (,ulocal* ...)
			       (locate ([,(map car home*) ,(map cdr home*)] ...)
				       (frame-conflict ([,y* ,var** ...] ...) ,tbody)))))]))

  )
