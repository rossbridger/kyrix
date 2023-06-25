(library (compiler assign-registers)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass assign-registers : L21 (x) -> L22 ()
    (definitions
      (define remove-occurence
	;;Removes the occurence of a var from var* and returns the list
	(lambda (var ct)
	  (map (lambda (x) 
			(cond
			 [(eq? (car x) var) x]
			 [else (remq var x)])) ct)))
	     (define replace
	       ;;Replaces the occurences of variables in the conflict-list with the register-homes
	       (lambda (allocations ct)
		 (cond 
		  [(null? allocations) ct]
		  [else (replace (cdr allocations) (replace-helper (car allocations) ct))])))
	     (define replace-helper
	       (lambda (allocation ct)
		 (map (lambda (ct-entry)
			(cond
			 [(eq? (car allocation) (car ct-entry)) allocation]
			 [else (cons (car ct-entry) (replace* (cdr ct-entry) allocation))])) ct)))
	     (define replace*
	       (lambda (conflicts allocation)
		 (cond
		  [(null? conflicts) '()]
		  [(eq? (car conflicts) (car allocation)) (cons (cadr allocation) (replace* (cdr conflicts) allocation))]
		  [else (cons (car conflicts) (replace* (cdr conflicts) allocation))])))			
	     (define k (length registers))
	     (define low-degree?
	       (lambda (var ct)
		 (< (length (cdr (assq var ct))) k)))
	     (define num-conflicts
	       (lambda (var ct)
		 (length (cdr (assq var ct)))))
	     (define pick-min
	       ;;Picks a node with least number of conflicts like the min function
	       (lambda (var degree var* ct)
		 (cond
		  [(null? var*) var]
		  [(<= degree (num-conflicts (car var*) ct)) (pick-min var degree (cdr var*) ct)]
		  [else (let* ((node (car var*))
			       (degree^ (num-conflicts node ct)))
			  (pick-min node degree^ (cdr var*) ct))])))
	     (define find-homes
	       (lambda (var* ct)
		 (cond
		  [(null? var*) '()]
		  [else (let* ((current-var (pick-min (car var*) (num-conflicts (car var*) ct) (cdr var*) ct))
			       (new-conflict-table (remove-occurence current-var ct))
			       (results (find-homes (remq current-var var*) new-conflict-table))
			       (updated-ct (replace results ct))
			       (conflict-entry (cdr (assq current-var updated-ct)))
			       (remaining-registers (difference registers conflict-entry)))
			  (if (null? remaining-registers) 
			      results 
			      (let ((assign-register (car remaining-registers)))
				(cons (list current-var assign-register) results))))])))
	     (define get-replacement
	       (lambda (var entry)
		 (list var (car (difference registers entry))))))
	   (Body : Body (x) -> Body ()
		 [(locals (,local* ...) 
			  (ulocals (,ulocal* ...)
				   (locate ([,x* ,fv*] ...)
					   (frame-conflict ([,y* ,var** ...] ...)
							   (register-conflict ([,z* ,ur** ...] ...) ,[tbody])))))
		  (let ([uvar* (append local* ulocal*)])
		    (let ([home* (find-homes uvar* (map cons x* ur**))])
		      (let ([spill* (difference uvar* (map car home*))])
			(cond
			 [(null? spill*) `(locate ([,x* ,loc*] ... [,(map car home*) ,(map cadr home*)] ...)
						  ,tbody)]
			 [(null? (intersection ulocal* spill*))
			  (let ([local* (difference local* spill*)])
			    `(locals (,local* ...)
				     (ulocals (,ulocal* ...)
					      (spills (,spill* ...)
						      (locate ([,x* ,loc*] ...)
							      (frame-conflict ([,y* ,var** ...] ...) ,tbody))))))]
			 [else 
			  (error who "unspillable variables (~s) have been spilled"
				 (difference spill* local*))]))))]))
	 )
