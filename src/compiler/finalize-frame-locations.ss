(library (compiler finalize-frame-locations)
  (export finalize-frame-locations)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass finalize-frame-locations : L20 (x) -> L20 ()
    (Variable : Variable (x env) -> Variable ()
	      [,x (cond
		   [(assq x env) => cdr]
		   [else x])]
	      [else x])
    (Triv : Triv (x env) -> Triv ())
    (Pred : Pred (x env) -> Pred ())
    (Rhs : Rhs (x env) -> Rhs ())
    (Effect : Effect (x env) -> Effect ())
    (Tail : Tail (x env) -> Tail ())
    (LocateBody : LocateBody (x) -> LocateBody ()
		[(locate ([,x* ,fv*] ...)
			 (frame-conflict ([,y* ,var** ...] ...) ,tbody))
		 (let ([env (map cons x* fv*)])
		   `(locate ([,x* ,fv*] ...)
			    (frame-conflict ([,y* ,var** ...] ...) ,(Tail tbody env))))])))
