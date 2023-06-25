(library (compiler flatten-program)
  (export flatten-program)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass flatten-program : L27 (x) -> L28 ()
    (definitions
      (define stmt-stack* '())
      (define push-stmt!
	(lambda (stmt)
	  (set! stmt-stack*
		(cons stmt stmt-stack*))))
      (define (pop-stmt)
	(reverse stmt-stack*)))
    (Effect : Effect (x) -> Statement ())
    (Tail : Tail (x) -> Statement ()
	  [(if (,relop ,[triv1] ,[triv2]) (,l1) (,l2))
	   (push-stmt! `(if (,relop ,triv1 ,triv2) (jump ,l1)))
	   (push-stmt! `(jump ,l2))]
	  [(,triv) (push-stmt! `(jump ,triv))]
	  [(begin ,e* ... ,tbody)
	   (for-each (lambda (e) (push-stmt! (Effect e))) e*)
	   (Tail tbody)])
    (Program : Program (prog) -> Program ()
	     [(letrec ([,l* (lambda () ,tbody*)] ...) ,tbody)
	      (Tail tbody)
	      (for-each (lambda (l tbody)
			  (push-stmt! l)
			  (Tail tbody)) l* tbody*)
	      `(code ,(pop-stmt) ...)])
    )
  )
