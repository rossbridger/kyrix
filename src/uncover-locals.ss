(library (uncover-locals)
  (export uncover-locals)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass collect-locals : L13 (x) -> * (local*)
    (definitions
      (define local* '()))
    (Pred : Pred (x) -> * (local*)
	  [(let ([,x* ,v*] ...) ,pbody) (set! local* (append x* local*))]
	  [else (void)])
    (Effect : Effect (x) -> * (local*)
	    [(let ([,x* ,v*] ...) ,ebody) (set! local* (append x* local*))]
	    [else (void)])
    (Value : Value (x) -> * (local*)
	   [(let ([,x* ,v*] ...) ,vbody) (set! local* (append x* local*))]
	   [else (void)])
    (Tail : Tail (x) -> * (local*)
	  [(let ([,x* ,v*] ...) ,tbody) (set! local* (append x* local*))]
	  [else (void)])
    (Tail x)
    local*)

  (define-pass uncover-locals : L12 (x) -> L13 ()
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x* ...) ,[tbody]) `(lambda (,x* ...) (locals (,(collect-locals tbody) ...) ,tbody))])
    (Program : Program (x) -> Program ()
	     [(letrec ([,l* ,[le*]] ...) ,[tbody]) `(letrec ([,l* ,le*] ...) (locals (,(collect-locals tbody) ...) ,tbody))])))
