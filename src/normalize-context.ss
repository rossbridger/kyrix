(library (normalize-context)
  (export normalize-context)

  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass normalize-context : L10 (x) -> L11 ()
    (Value : Expr (x) -> Value ()
	   [(let ([,x* ,[v*]] ...) ,[vbody]) `(let ([,x* ,v*] ...) ,vbody)]
	   [(begin ,[e*] ... ,[v]) `(begin ,e* ... ,v)]
	   [(,pr ,[v*] ...) (guard (value-primitive? pr))
	    `(,pr ,v* ...)]
	   [(,pr ,[v*] ...) (guard (predicate-primitive? pr))
	    `(if (,pr ,v* ...) '#t '#f)]
	   [(,pr ,[v*] ...) (guard (effect-primitive? pr))
	    `(begin (,pr ,v* ...) `(void))]
	   [(,[v] ,[v*] ...) `(call ,v ,v* ...)])
    (Effect : Expr (x) -> Effect ()
	    [',c `(nop)]
	    [(if ,[p0] ,[e1] ,[e2]) `(if ,p0 ,e1 ,e2)]
	    [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
	    [(let ([,x* ,[v*]] ...) ,[ebody])
	     `(let ([,x* ,v*] ...) ,ebody)]
	    [(,pr ,[v*] ...) (guard (effect-primitive? pr))
	     `(,pr ,v* ...)]
	    [(,pr ,[v*] ...) `(nop)]
	    [(,[v] ,[v*] ...) `(call ,v ,v* ...)])
    (Pred : Expr (x) -> Pred ()
	  [',c (if c `(true) `(false))]
	  [(begin ,[e*] ... ,[p]) `(begin ,e* ... ,p)]
	  [(let ([,x* ,[v*]] ...) ,[pbody]) `(let ([,x* ,v*] ...) ,pbody)]
	  [,x `(if (eq? ,x '#f) (false) (true))]
	  [,l `(true)]
	  [(,pr ,[v*] ...) (guard (predicate-primitive? pr))
	   `(,pr ,v* ...)]
	  [(,pr ,[v*] ...) (guard (effect-primitive? pr))
	   `(begin (,pr ,v* ...) (true))]
	  [(,pr ,[v*] ...) (guard (value-primitive? pr))
	   `(if (eq? (,pr ,v* ...) '#f) (false) (true))]
	  [(,[v] ,[v*] ...) `(if (eq? (call ,v ,v* ...) '#f) (false) (true))])
    (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		[(lambda (,x* ...) ,[vbody])
		 `(lambda (,x* ...) ,vbody)])
    (Program : Program (x) -> Program ()
	     [(letrec ([,l* ,[le*]] ...) ,[vbody])
	      `(letrec ([,l* ,le*] ...) ,vbody)]))
  )
