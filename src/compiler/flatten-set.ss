(library (compiler flatten-set)
  (export flatten-set!)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass flatten-set! : L15 (x) -> L16 ()
    (trivialize : Value (v x) -> Effect ()
		[(if ,[p0] ,[e1] ,[e2]) `(if ,p0 ,e1 ,e2)]
		[(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
		[(,binop ,triv1 ,triv2) `(set! ,x (,binop ,triv1 ,triv2))]
		[(call ,triv ,triv* ...) `(set! ,x (call ,triv ,triv* ...))]
		[(mref ,triv1 ,triv2) `(set! ,x (mref ,triv1 ,triv2))]
		[(alloc ,triv) `(set! ,x (alloc ,triv))]
		[,triv `(set! ,x ,triv)]
		[else (error who "unrecognized Value  ̃s" v)])
    (Effect : Effect (x) -> Effect ()
	    [(set! ,x ,v) (trivialize v x)])))
