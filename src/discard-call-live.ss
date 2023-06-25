(library (discard-call-live)
  (export discard-call-live)
  (import (chezscheme)
	  (nanopass)
	  (helpers)
	  (ir))

  (define-pass discard-call-live : L22 (x) -> L23 ()
    (Body : Body (x) -> Body ()
	  [(locate ([,x* ,loc*] ...) ,[tbody])
	   `(locate ([,x* ,loc*] ...) ,tbody)]
	  [else (error who "invalid body ~s" x)])
    (Tail : Tail (x) -> Tail ()
	  [(call ,triv ,loc* ...) `(,triv)])))
