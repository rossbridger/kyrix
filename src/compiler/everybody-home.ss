(library (compiler everybody-home)
  (export everybody-home?)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass everybody-home? : L22 (x) -> * (bool)
    (Body : Body (x) -> * (bool)
	  [(locate ([,x* ,loc*] ...) ,tbody) #t]
	  [else #f])
    (Program : Program (x) -> * (bool)
	     [(letrec ([,l* (lambda () ,body*)] ...) ,body)
              (andmap Body (cons body body*))]
	     [else (error who "program with unexpected shape  ̃ s" x)])
    (Program x)))
