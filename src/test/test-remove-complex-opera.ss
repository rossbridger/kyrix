;; Run from the repository root:
;;   scheme --libdirs src --script src/test/test-remove-complex-opera.ss
(import (chezscheme)
	(nanopass)
	(compiler helpers)
	(compiler ir)
	(compiler remove-complex-opera)
	(compiler flatten-set))

(define-parser parse-L14 L14)

;; Evaluates an unparsed L14/L15/L16 program directly in Chez Scheme,
;; returning its value together with a snapshot of the heap.
(define (run prog)
  (let ([env (copy-environment (scheme-environment))])
    (for-each (lambda (d) (eval d env))
	      '((define mem (make-eqv-hashtable))
		(define ap 0)
		(define (alloc n) (let ([a ap]) (set! ap (+ ap n)) a))
		(define (mref b o) (hashtable-ref mem (+ b o) 'uninitialized))
		(define (mset! b o v) (hashtable-set! mem (+ b o) v))
		(define (sra x n) (ash x (- n)))
		(define (nop) (void))
		(define (true) #t)
		(define (false) #f)
		(define-syntax call
		  (syntax-rules () [(_ f a ...) (f a ...)]))
		(define-syntax locals
		  (syntax-rules () [(_ (x ...) body) (let ([x 'unset] ...) body)]))))
    (let ([v (eval prog env)])
      (let-values ([(k* v*) (hashtable-entries (eval 'mem env))])
	(cons v (sort (lambda (a b) (< (car a) (car b)))
		      (map cons (vector->list k*) (vector->list v*))))))))

;; Every variable assigned by set! must be a formal or listed in locals.
(define (undeclared-assignments prog)
  (define bad '())
  (define (walk x bound)
    (when (pair? x)
      (case (car x)
	[(lambda) (walk (caddr x) (append (cadr x) bound))]
	[(locals) (walk (caddr x) (append (cadr x) bound))]
	[(set!) (unless (memq (cadr x) bound) (set! bad (cons (cadr x) bad)))
	 (walk (caddr x) bound)]
	[else (for-each (lambda (y) (walk y bound)) x)])))
  (walk prog '())
  bad)

(define (operand-ok? x) (or (symbol? x) (integer? x)))

;; Every operand of alloc/mref/mset!/binop/relop/call must be a Triv.
(define (complex-operands prog)
  (define bad '())
  (define (walk x)
    (when (pair? x)
      (case (car x)
	[(alloc mref mset! call + - * logand logor sra < <= = >= >)
	 (for-each (lambda (y) (unless (operand-ok? y) (set! bad (cons x bad))))
		   (cdr x))]
	[else (void)])
      (for-each walk x)))
  (walk prog)
  bad)

(define failures 0)

(define (check name src expected)
  (let* ([in (parse-L14 src)]
	 [out (remove-complex-opera* in)]
	 [out-sexp (unparse-L15 out #t)]
	 [flat-sexp (unparse-L16 (flatten-set! out) #t)]
	 [ref (run (unparse-L14 in #t))])
    (define (fail msg . args)
      (set! failures (+ failures 1))
      (printf "FAIL ~a: ~?\n  output: ~s\n" name msg args out-sexp))
    (cond
     [(not (equal? (car ref) expected))
      (fail "input evaluates to ~s, expected ~s" (car ref) expected)]
     [(pair? (complex-operands out-sexp))
      (fail "complex operands remain: ~s" (complex-operands out-sexp))]
     [(pair? (undeclared-assignments out-sexp))
      (fail "undeclared temporaries: ~s" (undeclared-assignments out-sexp))]
     [(not (equal? (run out-sexp) ref))
      (fail "output evaluates to ~s, input to ~s" (run out-sexp) ref)]
     [(not (equal? (run flat-sexp) ref))
      (fail "flatten-set! output evaluates to ~s, input to ~s" (run flat-sexp) ref)]
     [else (printf "ok   ~a\n" name)])))

(define (check-unchanged name src)
  (let ([out (unparse-L15 (remove-complex-opera* (parse-L14 src)) #t)])
    (if (equal? out src)
	(printf "ok   ~a\n" name)
	(begin
	  (set! failures (+ failures 1))
	  (printf "FAIL ~a: expected unchanged\n  output: ~s\n" name out)))))

(check-unchanged "trivial operands are left alone"
  '(letrec ([f$1 (lambda (a.1 b.2) (locals () (+ a.1 b.2)))])
     (locals (x.3)
       (begin
	 (set! x.3 (alloc 16))
	 (mset! x.3 0 x.3)
	 (if (< x.3 8) (call f$1 x.3 3) (mref x.3 0))))))

(check-unchanged "operand (begin e ... triv) needs no temporary"
  '(letrec ()
     (locals (x.1 y.2)
       (begin
	 (set! y.2 1)
	 (begin (set! x.1 2) (+ y.2 x.1))))))

(check "variable operand assigned by a later operand"
  '(letrec ()
     (locals (x.1)
       (begin
	 (set! x.1 1)
	 (+ x.1 (begin (set! x.1 2) x.1)))))
  3)

(check "nested binops"
  '(letrec () (locals () (+ (+ 1 2) (* (- 10 6) 3))))
  15)

(check "left-to-right order with side effects"
  '(letrec ()
     (locals (p.1)
       (begin
	 (set! p.1 (alloc 16))
	 (mset! p.1 0 7)
	 (+ (mref p.1 0) (begin (mset! p.1 0 10) (mref p.1 0))))))
  17)

(check "complex operands in calls and labels"
  '(letrec ([f$1 (lambda (a.2 b.3) (locals () (- a.2 b.3)))])
     (locals (x.4)
       (begin
	 (set! x.4 (if (< (+ 1 1) 3) 10 20))
	 (call f$1 (call f$1 x.4 (+ 1 2)) (if (true) 5 6)))))
  2)

(check "complex operator position"
  '(letrec ([f$1 (lambda (a.1) (locals () (+ a.1 1)))]
	    [g$2 (lambda (a.2) (locals () (- a.2 1)))])
     (locals () (call (if (false) f$1 g$2) (call f$1 (* 2 3)))))
  6)

(check "relop operands and mset! operands"
  '(letrec ([id$1 (lambda (a.1) (locals () a.1))])
     (locals (v.2)
       (begin
	 (set! v.2 (+ (alloc 24) 3))
	 (mset! (- v.2 3) (+ 0 0) 4)
	 (mset! (- v.2 3) (+ 2 (* 3 2)) (if (> (mref v.2 -3) 0) 1 2))
	 (mset! v.2 (- 16 3) (call (begin (nop) (if (true) id$1 v.2)) (mref v.2 5)))
	 (if (= (mref v.2 13) (+ (begin (mset! v.2 13 0) 0) 1))
	     (mref (- v.2 3) (+ 8 8))
	     99))))
  0)

(check "effect-position calls and nested lambdas"
  '(letrec ([h$1 (lambda (p.1 n.2)
		   (locals (t.3)
		     (begin
		       (mset! p.1 (* n.2 8) (+ n.2 (mref p.1 0)))
		       (if (<= n.2 0)
			   (mref p.1 0)
			   (begin
			     (set! t.3 (call h$1 p.1 (- n.2 1)))
			     (+ t.3 (mref p.1 (* n.2 8))))))))])
     (locals (q.4)
       (begin
	 (set! q.4 (alloc 64))
	 (mset! q.4 0 1)
	 (call h$1 q.4 (begin (call h$1 q.4 (+ 1 1)) (- 4 1))))))
  10)

(if (zero? failures)
    (printf "all tests passed\n")
    (begin (printf "~a test(s) failed\n" failures) (exit 1)))
