(library (compiler generate-x86-64)
  (export generate-x86-64)
  (import (chezscheme)
	  (nanopass)
	  (compiler helpers)
	  (compiler ir))

  (define-pass generate-x86-64 : L28 (x) -> * ()
    (definitions
      (define rand->x86-64-arg
	(lambda (rand)
	  (cond
	   [(string? rand) rand] ; precooked operand
	   [(number? rand)  (format "$~s" rand)]
	   [(register? rand)  (format "%~s" rand)]
	   [(label? rand) (format "~a(%rip)" (label->x86-64-label rand))]
	   [else (error who "invalid instruction argument ~s" rand)])))

      (define-syntax emit
	(syntax-rules ()
	  [(_ opcode) (printf "    ~a\n" opcode)]
	  [(_ opcode rand)
	   (printf "    ~a ~a\n" opcode (rand->x86-64-arg rand))]
	  [(_ opcode rand1 rand2)
	   (printf "    ~a ~a, ~a\n" opcode
		   (rand->x86-64-arg rand1)
		   (rand->x86-64-arg rand2))]))

      (define emit-label
	(lambda (label)
	  (if (string? label)
              (printf "~a:\n" label)
              (printf "~a:\n" (label->x86-64-label label)))))

      (define-syntax emit-jump
	(syntax-rules ()
	  [(_ opcode ?target)
	   (let ([target ?target])
	     (if (label? target)
		 (emit opcode (label->x86-64-label target))
		 (emit opcode (format "*~a" (rand->x86-64-arg target)))))]))

      (define-syntax emit-program
	(syntax-rules ()
	  [(_ code code* ...)
	   (begin
	     (emit ".globl _scheme_entry")
	     (emit-label "_scheme_entry")
	     (emit 'pushq 'rbx)
	     (emit 'pushq 'rbp)
	     (emit 'pushq 'r12)
	     (emit 'pushq 'r13)
	     (emit 'pushq 'r14)
	     (emit 'pushq 'r15)
	     (emit 'movq 'rdi 'rbp)
	     (emit 'leaq "_scheme_exit(%rip)" 'r15)
	     code code* ...
	     (emit-label "_scheme_exit")
	     (emit 'popq 'r15)
	     (emit 'popq 'r14)
	     (emit 'popq 'r13)
	     (emit 'popq 'r12)
	     (emit 'popq 'rbp)
	     (emit 'popq 'rbx)
	     (emit 'ret))]))

      ;;Converts an operator to instruction
      (define op->inst
	(lambda (op)
	  (case op
	    [(+) 'addq]
	    [(-) 'subq]
	    [(*) 'imulq]
	    [(logand) 'andq]
	    [(logor) 'orq]
	    [(sra) 'sarq]
	    [(=) 'je]
	    [(<) 'jl]
	    [(<=) 'jle]
	    [(>) 'jg]
	    [(>=) 'jge]
	    [else (error who "unexpected binop ~s" op)])))

    ;;; Takes assembly instruction and returns the opposite of it
      (define inst->inst^
	(lambda (inst)
	  (case inst
	    [(je) 'jne]
	    [(jl) 'jge]
	    [(jle) 'jg]
	    [(jg) 'jle]
	    [(jge) 'jl]
	    )))
      )
    (Location : Location (x) -> * (str)
	  [(disp ,r ,i) (format "~s(%~s)" i r)]
	  [else x])

    ;; TODO: implement disp as r7rs records, so that we don't need the superfluous Triv functions.
    (emit-statement : Statement (x) -> * ()
		    [,l (emit-label l)]
		    [(set! ,loc (,binop ,triv1 ,triv2))
		     (emit (op->inst binop) triv2 (Location loc))]
		    [(set! ,loc ,l)
		     (emit 'leaq l (Location loc))]
		    [(set! ,loc ,triv)
		     (emit 'movq triv (Location loc))]
		    [(if (,relop ,triv1 ,triv2) (jump ,triv))
		     (emit 'cmpq triv2 triv1)
		     (emit-jump (op->inst relop) triv)]
		    [(if (not (,relop ,triv1 ,triv2)) (jump ,triv))
		     (emit 'cmpq triv2 triv1)
		     (emit-jump (inst->inst^ (op->inst relop)) triv)]
		    [(jump ,triv) (emit-jump 'jmp triv)]
		    [else (error who "~s is not generated!" x)])

    (Program : Program (x) -> * ()
	     [(code ,stmt* ...)
	      (emit-program (for-each emit-statement stmt*))]))
  )

