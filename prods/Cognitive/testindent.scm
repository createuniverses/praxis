(define (fact x)
  (if (<= x 1)
    1
    (* x
      (fact
        (- x 1)))))

print2(getErrorText())
[string "function f3Pressed()..."]:2: attempt to call global 'findMatchingLeftParen' (a nil value)
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function 'findMatchingLeftParen'
	[string "function f3Pressed()..."]:2: in function 'f3Pressed'
	[string "f3Pressed()"]:1: in main chunk

setBufferName("testindent.scm")
