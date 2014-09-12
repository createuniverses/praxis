(provide 'write.scm)

;;; pretty-print:   the usual pretty printer, intended for s7
;;; checkpoint:     use write-readably to save the current runtime state of s7 as a loadable scheme file


;;; -------------------------------- pretty-print --------------------------------

(define* (pretty-print obj (port (current-output-port)) (column 0))

  (define (pretty-print-1 obj port column)
    (define (spaces n) 
      (write-char #\newline port)
      (do ((i 0 (+ i 1))) ((= i n)) (write-char #\space port)))
    
    (define (stacked-list lst col)
      (let ((added 0))
	(if (keyword? (car lst))
	    (begin
	      (write (car lst) port)
	      (write-char #\space port)
	      (set! added (+ 1 (length (object->string (car lst)))))
	      (set! lst (cdr lst))))
	(if (pair? lst)
	    (begin
	      (pretty-print-1 (car lst) port (+ col added))
	      (set! added 0)
	      (do ((l1 (cdr lst) (cdr l1)))
		  ((null? l1))
		(spaces col)
		(if (keyword? (car l1))
		    (begin
		      (write (car l1) port)
		      (write-char #\space port)
		      (set! added (+ 1 (length (object->string (car l1)))))
		      (set! l1 (cdr l1))))
		(if (pair? l1)
		    (pretty-print-1 (car l1) port (+ col added)))
		(set! added 0))))))
    
    (define (stacked-split-list lst col)
      (if (pair? lst)
	  (begin
	    (write-char #\( port)
	    (write (caar lst) port)
	    (write-char #\space port)
	    (pretty-print-1 (cadar lst) port (+ col (length (symbol->string (caar lst))) 2))
	    (write-char #\) port)
	    (do ((l1 (cdr lst) (cdr l1)))
		((null? l1))
	      (spaces col)
	      (write-char #\( port)
	      (write (caar l1) port)
	      (write-char #\space port)
	      (pretty-print-1 (cadar l1) port (+ col (length (symbol->string (caar l1))) 2))
	      (write-char #\) port)))))
    
    (define (messy-number z)
      (if (real? z)
	  (if (or (nan? z)
		  (infinite? z))
	      (object->string z)
	      (if (= z pi)
		  "pi"
		  (format #f "~,4f" z)))
	  (format "~A~A~Ai" 
		  (messy-number (real-part z))
		  (if (negative? (imag-part z)) "-" "+")
		  (messy-number (abs (imag-part z))))))

    (define (any-keyword? lst)
      (and (pair? lst)
	   (or (keyword? (car lst))
	       (any-keyword? (cdr lst)))))
    
    (cond ((number? obj)
	   (if (rational? obj)
	       (write obj port)
	       (display (messy-number obj) port)))
	  
	  ((pair? obj)
	   (case (car obj)
	     
	     ((lambda lambda* define* define-macro define-macro* define-bacro define-bacro* with-environment when unless)
	      (format port "(~A ~A" (car obj) (cadr obj))
	      (spaces (+ column 2))
	      (stacked-list (cddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((defmacro defmacro*)
	      (format port "(~A ~A ~A" (car obj) (cadr obj) (caddr obj))
	      (spaces (+ column 2))
	      (stacked-list (cdddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((define)
	      (format port "(~A ~A " (car obj) (cadr obj))
	      (if (pair? (cadr obj))
		  (begin
		    (spaces (+ column 2))
		    (stacked-list (cddr obj) (+ column 2))
		    (write-char #\) port))
		  (write (caddr obj) port)))
	     
	     ((do)
	      (format port "(do (")
	      (if (pair? (cadr obj))
		  (stacked-list (cadr obj) (+ column 5)))
	      (write-char #\) port)
	      (spaces (+ column 4))
	      (write (caddr obj) port)
	      (spaces (+ column 2))
	      (stacked-list (cdddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((cond)
	      (format port "(cond ")
	      (stacked-list (cdr obj) (+ column 6))
	      (write-char #\) port))
	     
	     ((or and)
	      (if (> (length (object->string obj)) 40)
		  (begin
		    (format port "(~A " (car obj))
		    (stacked-list (cdr obj) (+ column 2 (length (symbol->string (car obj)))))
		    (write-char #\) port))
		  (write obj port)))
	     
	     ((case)
	      (format port "(case ~A" (cadr obj))
	      (spaces (+ column 2))
	      (stacked-list (cddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((begin call-with-exit call/cc call-with-current-continuation with-baffle)
	      (format port "(~A" (car obj))
	      (if (pair? (cdr obj))
		  (begin
		    (spaces (+ column 2))
		    (stacked-list (cdr obj) (+ column 2))))
	      (write-char #\) port))
	     
	     ((if)
	      (let ((objstr (object->string obj))
		    (ifcol (+ column 4)))
		(if (< (length objstr) 40)
		    (display objstr port)
		    (begin
		      (format port "(if ")
		      (pretty-print-1 (cadr obj) port ifcol)
		      (spaces (+ column 4))
		      (pretty-print-1 (caddr obj) port ifcol)
		      (if (pair? (cdddr obj))
			  (begin
			    (spaces (+ column 4))
			    (pretty-print-1 (cadddr obj) port ifcol)))
		      (write-char #\) port)))))
	     
	     ((let let* letrec letrec*)
	      (let ((head-len (length (symbol->string (car obj)))))
		(if (symbol? (cadr obj))
		    (begin
		      (format port "(~A ~A (" (car obj) (cadr obj))
		      (stacked-split-list (caddr obj) (+ column head-len (length (symbol->string (cadr obj))) 4)))
		    (begin
		      (format port "(~A (" (car obj))
		      (stacked-split-list (cadr obj) (+ column head-len 3))))
		(write-char #\) port)
		(spaces (+ column 2))
		(stacked-list (if (symbol? (cadr obj)) (cdddr obj) (cddr obj)) (+ column 2))
		(write-char #\) port)))
	     
	     ((set!)
	      (let ((str (object->string obj)))
		(if (> (length str) 60)
		    (let ((settee (object->string (cadr obj))))
		      (format port "(set! ~A" settee)
		      (if (> (length settee) 20)
			  (begin
			    (spaces (+ column 6))
			    (pretty-print-1 (caddr obj) port (+ column 6)))
			  (begin
			    (write-char #\space port)
			    (pretty-print-1 (caddr obj) port (+ column 7 (length settee))))))
		    (display str port))))
	     
	     ((quote)
	      (write-char #\' port)
	      (pretty-print-1 (cadr obj) port column))
	     
	     (else
	      (let* ((objstr (object->string obj))
		     (strlen (length objstr)))
		(if (< strlen 60)
		    (display objstr port)
		    (let ((lstlen (length obj)))
		      (if (or (infinite? lstlen)
			      (< lstlen 2))
			  (display objstr port)
			  (let* ((carstr (object->string (car obj)))
				 (carstrlen (length carstr)))
			    (format port "(~A" carstr)
			    (if (any-keyword? (cdr obj))
				(begin
				  (spaces (+ column 2))
				  (stacked-list (cdr obj) (+ column 2)))
				(let ((line-len (ceiling (/ (- strlen carstrlen) 40))))
				  (if (= lstlen 2)
				      (begin
					(write-char #\space port)
					(pretty-print-1 (cadr obj) port (+ column 2 carstrlen)))
				      (if (< lstlen 5)
					  (begin
					    (write-char #\space port)
					    (stacked-list (cdr obj) (+ column 2 carstrlen)))
					  (let ((lst (cdr obj)))
					    (do ((i 1 (+ i line-len)))
						((>= i lstlen))
					      (do ((k 0 (+ k 1)))
						  ((or (null? lst)
						       (= k line-len)))
						(format port " ~A" (car lst))
						(set! lst (cdr lst)))
					      (if (pair? lst)
						  (spaces (+ column carstrlen 2)))))))))))))))))
	  (else
	   (write obj port))))
  
  (pretty-print-1 obj port column))


(define (pp obj)
  (with-output-to-string
    (lambda ()
      (pretty-print obj))))

(define (test-pretty-print)

  (if (not (string=? (pp '(lambda* (a b) (+ a b) (* 1 2))) "(lambda* (a b)\n  (+ a b)\n  (* 1 2))"))
      (format *stderr* "pp 1"))

  (if (not (string=? (pp '(let ((a 1) (b 2)) (+ a b))) "(let ((a 1)\n      (b 2))\n  (+ a b))"))
      (format *stderr* "pp 2"))

  (if (not (string=? (pp '(let () (+ a b))) "(let ()\n  (+ a b))"))
      (format *stderr* "pp 2a"))

  (if (not (string=? (pp '(begin (+ 1 2) (* 2 3))) "(begin\n  (+ 1 2)\n  (* 2 3))"))
      (format *stderr* "pp 3"))

  (if (not (string=? (pp '(case a ((a b c) 1) ((d) 2) (else 3))) "(case a\n  ((a b c) 1)\n  ((d) 2)\n  (else 3))"))
      (format *stderr* "pp 4"))

  (if (not (string=? (pp '(cond ((> a 1) 2) ((< a 3) 3) (#t 4))) "(cond ((> a 1) 2)\n      ((< a 3) 3)\n      (#t 4))"))
      (format *stderr* "pp 5"))

  (if (not (string=? (pp '(if a '(1 2 3))) "(if a '(1 2 3))"))
      (format *stderr* "pp7"))
  )

(test-pretty-print)

#|
(let ((st (symbol-table)))
  (for-each
   (lambda (sym)
     (if (defined? sym)
	 (let ((val (symbol->value sym)))
	   (format *stderr* "~A ~A " sym val)
	   (format *stderr* "~A" (if (or (procedure? val)
					 (macro? val))
				     (pp (procedure-source val))
				     (pp val)))
	   (newline *stderr*))))
   st))
|#


 
#|
;;; -------------------------------- checkpoint --------------------------------

(define* (checkpoint (file "checkpoint-s7.scm"))
  (call-with-output-file file
    (lambda (p)
      (let ((st (symbol-table)))
	  (for-each
	   (lambda (sym)
	     (if (defined? sym)
		 (let ((choice (*autoload* sym)))
		   (if (string? choice)
		       (format p "(if (not (defined? '~A)) (load ~S))~%" sym choice)
		       (if (procedure? choice)
			   (format p "(if (not (defined? '~A)) ((~S) (current-environment)))~%" sym choice))))))
	   st)

      ;; now presumably we've loaded all the findable files, and called the autoload functions
      ;; run through the table again checking for diffs or omissions -- will this cover all the s7 settings?

	  (for-each
	   (lambda (sym)
	     (if (and (defined? sym)
		      (not (constant? sym))
		      (not (memq sym '(i st p file 
					 
					 ;; s7
					 multiple-value-bind letrec* *load-path* macroexpand *safety* *maximum-stack-size*
					 *#readers* *vector-print-length* *gc-stats* multiple-value-set! cond-expand
					 *features* call-with-values

					 ;; snd
					 *snd-opened-sound* break break-ok break-enter break-exit undo-edit redo-edit
					 ))))
		 ;; here we need to leave out built-in functions like abs 
		 (let ((choice (*autoload* sym)))
		   (if (and (not choice)
			    (not (string=? (object->string (symbol->value sym)) (object->string sym))))
		       (begin
			 (catch #t
			   (lambda ()
			     (let ((str (object->string (symbol->value sym) :readable)))
			       (format p "(if (not (defined? '~A)) (define ~A ~A))~%" sym sym str)))
			   (lambda args
			     (format *stderr* "~A not saved~%" sym))))))))
	   st)

	;; now look for changes?  This probably can't work for macros/functions (not equal? anyway)

      )))
  #f)
|#
