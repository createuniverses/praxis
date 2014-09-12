;; some useful (or at least amusing) functions and macros

(provide 'stuff.scm)

;;; ----------------
(define (empty? obj) 
  "(empty? obj) returns #t if obj is an empty sequence"
  (catch #t
    (lambda ()
      (if (hash-table? obj)
	  (= (hash-table-entries obj) 0) ; length here is table size
	  (= (length obj) 0)))
    (lambda args #f)))

(define applicable? arity)

(define (sequence? obj)
  "(sequence> obj) returns #t if obj is a sequence (vector, list, string, etc)"
  (catch #t
    (lambda () (length obj))
    (lambda args #f)))

(define (indexable? obj)
  "(indexable? obj) returns #t if obj can be applied to an index: (obj 0)"
  (and (sequence? obj)
       (applicable? obj)))
      


;;; ----------------
(define (first obj) (obj 0))
(define (second obj) (obj 1))
(define (third obj) (obj 2))
(define (fourth obj) (obj 3))
(define (fifth obj) (obj 4))
(define (sixth obj) (obj 5))
(define (seventh obj) (obj 6))
(define (eighth obj) (obj 7))
(define (ninth obj) (obj 8))
(define (tenth obj) (obj 9))


(define* (iota n (start 0) (incr 1))
  "(iota n (start 0) (incr 1)) returns a list counting from start for n:\n\
    (iota 3) -> '(0 1 2)"
  (if (or (not (integer? n))
	  (< n 0))
      (error 'wrong-type-arg "iota length ~A should be a non-negative integer" n))
  (let ((lst (make-list n)))
    (do ((p lst (cdr p))
	 (i start (+ i incr)))
	((null? p) lst)
      (set! (car p) i))))


(define* (make-circular-list n init)
  "(make-circular-list n init) returns a circular list with n entries initialized to init:\n\
    (make-circular-list 3 #f) -> #1=(#f #f #f . #1#)"
  (let ((l (make-list n init)))
    (set-cdr! (list-tail l (- n 1)) l)))

(define (circular-list . objs)
  "(circular-list . objs) returns a circular list with objs:\n\
    (circular-list 1 2) -> #1=(1 2 . #1#)"
  (let ((lst (copy objs)))
    (set-cdr! (list-tail lst (- (length lst) 1)) lst)))

(define (circular-list? obj)
  "(circular-list? obj) returns #t if obj is a circular list"
  (catch #t
    (lambda () (infinite? (length obj)))
    (lambda args #f)))

(define (linearize lst) 
  " (linearize lst) turns a circular list into normal list:\n\
    (linearize (circular-list 1 2)) -> '(1 2)"
  (define (lin-1 lst result sofar)
    (if (or (not (pair? lst))
	    (memq lst sofar))
	(reverse! result)
	(lin-1 (cdr lst) 
	       (cons (car lst) result) 
	       (cons lst sofar))))
  (lin-1 lst () ()))

(define (cyclic? obj)
  "(cyclic obj) returns #t if the sequence obj contains any cycles"
  (pair? (cyclic-sequences obj)))


(define (copy-tree lis)
  "(copy-tree lst) returns a full copy of lst"
  (if (pair? lis)
      (cons (copy-tree (car lis))
	    (copy-tree (cdr lis)))
      lis))

(define (tree-member sym tree)
  "(tree-member sym tree) returns #t if sym is found anywhere in tree:\n\
    (tree-member 'a '(1 (2 a))) -> #t"
  (and (pair? tree)
       (or (eq? (car tree) sym)
	   (and (pair? (car tree))
		(tree-member sym (car tree)))
	   (tree-member sym (cdr tree)))))

(define (adjoin obj lst)
  "(adjoin obj lst) adds obj to lst if it is not already in lst, returning the new list"
  (if (member obj lst) lst (cons obj lst)))



;;; ----------------
(define setf
  (let ((args (gensym))
	(name (gensym)))
    (symbol->value 
     (apply define-bacro `((,name . ,args)        
			   (unless (null? ,args)
			     (apply set! (car ,args) (cadr ,args) ())
			     (apply setf (cddr ,args))))))))

(define-macro* (incf sym (inc 1))
  `(set! ,sym (+ ,sym ,inc)))

(define-macro* (decf sym (dec 1))
  `(set! ,sym (- ,sym ,dec)))

(define-macro (shiftf . places)
  (let ((tmp (gensym)))
    `(let ((,tmp ,(car places)))
       ,@(map (lambda (a b)
		`(set! ,a ,b))
	      places
	      (cdr places))
       ,tmp)))

(define-macro (rotatef . places)
  (let ((tmp (gensym))
	(last (car (list-tail places (- (length places) 1)))))
    `(let ((,tmp ,(car places)))
       ,@(map (lambda (a b)
		`(set! ,a ,b))
	      places
	      (cdr places))
       (set! ,last ,tmp))))

(define-macro (progv vars vals . body)
  `(apply (apply lambda ,vars ',body) ,vals))

(define-macro (symbol-set! var val) ; like CL's set
  `(apply set! ,var ',val ()))

(define-bacro (value->symbol val)
  `(call-with-exit
    (lambda (return)
      (do ((e (current-environment) (outer-environment e))) ()
	(for-each 
	 (lambda (slot)
	   (if (equal? ,val (cdr slot))
	       (return (car slot))))
	 e)
	(if (eq? e (global-environment))
	    (return #f))))))

(define-macro (enum . args)
  `(for-each define ',args (iota (length ',args))))

(define-macro (destructuring-bind lst expr . body) ; if only there were some use for this!
  `(let ,(letrec ((flatten (lambda (lst1 lst2 args)
			     (cond ((null? lst1) args)
				   ((not (pair? lst1)) 
				    (cons (list lst1 lst2) args))
				   (#t (flatten (car lst1) (car lst2) 
					 (flatten (cdr lst1) (cdr lst2) args)))))))
	   (flatten lst (eval expr) ()))
     ,@body))

(define-macro (elambda args . body)  ; lambda but pass extra arg "*env*" = run-time env
  `(symbol->value 
    (define-bacro (,(gensym) ,@args)
      `((lambda* ,(append ',args `((*env* (current-environment))))
	  ,'(begin ,@body)) 
	,,@args))))

(define-macro* (rlambda args . body) ; lambda* but eval arg defaults in run-time env
  (let ((arg-names (map (lambda (arg) (if (pair? arg) (car arg) arg)) args))
	(arg-defaults (map (lambda (arg) (if (pair? arg) `(,(car arg) (eval ,(cadr arg))) arg)) args)))
  `(symbol->value
    (define-bacro* (,(gensym) ,@arg-defaults)
      `((lambda ,',arg-names ,'(begin ,@body)) ,,@arg-names)))))

(define-macro (and-let* vars . body)
  `(let ()                                ; bind vars, if any is #f stop, else evaluate body with those bindings
     (and ,@(map (lambda (var) 
		   `(begin 
		      (apply define ',var) 
		      ,(car var))) 
		 vars)
          (begin ,@body))))

(define-macro (while test . body)         ; while loop with predefined break and continue
  `(call-with-exit
    (lambda (break) 
      (letrec ((continue (lambda () 
			   (if (let () ,test)
			       (begin 
				 (let () ,@body)
				 (continue))
			       (break)))))
	(continue)))))

(define-macro (do* spec end . body)
  `(let* (,@(map (lambda (var) 
		   (list (car var) (cadr var))) 
		 spec))
     (do () ,end
       ,@body
       ,@(map (lambda (var) 
		(if (pair? (cddr var))
		    `(set! ,(car var) ,(caddr var))
		    (values)))
	      spec))))

(define-macro (string-case selector . clauses)
  `(case (symbol ,selector)             ; case with string constant keys
     ,@(map (lambda (clause)
	      (if (pair? (car clause))
		  `(,(map symbol (car clause)) ,@(cdr clause))
		  clause))
	    clauses)))

(define-macro (eval-case key . clauses) ; case with evaluated key-lists
  (let ((select (gensym)))
    `(begin 
       (define ,select ,key)
       (cond ,@(map (lambda (lst)
		      (if (pair? (car lst))
			  (cons `(member ,select (list ,@(car lst)))
				(cdr lst))
			  lst))
		    clauses)))))



;;; ---------------- 
(define (hash-table->alist table)
  "(hash-table->alist table) returns the contents of table as an association list:\n\
    (hash-table->alist (hash-table '(a . 1))) -> '((a . 1))"
  (map values table))

(define (merge-hash-tables . tables)
  "(merge-hash-tables . tables) returns a new hash-table with the contents of all the tables"
  (apply hash-table 
    (apply append 
      (map hash-table->alist tables))))



;;; ----------------
(define-macro (c?r path)
  (define (X-marks-the-spot accessor tree)
    (if (pair? tree)
	(or (X-marks-the-spot (cons 'car accessor) (car tree))
	    (X-marks-the-spot (cons 'cdr accessor) (cdr tree)))
	(if (eq? tree 'X) accessor #f)))
  (let ((body 'lst))
    (for-each
     (lambda (f)
       (set! body (list f body)))
     (reverse (X-marks-the-spot () path)))
    `(make-procedure-with-setter
      (lambda (lst) 
	,body)
      (lambda (lst val)
	(set! ,body val)))))



;;; ----------------
(define (find-if f sequence)
  "(find-if func sequence) applies func to each member of sequence.\n\
If func approves of one, find-if returns that member of the sequence"
  (call-with-exit
   (lambda (return)
     (for-each (lambda (arg)
		 (if (f arg)
		     (return arg)))
	       sequence)
     #f)))

(define (member? obj sequence)
  "(member? obj sequence) returns #t if obj is an element of sequence"
  (find-if (lambda (x) (equal? x obj)) sequence))


(define (index-if f sequence)
  "(index-if func sequence) applies func to each member of sequence.\n\
If func approves of one, index-if returns the index that gives that element's position.\n\
    (index-if (lambda (x) (= x 32)) #(0 1 32 4)) -> 2\n\
    (index-if (lambda (x) (= (cdr x) 32)) (hash-table '(a . 1) '(b . 32))) -> 'b"
  (call-with-exit
   (lambda (return) 
     (if (or (hash-table? sequence)
	     (environment? sequence))
	 (for-each (lambda (arg)
		     (if (f arg)
			 (return (car arg))))
		   sequence)
	 (let ((position 0))
	   (for-each (lambda (arg)
		       (if (f arg)
			   (return position))
		       (set! position (+ position 1)))
		     sequence)))
     #f)))

(define (count-if f sequence)
  "(count-if func sequence) applies func to each member of sequence, returning the number of times func approves."
  (let ((count 0))
    (for-each (lambda (arg)
		(if (f arg)
		    (set! count (+ count 1))))
	      sequence)
    count))

(define (every? f sequence)
  "(every func sequence) returns #t if func approves of every member of sequence"
  (call-with-exit
   (lambda (return)
     (for-each (lambda (arg)
		 (if (not (f arg))
		     (return #f)))
	       sequence)
     #t)))

(define (any? f sequence)
  "(any func sequence) returns #t if func approves of any member of sequence"
  (call-with-exit
   (lambda (return)
     (for-each (lambda (arg)
		 (let ((val (f arg)))
		   (if val (return val))))
	       sequence)
     #f)))


(define (collect-if type f sequence)
  "(collect-if type func sequence) gathers the elements of sequence that satisfy func, and returns them via type:\n\
    (collect-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1 2)"
  (apply type (map (lambda (arg) (if (f arg) arg (values))) sequence)))

;;; if type=list, this is slightly wasteful because list currently copies its args, so:
;;;   ((if (eq? type list) values (values apply type)) ...) would work
;;;
;;; to return (f arg) rather than arg, (apply type (map f sequence))

(define (remove-if type f sequence)
  "(remove-if type f sequence) returns via type the elements of sequence that do not satisfy func:\n\
    (remove-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1.4 2/3 1+1i)"
  (collect-if type (lambda (obj) (not (f obj))) sequence))

(define (nonce type sequence) 
  "(nonce type sequence) returns via type the elements of sequence that occur only once"
  (collect-if type (lambda (obj) (= (count-if (lambda (x) (equal? x obj)) sequence) 1)) sequence))


(define (full-find-if f sequence)
  "(full-find-if func sequence) searches sequence, and recursively any sequences it contains, for an element that satisfies func"
  (call-with-exit
   (lambda (return)
     (letrec ((full-find-if-1 
	       (lambda (seq)
		 (for-each (lambda (x)
			     (if (f x)
				 (return x)
				 (if (sequence? x)
				     (full-find-if-1 x))))
			   seq))))
       (full-find-if-1 sequence))
     #f)))

(define (full-count-if f sequence)
  "(full-count-if func sequence) searches sequence, and recursively any sequences it contains, returning the number of elements that satisfy func"
  (let ((count 0))
    (full-find-if (lambda (x) (if (f x) (set! count (+ count 1))) #f) sequence)
    count))

(define (full-index-if f sequence)
  "(full-index-if func sequence) searches sequence, and recursively any sequences it contains, returning the indices of the first element that satisfies func:\n\
    (full-index-if (lambda (x) (and (integer? x) (= x 3))) '(1 (2 3))) -> '(1 1)"
  (call-with-exit
   (lambda (return)
     (letrec ((full-index-if-1 
	       (lambda (f seq path)
		 (if (or (hash-table? seq)
			 (environment? seq))
		     (for-each (lambda (arg)
				 (if (f arg)
				     (return (reverse (cons (car arg) path)))
				     (if (indexable? (cdr arg))
					 (full-index-if-1 f (cdr arg) (cons (car arg) path)))))
			       seq)
		     (let ((position 0))
		       (for-each (lambda (arg)
				   (if (f arg)
				       (return (reverse (cons position path)))
				       (if (indexable? arg)
					   (full-index-if-1 f arg (cons position path))))
				   (set! position (+ position 1)))
				 seq))))))
       (full-index-if-1 f sequence ())
       #f))))

(define (safe-find-if f sequence) ; can handle almost any kind of cycle
  "(safe-find-if func sequence) searches sequence, and recursively any sequences it contains, for an element that satisfies func.\
Unlike full-find-if, safe-find-if can handle any circularity in the sequences."
  (let ((unseen-cycles (cyclic-sequences sequence))
	(cycles-traversed ()))
    (call-with-exit
     (lambda (return)
      (letrec* ((check (lambda (obj)
			 (if (f obj)
			     (return obj))
			 (if (sequence? obj)
			     (safe-find-if-1 obj))))
		(safe-find-if-1 
		 (lambda (seq)
		   (when (not (memq seq cycles-traversed))
		     (if (memq seq unseen-cycles)
			 (begin
			   (set! cycles-traversed (cons seq cycles-traversed))
			   (set! unseen-cycles (remove-if list (lambda (x) (eq? x seq)) unseen-cycles))
			   (if (pair? seq)
			       (begin
				 (check (car seq))
				 (do ((p (cdr seq) (cdr p)))
				     ((or (not (pair? p))
					  (memq p cycles-traversed)
					  (memq p unseen-cycles)))
				   (check (car p))))
			       (for-each check seq)))
			 (for-each check seq))))))
	 (safe-find-if-1 sequence))
       #f))))

(define (safe-count-if f sequence)
  (let ((count 0))
    (safe-find-if (lambda (x) (if (f x) (set! count (+ count 1))) #f) sequence)
    count))




;;; ----------------
(define (sequences->list . sequences)
  "(sequences->list . sequences) returns a list of elements of all the sequences:\n\
    (sequences->list \"hi\" #(0 1) (hash-table '(a . 2))) -> '(#\\h #\\i 0 1 (a . 2))"
  (apply append 
    (map (lambda (sequence) 
	   (map values sequence)) 
	 sequences)))

(define (concatenate type . sequences)
  "(concatenate type . sequences) concatenates sequences returning an object of type:\n\
    (concatenate vector '(1 2) #(3 4)) -> #(1 2 3 4)"
  (apply type (apply sequences->list sequences)))

(define (intersection type . sequences)
  "(intersection type . sequences) returns via type the intersection of the sequences:\n\
    (intersection vector '(1 2 3) #(2 3 4)) -> #(2 3)"
  (apply type (let ((lst ()))
		(if (pair? sequences)
		    (for-each (lambda (obj)
				(if (every? (lambda (seq) 
					      (find-if (lambda (x) 
							 (equal? x obj)) 
						       seq)) 
					    (cdr sequences))
				    (set! lst (cons obj lst))))
			      (car sequences)))
		(reverse lst))))

(define (union type . sequences)
  "(union type . sequences) returns via type the union of the sequences:\n\
    (union  vector '(1 2 3) #(2 3 4)) -> #(1 2 3 4)"
  (apply type (let ((lst ()))
		(for-each (lambda (obj)
			    (if (not (member obj lst))
				(set! lst (cons obj lst))))
			  (apply sequences->list sequences))
		(reverse lst))))

(define (asymmetric-difference type . sequences) ; complement, elements in B's not in A
  "(asymmetric-difference type . sequences) returns the elements in the rest of the sequences that are not in the first:\n\
    (asymmetric-difference vector '(1 2 3) #(2 3 4) '(1 5)) -> #(4 5)"
  (if (and (pair? sequences)
	   (pair? (cdr sequences)))
      (collect-if type (lambda (obj) 
			 (not (member obj (car sequences))))
		  (apply union list (cdr sequences)))
      (apply type ())))

(define (cl-set-difference type . sequences)     ; CL: elements in A not in B's
  "(cl-set-difference type .sequences) returns the elements in the first sequence that are not in the rest of the sequences:\n\
    (cl-set-difference vector '(1 2 3) #(2 3 4) '(1 5)) -> #()"
  (if (and (pair? sequences)
	   (pair? (cdr sequences)))
      (let ((others (apply union list (cdr sequences))))
	(collect-if type (lambda (obj) 
			   (not (member obj others)))
		    (car sequences)))
      (apply type ())))
  
(define (symmetric-difference type . sequences)  ; xor, elements in an odd number of sequences (logxor A B...)
  "(symmetric-difference type .sequences) returns the elements that are in an odd number of the sequences:\n\
    (symmetric-difference vector '(1 2 3) #(2 3 4) '(5)) -> #(1 4 5)"
  (let ((all (apply sequences->list sequences)))
    (collect-if type (lambda (obj) 
		       (odd? (count-if (lambda (x) 
					 (equal? x obj)) 
				       all))) 
		(apply union list sequences))))

(define (power-set type . sequences) ; ignoring repeats
  "(power-set type . sequences) returns the power set of the union of the elements in the sequences."
  (letrec ((pset (lambda (set)
		   (if (null? set)
		       '(()) 
		       (let ((rest (pset (cdr set))))
			 (append rest (map (lambda (subset) 
					     (cons (car set) subset)) 
					   rest)))))))
    (apply type (pset (apply union list sequences)))))



;;; ----------------
(define ->predicate
  (let ((predicates (list integer? rational? real? complex? number?
			  bytevector? string?
			  float-vector? vector?
			  null? pair? list? 
			  keyword? gensym? symbol?
			  char? string?
			  hash-table? hash-table-iterator? 
			  continuation? 
			  input-port? output-port? 
			  environment? 			     
			  procedure-with-setter? procedure? macro?
			  boolean?
			  random-state? 
			  eof-object? 
			  c-pointer? 
			  (lambda (obj) (eq? obj #<unspecified>))
			  (lambda (obj) (eq? obj #<undefined>)))))
    (lambda (obj)
      "(->predicate obj) returns the type predicate function for obj: (->predicate 31) -> integer?"
      (find-if (lambda (pred) (pred obj)) predicates))))

(define (add-predicate p) 
  "(add-predicate p) adds p (and boolean function of one argument) to the list of predicates used by ->predicate"
  (let ((e (procedure-environment ->predicate)))
    (set! (e 'predicates) (cons p (e 'predicates)))))

(define (typeq? . objs)
  "(typeq? . objs) returns #t if all objs have the same type (as determined by ->predicate)"
  (or (null? objs)
      (every? (->predicate (car objs)) (cdr objs))))

(define-macro (typecase expr . clauses) ; actually type=any boolean func
  (let ((obj (gensym)))
    `(begin                             ; normally this would be (let ((,obj ,expr)) ...)
       (define ,obj ,expr)              ;   but use begin so that internal defines are not blocked	    
       (cond ,@(map (lambda (clause)         
		      (if (memq (car clause) '(#t else))
			  clause
			  (if (= (length (car clause)) 1)
			      `((,(caar clause) ,obj) ,@(cdr clause))
			      `((or ,@(map (lambda (type)
					     `(,type ,obj))
					   (car clause)))
				,@(cdr clause)))))
		    clauses)))))



;;; ----------------
(define (2^n? x) 
  "(2^n? x) returns #t if x is a power of 2"
  (and (not (zero? x)) 
       (zero? (logand x (- x 1)))))

(define (2^n-1? x) 
  (zero? (logand x (+ x 1))))

(define (lognand . ints) 
  (lognot (apply logand ints)))

(define (lognor . ints) 
  (lognot (apply logior ints)))

(define (logeqv . ints)
  (if (odd? (length ints))
      (lognot (apply logxor -1 ints)) ; Clisp does it this way
      (lognot (apply logxor ints))))

(define (log-none-of . ints)  ; bits on in none of ints
  (lognot (apply logior ints)))

(define (log-all-of . ints)   ; bits on in all of ints
  (apply logand ints))

(define (log-any-of . ints)   ; bits on in at least 1 of ints
  (apply logior ints))

(define (log-n-of n . ints)   ; return the bits on in exactly n of ints
  (let ((len (length ints)))
    (cond ((= len 0) (if (= n 0) -1 0))
	  ((= n 0)   (lognot (apply logior ints)))
	  ((= n len) (apply logand ints))
	  ((> n len) 0)
	  (#t 
	   (do ((1s 0)
		(prev ints)
		(i 0 (+ i 1)))
	       ((= i len) 1s)
	     (let ((cur (ints i)))
	       (if (= i 0)
		   (set! 1s (logior 1s (logand cur (apply log-n-of (- n 1) (cdr ints)))))
		   (let* ((mid (cdr prev))
			  (nxt (if (= i (- len 1)) () (cdr mid))))
		     (set! (cdr prev) nxt)  
		     (set! 1s (logior 1s (logand cur (apply log-n-of (- n 1) ints))))
		     (set! (cdr prev) mid)
		     (set! prev mid)))))))))

;; from Rick
(define (byte siz pos) ;; -> cache size, position and mask.
  (list siz pos (ash (- (ash 1 siz) 1) pos)))

(define byte-size car)
(define byte-position cadr)
(define byte-mask caddr)

(define (ldb bytespec integer)
  (ash (logand integer (byte-mask bytespec))
       (- (byte-position bytespec))))

(define (dpb integer bytespec into)
  (logior (ash (logand integer (- (ash 1 (byte-size bytespec)) 1)) (byte-position bytespec))
	  (logand into (lognot (byte-mask bytespec)))))


;;; ----------------
(define-bacro* (define-class class-name inherited-classes (slots ()) (methods ()))
  ;; a bacro is needed so that the calling environment is accessible via outer-environment
  ;;   we could also use the begin/let shuffle, but it's too embarrassing
  `(let ((outer-env (outer-environment (current-environment)))
	 (new-methods ())
	 (new-slots ()))
     
     (for-each
      (lambda (class)
	;; each class is a set of nested environments, the innermost (first in the list)
	;;   holds the local slots which are copied each time an instance is created,
	;;   the next holds the class slots (global to all instances, not copied);
	;;   these hold the class name and other such info.  The remaining environments
	;;   hold the methods, with the localmost method first.  So in this loop, we
	;;   are gathering the local slots and all the methods of the inherited
	;;   classes, and will splice them together below as a new class.
	
	(set! new-slots (append (environment->list class) new-slots))
	(do ((e (outer-environment (outer-environment class)) (outer-environment e)))
	    ((or (not (environment? e))
		 (eq? e (global-environment))))
	  (set! new-methods (append (environment->list e) new-methods))))
      ,inherited-classes)
     
     (let ((remove-duplicates 
	    (lambda (lst)         ; if multiple local slots with same name, take the localmost
	      (letrec ((rem-dup
			(lambda (lst nlst)
			  (cond ((null? lst) nlst)
				((assq (caar lst) nlst) (rem-dup (cdr lst) nlst))
				(else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
		(reverse (rem-dup lst ()))))))
       (set! new-slots 
	     (remove-duplicates
	      (append (map (lambda (slot)
			     (if (pair? slot)
				 (cons (car slot) (cadr slot))
				 (cons slot #f)))
			   ,slots)                    ; the incoming new slots, #f is the default value
		      new-slots))))                   ; the inherited slots
     
     (set! new-methods 
	   (append (map (lambda (method)
			  (if (pair? method)
			      (cons (car method) (cadr method))
			      (cons method #f)))
			,methods)                     ; the incoming new methods
		   
		   ;; add an object->string method for this class (this is already a generic function).
		   (list (cons 'object->string (lambda* (obj (use-write #t))
						 (if (eq? use-write :readable)    ; write readably
						     (format #f "(make-~A~{ :~A ~W~^~})" 
							     ',class-name 
							     (map (lambda (slot)
								    (values (car slot) (cdr slot)))
								  obj))
						     (format #f "#<~A: ~{~A~^ ~}>" 
							     ',class-name
							     (map (lambda (slot)
								    (list (car slot) (cdr slot)))
								  obj))))))
		   (reverse! new-methods)))           ; the inherited methods, shadowed automatically
     
     (let ((new-class (open-environment
                       (apply augment-environment           ; the local slots
			      (augment-environment               ; the global slots
				  (apply environment               ; the methods
					 (reverse new-methods))
				(cons 'class-name ',class-name)  ; class-name slot
				(cons 'inherited ,inherited-classes)
				(cons 'inheritors ()))           ; classes that inherit from this class
			      new-slots))))
       
       (augment-environment! outer-env                  
	 (cons ',class-name new-class)                       ; define the class as class-name in the calling environment
	 
	 ;; define class-name? type check
	 (cons (string->symbol (string-append (symbol->string ',class-name) "?"))
	       (lambda (obj)
		 (and (environment? obj)
		      (eq? (obj 'class-name) ',class-name)))))
       
       (augment-environment! outer-env
	 ;; define the make-instance function for this class.  
	 ;;   Each slot is a keyword argument to the make function.
	 (cons (string->symbol (string-append "make-" (symbol->string ',class-name)))
	       (apply lambda* (map (lambda (slot)
				     (if (pair? slot)
					 (list (car slot) (cdr slot))
					 (list slot #f)))
				   new-slots)
		      `((let ((new-obj (copy ,,class-name)))
			  ,@(map (lambda (slot)
				   `(set! (new-obj ',(car slot)) ,(car slot)))
				 new-slots)
			  new-obj)))))
       
       ;; save inheritance info for this class for subsequent define-method
       (letrec ((add-inheritor (lambda (class)
				 (for-each add-inheritor (class 'inherited))
				 (if (not (memq new-class (class 'inheritors)))
				     (set! (class 'inheritors) (cons new-class (class 'inheritors)))))))
	 (for-each add-inheritor ,inherited-classes))
       
       ',class-name)))

(define-macro (define-generic name)    ; (define (genfun any) ((any 'genfun) any))
  `(define ,name 
     (lambda args 
       (let ((gf ((car args) ',name))) ; get local definition
	 (if (not (eq? gf ,name))      ; avoid infinite recursion
             (apply gf args)
	     (error "attempt to call generic function wrapper recursively"))))))

(define-macro (define-slot-accessor name slot)
  `(define ,name (make-procedure-with-setter 
		  (lambda (obj) (obj ',slot)) 
		  (lambda (obj val) (set! (obj ',slot) val)))))

(define-bacro (define-method name-and-args . body)
  `(let* ((outer-env (outer-environment (current-environment)))
	  (method-name (car ',name-and-args))
	  (method-args (cdr ',name-and-args))
	  (object (caar method-args))
	  (class (symbol->value (cadar method-args)))
	  (old-method (class method-name))
	  (method (apply lambda* method-args ',body)))
     
     ;; define the method as a normal-looking function
     ;;   s7test.scm has define-method-with-next-method that implements call-next-method here
     ;;   it also has make-instance 
     (augment-environment! outer-env
       (cons method-name 
	     (apply lambda* method-args 
		    `(((,object ',method-name)
		       ,@(map (lambda (arg)
				(if (pair? arg) (car arg) arg))
			      method-args))))))
     
     ;; add the method to the class
     (augment-environment! (outer-environment (outer-environment class))
       (cons method-name method))
     
     ;; if there are inheritors, add it to them as well, but not if they have a shadowing version
     (for-each
      (lambda (inheritor) 
	(if (not (eq? (inheritor method-name) #<undefined>)) ; defined? goes to the global env
	    (if (eq? (inheritor method-name) old-method)
		(set! (inheritor method-name) method))
	    (augment-environment! (outer-environment (outer-environment inheritor))
   	      (cons method-name method))))
      (class 'inheritors))
     
     method-name))

(define (all-methods obj method)
  ;; for arbitrary method combinations: this returns a list of all the methods of a given name
  ;;   in obj's class and the classes it inherits from (see example below)
  (let* ((base-method (obj method))
	 (methods (if (procedure? base-method) (list base-method) ())))
    (for-each 
     (lambda (ancestor)
       (let ((next-method (ancestor method)))
	 (if (and (procedure? next-method)
		  (not (memq next-method methods)))
	     (set! methods (cons next-method methods)))))
     (obj 'inherited))
    (reverse methods)))



;;; ----------------
(define (for-each-subset func args)
  "(for-each-subset func args) forms each subset of args, then applies func to the subsets that fit its arity"
  (define (subset source dest len)
    (if (null? source)
        (if (aritable? func len)             ; does this subset fit?
	    (apply func dest))
	(begin
	  (subset (cdr source) (cons (car source) dest) (+ len 1))
	  (subset (cdr source) dest len))))
  (subset args () 0))

(define (for-each-permutation func vals)
  "(for-each-permutation func vals) applies func to every permutation of vals:\n\
    (for-each-permutation (lambda args (format #t \"~{~A~^ ~}~%\" args)) '(1 2 3))"
  (define (pinner cur nvals len)
    (if (= len 1)
        (apply func (cons (car nvals) cur))
        (do ((i 0 (+ i 1)))                       ; I suppose a named let would be more Schemish
            ((= i len))
          (let ((start nvals))
            (set! nvals (cdr nvals))
            (let ((cur1 (cons (car nvals) cur)))  ; add (car nvals) to our arg list
              (set! (cdr start) (cdr nvals))      ; splice out that element and 
              (pinner cur1 (cdr start) (- len 1)) ;   pass a smaller circle on down, "wheels within wheels"
              (set! (cdr start) nvals))))))       ; restore original circle
  (let ((len (length vals)))
    (set-cdr! (list-tail vals (- len 1)) vals)    ; make vals into a circle
    (pinner () vals len)
    (set-cdr! (list-tail vals (- len 1)) ())))    ; restore its original shape





;;; ----------------
(define (clamp minimum x maximum)
  (min maximum (max x minimum)))

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define (n-choose-k n k)
  "(n-choose-k n k) returns the binomial coefficient C(N,K)"
  (let ((mn (min k (- n k))))
    (if (< mn 0)
	0
	(if (= mn 0)
	    1
	    (let* ((mx (max k (- n k)))
		   (cnk (+ 1 mx)))
	      (do ((i 2 (+ i 1)))
		  ((> i mn) cnk)
		(set! cnk (/ (* cnk (+ mx i)) i))))))))



;;; ----------------

(define (continuable-error . args)
  "(continuable-error . args) is (apply error args) wrapped in a continuation named 'continue."
  (call/cc 
   (lambda (continue)
     (apply error args))))

(define (continue-from-error) ; maybe arg for value to pass back
  "(continue-from-error) tries to continue from the point of the earlier continuable-error"
  (if (continuation? ((error-environment) 'continue))
      (((error-environment) 'continue))))

(define (flatten-environment e)
  (let ((slots ()))
    (do ((pe e (outer-environment pe)))
	((eq? pe (global-environment))
	 (apply environment slots))
      (for-each (lambda (slot)
		  (if (not (assq (car slot) slots))
		      (set! slots (cons slot slots))))
		pe))))

(define (call-with-input-vector v proc)
  (let ((i -1))
    (proc (open-environment
	   (environment* 'read (lambda (p)
				 (v (set! i (+ i 1)))))))))

(define (call-with-output-vector proc)
  (let* ((size 1)
	 (v (make-vector size #f))
	 (i 0)
	 (write-to-vector (lambda (obj p)
			    (when (= i size) ; make the vector bigger to accommodate the output
			      (set! v (copy v (make-vector (set! size (* size 2)) #f))))
			    (set! (v i) obj)
			    (set! i (+ i 1))
			    #<unspecified>))) ; that's what write/display return!
    (proc (open-environment
	   (environment* 'write (lambda* (obj p)
				  ((if (not (environment? p)) write write-to-vector) obj p))
			 'display (lambda* (obj p)
				    ((if (not (environment? p)) display write-to-vector) obj p))
			 'format (lambda (p . args)
				   (if (not (environment? p))
				       (apply format p args)
				       (write (apply format #f args) p))))))
    (make-shared-vector v (list i)))) ; ignore extra trailing elements
