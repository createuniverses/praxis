;;; r7rs compatibility
;;;
;;; records are built on the class mechanism

(if (not (provided? 'cload.scm)) (load "cload.scm"))
(provide 'r7rs.scm)


(define (vector-map p . args) (list->vector (apply map p args)))
(define (string-map p . args) (list->string (apply map p args)))
(define vector-for-each for-each) 
(define string-for-each for-each) 


(define* (vector->string v (start 0) end) 
  (let ((stop (or end (length v)))) 
    (copy v (make-string (- stop start)) start stop)))

(define* (string->vector s (start 0) end)
  (let ((stop (or end (length s)))) 
    (copy s (make-vector (- stop start)) start stop)))

(define list-copy copy)

(define* (vector-copy v (start 0) end)
  (let ((stop (or end (length v)))) 
    (copy v (make-vector (- stop start)) start stop)))

(define* (r7rs-string-copy s (start 0) end) 
  (let ((stop (or end (length s)))) 
    (copy s (make-string (- stop start)) start stop)))

(define* (r7rs-vector-fill! seq fill start end)
  (if (not start)
      (fill! seq fill)
      (let ((len (or end (length seq))))
	(do ((i start (+ i 1)))
	    ((= i len) seq)
	  (set! (seq i) fill)))))

(define r7rs-string-fill! vector-fill!)

(define* (vector-copy! dest at src (start 0) end) ; apparently end is exclusive here?
  (let ((len (or end (length src))))
    (if (or (not (eq? dest src))
	    (<= at start))
	(do ((i at (+ i 1))
	     (k start (+ k 1)))
	    ((= k len) dest)
	  (set! (dest i) (src k)))
	(do ((i (+ at (- len start 1)) (- i 1))
	     (k (- len 1) (- k 1)))
	    ((< k start) dest)
	  (set! (dest i) (src k))))))

(define bytevector-copy! vector-copy!)
(define string-copy! vector-copy!)
;;; bytevector->list is (map values bv)


(define char-foldcase char-downcase) 
(define string-foldcase string-downcase)
;;; these and the string functions in s7 are not unicode-aware.  To get true unicode
;;;   handling of the bytes, use the glib functions in libxg or use cload (see xgdata.scm).
(define (digit-value c) (and (char-numeric? c) (- (char->integer c) (char->integer #\0))))


(define +inf.0 inf.0)
(define +nan.0 nan.0)
(define (finite? n) (and (number? n) (not (nan? n)) (not (infinite? n))))

(define exact-integer? integer?)	
(define (exact-integer-sqrt i) (let ((sq (floor (sqrt i)))) (values sq (- i (* sq sq)))))
(define inexact exact->inexact)
(define exact inexact->exact)
(define (square x) (* x x))
(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define floor-remainder modulo)
(define (floor-quotient x y) (floor (/ x y)))


(define (input-port-open? p) (not (port-closed? p))) 
(define (output-port-open? p) (not (port-closed? p))) 
(define (port? p) (or (input-port? p) (output-port? p)))
(define binary-port? port?)
(define textual-port? port?)
(define (close-port p) (if (input-port? p) (close-input-port p) (close-output-port p)))
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define (call-with-port port proc) 
  ((if (input-port? port) call-with-input-file call-with-output-file) port proc))


(define (bytevector-u8-ref b k) (b k))
(define (bytevector-u8-set! b k c) (set! (b k) c))
(define bytevector-length length)
(define (bytevector-copy . args) (->bytevector (apply string-copy args)))
(define (bytevector-append . args) (->bytevector (apply string-append args)))
(define write-bytevector write-string)
(define* (read-bytevector! bv port (start 0) end)
  (let ((lim (or end (length bv)))
	(pt (or port (current-input-port))))
    (do ((i start (+ i 1))
	 (c (read-byte pt) (read-byte pt)))
	((or (>= i lim)
	     (eof-object? c))
	 bv)
      (set! (bv i) c))))
(define* (read-bytevector k port)
  (read-bytevector! (->bytevector (make-string k)) port))
(define (get-output-bytevector port) (->bytevector (get-output-string port)))
(define open-input-bytevector open-input-string)
(define open-output-bytevector open-output-string)
(define read-u8 read-byte)
(define write-u8 write-byte) 
(define u8-ready? char-ready?) 
(define peek-u8 peek-char)
(define* (utf8->string v (start 0) end) (substring v start (or end (length v))))
(define* (string->utf8 s (start 0) end) (->bytevector (substring s start (or end (length s)))))
(define write-simple write)

(define (eof-object) #<eof>)
(define (features) *features*)


(define (with-exception-handler handler thunk) (catch #t thunk handler))
(define raise error)
(define raise-continuable error)

(define-macro (guard results . body)
  `(let ((,(car results) (catch #t (lambda () ,@body) (lambda args (car args)))))
     (cond ,@(cdr results))))

(define (read-error? obj) (eq? (car obj) 'read-error))
(define (file-error? obj) (eq? (car obj) 'io-error))
(define (error-message obj) (apply format #f (cadr obj)))
(define error-irritants cdadr)


(define interaction-environment current-environment)
(define-bacro (include . files) 
  `(begin
     ,@(map (lambda (file)
	      `(load ,file (outer-environment (current-environment))))
	    files)))

(set! *#readers* (cons (cons #\; (lambda (s) (read) (values))) *#readers*))
;; I prefer (define-expansion (comment . stuff) (reader-cond (#t (values))))


(define-macro (define-values vars . body)
  `(apply begin (map (lambda (var val) `(define ,var ,val)) ',vars (list (begin ,@body)))))

(define-macro (let*-values vars . body)
  `(let () 
     ,@(map (lambda (nvars . nbody)
              `(apply define-values ',nvars ',@nbody))
            (map car vars) 
            (map cdr vars))
     ,@body))


;; case-lambda       
(define-macro (case-lambda . choices)
  `(lambda args
     (case (length args)
       ,@(map (lambda (choice)
		(if (or (symbol? (car choice))
			(negative? (length (car choice))))
		    `(else (apply (lambda ,(car choice) ,@(cdr choice)) args))
		    `((,(length (car choice))) 
		      (apply (lambda ,(car choice) ,@(cdr choice)) args))))
	      choices))))


;; parameters
(define* (make-parameter init converter)
  (let* ((convert (or converter (lambda (x) x)))
	 (old-values ()) ; see below -- this is part of the procedure-environment
	 (value (convert init)))
    (lambda () value)))

(define-macro (parameterize vars . body)
  `(dynamic-wind
       (lambda ()
	 ,@(map (lambda (var)
		  `(with-environment (procedure-environment ,(car var))
		     (set! old-values (cons value old-values))
		     (set! value (convert ,(cadr var)))))
		vars))
       (lambda () 
         ,@body)
       (lambda ()
	 ,@(map (lambda (var)
		  `(with-environment (procedure-environment ,(car var))
		     (set! value (car old-values))
		     (set! old-values (cdr old-values))))
		vars))))


;; libraries
(apply define (symbol (object->string '(scheme base))) (environment) ()) ; ignore (scheme base)

(define-macro (define-library libname . body) ; |(lib name)| -> environment
  `(define ,(symbol (object->string libname))
     (with-environment (augment-environment (initial-environment) 
			 (cons 'import (symbol->value 'import))
			 (cons '*export* ())
			 (cons 'export (symbol->value 
					(define-macro (,(gensym) . names) 
					  `(set! *export* (append ',names *export*))))))
       ,@body
       (apply environment
	      (map (lambda (entry)
		     (if (or (member (car entry) '(*export* export import))
			     (and (pair? *export*)
				  (not (member (car entry) *export*))))
			 (values)
			 entry))
		   (current-environment))))))

(define-macro (import . libs)
  `(augment-environment! (current-environment)
     ,@(map (lambda (lib)
	      (case (car lib)
		((only) 
		 `((lambda (e names)
		     (apply environment 
			    (map (lambda (name)
				   (cons name (e name)))
				 names)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))
  
		((except)
		 `((lambda (e names)
		     (apply environment 
			    (map (lambda (entry)
				   (if (member (car entry) names)
				       (values)
				       entry))
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))
  
		((prefix)
		 `((lambda (e prefx)
		     (apply environment 
			    (map (lambda (entry)
				   (cons (string->symbol 
					  (string-append (symbol->string prefx) 
							 (symbol->string (car entry)))) 
					 (cdr entry)))
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (caddr ',lib)))
  
		((rename)
		 `((lambda (e names)
		     (apply environment 
			    (map (lambda (entry)
				   (let ((info (assoc (car entry) names)))
				     (if info
					 (cons (cadr info) (cdr entry))
					 entry))) ; I assume the un-renamed ones are included
				 e)))
		   (symbol->value (symbol (object->string (cadr ',lib))))
		   (cddr ',lib)))

		(else
		 `(let ((sym (symbol (object->string ',lib))))
		    (if (not (defined? sym))
			(format #t "~A not loaded~%" sym)
			(symbol->value sym))))))
	    libs)))


;; delay and force: ugh (hold your nose!)
;;   this implementation is based on the r7rs spec
(define-macro (delay-force expr) 
  `(make-promise #f (lambda () ,expr)))
(define-macro (r7rs-delay expr) ; "delay" is taken damn it
  `(delay-force (make-promise #t (lambda () ,expr))))
(define (make-promise done? proc) 
  (list (cons done? proc)))
(define (force promise)
  (if (caar promise)
      ((cdar promise))
      (let ((promise* ((cdar promise))))
        (if (not (caar promise))
            (begin
              (set-car! (car promise) (caar promise*))
              (set-cdr! (car promise) (cdar promise*))))
        (force promise))))


;; floor/ and truncate/ can't work as intended: they assume that multiple values 
;;   are not spliced.  The "division library" is a trivial, pointless micro-optimization.
;; and why no euclidean-rationalize or exact-integer-expt?
;;   (imagine what will happen when r8rs stumbles on the zoo of continued fraction algorithms!)
;;
;; get-environment-variable is a bad name: "environment" is already in use, and "get-"
;;   in any name should raise a red flag.  What about "home-environment"?

(let ((e (current-environment)))
  (c-define 
    '((in-C "static int g_time(void) {return((int)time(NULL));} \n\
             static struct timeval overall_start_time;  \n\
             static bool time_set_up = false;           \n\
             static double get_internal_real_time(void) \n\
             {                                          \n\
               struct timezone z0;                      \n\
               struct timeval t0;                       \n\
               double secs;                             \n\
               if (!time_set_up) {gettimeofday(&overall_start_time, &z0); time_set_up = true;} \n\
               gettimeofday(&t0, &z0);                  \n\
               secs = difftime(t0.tv_sec, overall_start_time.tv_sec);\n\
               return(secs + 0.000001 * (t0.tv_usec - overall_start_time.tv_usec)); \n\
             }")
      (double get_internal_real_time (void))
      (int g_time (void)))
    "" '("time.h" "sys/time.h"))
  (augment-environment! e 
    (cons 'jiffies-per-second (lambda () 1000))
    (cons 'current-jiffy (lambda () (round (* (get_internal_real_time) 1000.0))))
    (cons 'current-second g_time)))


(let ((e (current-environment)))
  (if (not (provided? 'libc.scm)) (load "libc.scm"))
  (augment-environment! e
    (cons 'get-environment-variable (*libc* 'getenv))
    (cons 'get-environment-variables (*libc* 'getenvs))
    (cons 'r7rs-file-exists? (lambda (arg) (= ((*libc* 'access) arg (*libc* 'F_OK)) 0)))))


;;; srfi 112
(let ((e (current-environment)))
  (if (not (provided? 'libc.scm)) (load "libc.scm"))
  (augment-environment! e 
    (cons 'os-type (lambda () (car ((*libc* 'uname)))))
    (cons 'cpu-architecture (lambda () (cadr ((*libc* 'uname)))))
    (cons 'machine-name (lambda () (caddr ((*libc* 'uname)))))
    (cons 'os-version (lambda () (string-append (list-ref ((*libc* 'uname)) 3) " " (list-ref ((*libc* 'uname)) 4))))
    (cons 'implementation-name (lambda () "s7"))
    (cons 'implementation-version (lambda () (substring (s7-version) 3 7)))))

;; command-line is problematic: s7 has no access to the caller's "main" function, and
;;   outside Windows, there's no reasonable way to get these arguments.

;; other minor differences: 
;;  in s7, single-quote can occur in a name
;;  s7 doesn't currently implement #\xxxx characters


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
				       (format #f "#<~A: ~{~A~^ ~}>" 
					       ',class-name
					       (map (lambda (slot)
						      (list (car slot) (cdr slot)))
						    obj)))))
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

;; records
(define-macro (define-record-type type make ? . fields)
  (let ((new-type (if (pair? type) (car type) type))
	(inherited (if (pair? type) `(list ,@(cdr type)) ())))
    `(begin
       (define-class ,new-type ,inherited
         (map (lambda (f) (if (pair? f) (car f) f)) ',fields))
       
       (define (,? obj)    ; perhaps the define-class type predicate should use this 
         (define (search-inherited obj type)
	   (define (search-inheritors objs type)
	     (and (pair? objs)
		  (or (search-inherited (car objs) type)
		      (search-inheritors (cdr objs) type))))
	   (or (eq? (obj 'class-name) type)
	       (search-inheritors (obj 'inherited) type)))
         (and (environment? obj)
	      (search-inherited obj ',new-type)))
       
       (define ,make 
         (let ((new-obj (copy ,new-type)))
	   ,@(map (lambda (slot)
		    `(set! (new-obj ',slot) ,slot))
		  (cdr make))
	   new-obj))
       
       ,@(map
	  (lambda (field)
	    (if (pair? field)
	        (if (null? (cdr field))
		    (values)
		    (if (null? (cddr field))
		        `(define (,(cadr field) obj)
                           (if (not (,? obj)) 
                               (error 'wrong-type-arg "~S should be a ~A" obj ',type))
                           (obj ',(car field)))
		        `(begin
			   (define (,(cadr field) obj)
			     (if (not (,? obj)) 
				 (error 'wrong-type-arg "~S should be a ~A" obj ',type))
                             (obj ',(car field)))
			   (define (,(caddr field) obj val)
			     (if (not (,? obj)) 
				 (error 'wrong-type-arg "~S should be a ~A" obj ',type))
                             (set! (obj ',(car field)) val)))))))
	  fields)
       
       ',new-type)))

;;; srfi 111:
(define-record-type box-type (box value) box? (value unbox set-box!))


