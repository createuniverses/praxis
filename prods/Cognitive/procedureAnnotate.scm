(define-bacro (procedure-annotate proc) ; use "bacro" so we can annotate local functions
  (let ((orig (procedure-source proc)))

    (define (proc-walk source)
      (if (pair? source)
    (if (or (eq? (car source) 'let)     ; if let or let*, show local variables
      (eq? (car source) 'let*))
        (if (symbol? (cadr source))
      ;; (let name vars . body) -> (let name vars print-vars . body)
      (append 
       (list (car source)
       (cadr source)
       (caddr source)
       `(format #t "    (let ~A (~{~A~^ ~}) ...)~%" 
                                  ,(cadr source) (environment->list (current-environment))))
       (cdddr source))
      ;; (let(*) vars . body) -> (let vars print-vars . body)
      (append 
       (list (car source)
       (cadr source)
       `(format #t "    (~A (~{~A~^ ~}) ...)~%" 
                                  ,(car source) (environment->list (current-environment))))
       (cddr source)))
        (cons (proc-walk (car source))
        (proc-walk (cdr source))))
    source))

    (let* ((new-body (proc-walk orig))
     (result (gensym))
     (new-source 
      `(lambda ,(cadr orig)
         (let ((,result #<undefined>))
     (dynamic-wind
         (lambda ()       ; upon entry, show procedure name and args
           (format #t "(~A~{ ~A~})~%" 
                               ',proc 
                               (environment->list 
                                 (outer-environment 
                                   (outer-environment (current-environment))))))
         (lambda ()
           (set! ,result (,new-body ,@(cadr orig)))
           ,result)
         (lambda ()       ; at exit, show result
           (if (eq? ,result #<undefined>)
         (format #t "  ~A returns early~%" ',proc)
         (format #t "  ~A returns ~A~%" ',proc ,result))))))))

      `(set! ,proc (eval ,new-source)))))




(define (hi a) (let ((b 12)) (+ b a)))
(procedure-annotate hi)
(let ((x 32)) (+ 1 (hi x)))


(procedure-annotate deriv)
(procedure-annotate make-product)
(procedure-annotate make-sum)


(deriv '(+ (* a (* x x)) (* b x) c) 'x)
(+ (* a (+ x x)) b)


