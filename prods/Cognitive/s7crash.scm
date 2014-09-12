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
procedure-annotate

procedure-annotate

(define (addone x)
  (define (oneadder x) (+ 1 x))
  (oneadder x))
addone

addone

(addone 3)
4

4

(procedure-annotate addone)
#<lambda (x)>

#<lambda (x)>

(addone 3)
4

(define (countdown x)
  (define (countdowner x)
     (cond ((<= x 0) (list 0))
           (else (append (list x) (countdowner (- x 1))))))
  (countdowner x))


(define (countdown x)
  (define (countdowner x)
     (cond ((<= x 0) (list 0))
           (else (list x (countdowner (- x 1))))))
  (countdowner x))



(countdown 20)
(20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
(10 9 8 7 6 5 4 3 2 1 0)

(10 (9 (8 (7 (6 (5 (4 (3 (2 (1 (0)))))))))))

(procedure-annotate countdown)

(countdown 10)
; crash!
