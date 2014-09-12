(define *padepth* 0)
*padepth*
(sleep)
(format #t "hello")
"hello"
(do ((i 0 (+ i 1)))
    ((= i -1))
    (format #t "~D" i)
    (sleep))
64


(define-bacro (procedure-annotate proc) ; use "bacro" so we can annotate local functions
  (let ((orig (procedure-source proc))) ; this assumes we haven't called "proc" yet

    (define (proc-walk source)
      (if (pair? source)
    (if (memq (car source) '(let let*))     ; if let or let*, show local variables
        (if (symbol? (cadr source))         ; named let?
      ;; (let name vars . body) -> (let name vars print-vars . body)
      (append 
       (list (car source)
       (cadr source)
       (caddr source)
       `(format #t "    (let ~A (~{~A~^ ~}) ...)~%" ,(cadr source) (current-environment)))
       (cdddr source))
      ;; (let(*) vars . body) -> (let vars print-vars . body)
      (append 
       (list (car source)
       (cadr source)
       `(format #t "    (~A (~{~A~^ ~}) ...)~%" ,(car source) (current-environment)))
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
           (begin (repeat *padepth* (lambda () (format #t " ")))
                  (set! *padepth* (+ *padepth* 1))
                  (format #t "(~A~{ ~A~})~%" 
                             ',proc 
                             (outer-environment (outer-environment (current-environment))))))
         (lambda ()
           (set! ,result (,new-body ,@(cadr orig))))
         (lambda ()       ; at exit, show result
           (if (eq? ,result #<undefined>)
             (begin (set! *padepth* (- *padepth* 1))
                    (repeat *padepth* (lambda () (format #t " ")))
                    (format #t " ~A returns early~%"))
             (begin (set! *padepth* (- *padepth* 1))
                    (repeat *padepth* (lambda () (format #t " ")))
                    (format #t " ~A returns ~A~%" ',proc ,result)))))))))

      `(define ,proc ,new-source))))
procedure-annotate




(define (repeat n fn)
  (if (<= n 0)
      'done
      ((lambda () (fn)
                  (repeat (- n 1) fn)))))

(begin (format #t "hello~%")
       (format #t "there~%"))

(repeat 3 (lambda () (format #t "hello~%")))
done


