(define (trace var)
  (let* ((cur-access (symbol-access var))
         (cur-set    (and cur-access (cadr cur-access))))
    (set! (symbol-access var)
          (list (and cur-access (car cur-access))
                (lambda (symbol new-value) 
                        (format #t "~A set to ~A~%" symbol new-value) 
                        (if cur-set 
                            (cur-set symbol new-value)
                            new-value))
                (and cur-access (caddr cur-access))
                cur-access))))

(define (untrace var)
  (if (and (symbol-access var)
           (cdddr (symbol-access var)))
      (set! (symbol-access var) (cadddr (symbol-access var)))))
untrace


(define B 3)
B

B
B
3
(trace 'B)
(#f #<lambda (symbol new-value)> #f (#f #<lambda (symbol new
-value)> #f #f))
(set! B 7)
7
B
7

4

wrong-type-arg 
;symbol-access argument, 3, is an integer bu
t should be a symbol
;
; trace: (let ((cur-access (symbol-ac
cess v... ; var: 3
; (trace B)                              
     



(#f #<lambda (symbol new-value)> #f #f)
B
syntax-error 
;attempt to apply the boolean #f to (#<lambda 
(symbol new-value)> #f #f)?
;
; (#f #<lambda (symbol new-val
ue)> #f #f)     
; (#f #<lambda (symbol new-value)> #f #f)  
   



(trace 'A)
(#f #<lambda (symbol new-value)> #f #f)
(A 1 10)
1024

1024

1024



