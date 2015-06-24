(define-macro (trace f)
    `(define ,f 
       (apply lambda 'args 
         `((format #t "(~A ~{~A~^ ~}) -> " ',,f args)
           (let ((val (apply ,,f args))) 
             (format #t "~A~%" val) 
             val)))))

(define (repeat n fn)
  (if (<= n 0)
      'done
      ((lambda () (fn)
                  (repeat (- n 1) fn)))))
