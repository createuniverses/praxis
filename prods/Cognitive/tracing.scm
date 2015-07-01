(define (repeat n fn)
  (if (<= n 0)
      'done
      ((lambda () (fn)
                  (repeat (- n 1) fn)))))

(define-macro (trace-old f)
    `(define ,f 
       (apply lambda 'args 
         `((format #t "(~A ~{~A~^ ~}) -> " ',,f args)
           (let ((val (apply ,,f args))) 
             (format #t "~A~%" val) 
             val)))))
             
(define-macro (trace f)
  `(define ,f
     (apply lambda 'args
            `((format #t "(~A ~{~A~^ ~}) -> " ',',f args)
              (let ((val (apply ,,f args)))
                (format #t "~A~%" val)
                val)))))

(define *depth* 1)

(define-macro (trace-pp f)
  (let ((n (symbol->string f)))
    `(define ,f 
       (apply lambda 'args 
         `((repeat (- *depth* 1) (lambda () (format #t " |   ")))
           (if (>= *depth* 1) (format #t " /-- "))
           (format #t "(~A ~{~A~^ ~})~%" ,,n args)
           (repeat *depth* (lambda () (format #t " |   ")))
           (format #t "~%")
           (set! *depth* (+ *depth* 1))
           (let ((val (apply ,,f args))) 
             (set! *depth* (- *depth* 1))
             (repeat (- *depth* 1) (lambda () (format #t " |   ")))
             (if (>= *depth* 1) (format #t " \\-- "))
             (format #t "~A~%" val) 
             (repeat (- *depth* 1) (lambda () (format #t " |   ")))
             (format #t "~%")
             val))))))
;
(define (addone x) (+ 1 x))

(define thing (macroexpand (trace addone)))
;;(pp thing)

;(define addone2
; (apply lambda
;        'args
;        ({list} ({list} 'format #t \"(~A ~{~A~^ ~}) -> \" ({list} 'quote addone) 'args)
;                ({list} 'let ({list} ({list} 'val ({list} 'apply addone 'args))) '(format #t \"~A~%\" val)
;                        'val))))
;

