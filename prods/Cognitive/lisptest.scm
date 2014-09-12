(define (mul a b) (* a b))

(define (factorial n)
  ;;(format #t "~A~%" (environment->list (current-environment)))
  (if (= 1 n) 1
      (mul n (factorial (- n 1)))))

(define (factorial n)
  ;;(format #t "~A~%" (environment->list (current-environment)))
  (if (= 1 n) 1
      (* n (factorial (- n 1)))))

(factorial 10)
(with-output-to-string (lambda () (factorial 10)))

(define (ack x y)
  ;;(format #t "~A~%" (environment->list (current-environment)))
  ;;(format #t "(ack ~A ~A)~%" x y)
  ;;(format #t "~A~%" (stacktrace))
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ack (- x 1)
                   (ack x (- y 1))))))
ack


(ack 1 10)
(with-output-to-string (lambda () (ack 1 10)))


(define (1+ x) (+ 1 x))
1+

(define (1- x) (- x 1))
1-



(define (add-rec a b)
  (if (= a 0)
      b
      (1+ (add-rec (1- a) b))))
add-rec

add-rec

(add-rec 4 5)

(define (add-iter a b)
  (if (= a 0)
      b
      (add-iter (- a 1) (+ b 1))))
(define (repeat n fn)
  (if (= n 0)
      'done
      ((lambda () (fn)
                  (repeat (- n 1) fn)))))

(repeat 5 (lambda () (format #t "hello~%")))

(add-iter 4 5)
9

syntax-error 
;1-: unbound variable
;
; add-iter: (1- a)                             ; a: 4
; add-iter: ((1+ b))                           ; b: 5
; (add-iter 4 5)                              



9





