(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
deriv

deriv

deriv


(deriv '(+ (* a (* x x)) (* b x) c) 'x)
(+ (* a (+ x x)) b)

(+ (* a (+ x x)) b)

(+ (* a (+ x x)) b)
*dbug*
((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b)

(car *dbug*)
(+ (* a (* x x)) (* b x) c)
(cadr *dbug*)
(* a (* x x))
(caddr *dbug*)
(* x x)
(cadddr *dbug*)
x

(format #t "hello")
"hello"

((* a #1=(* x x)) #1# x x a (* b x) x b)


((+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c) (+ 
(* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c) (+ #1=(
* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b)

(define (test a n)
  (let ((tmp a))
       (set! *dbug* (append *dbug* (list tmp))))
  (cond ((< n 0) 0)
        (else (test a (- n 1)))))
test

(test '(+ (* a (* x x)) (* b x) c) 4)
0

0
*dbug*
(#1=(+ (* a (* x x)) (* b x) c) #1# #1# #1# #1# #1#)
()
(#1=(+ (* a (* x x)) (* b x) c) #1# #1# #1# #1# #1#)
(list *dbug*)
((#1=(+ (* a (* x x)) (* b x) c) #1# #1# #1# #1# #1#))

(car *dbug*)
(+ (* a (* x x)) (* b x) c)

(set! *dbug* ())
()
()



;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

make-product

(deriv '(* x y) 'x)
y
(deriv '(* (* x y) (+ x 3)) 'x)
(+ (* x y) (* y (+ x 3)))
(deriv '(+ (* a (* x x)) (* b x) c) 'x)
(+ (* a (+ x x)) b)

(+ (* a (+ x x)) b)

(+ (* a (+ x x)) b)

(append *dbug* '(deriv '(+ (* a (* x x)) (* b x) c) 'x))
((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b 
deriv '(+ (* a (* x x)) (* b x) c) 'x)

(define *dbug* ())
()
*dbug*
(((((((((((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a
 #3# x b) (+ #4=(* a #5=(* x x)) #6=(* b x) c)) #4#) #5#) x)
 x) a) #6#) x) b)

(set! *dbug* ())
()

()
(set! *dbug* (append *dbug* (list '(+ (* a (* x x)) (* b x) c))))
((+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c) (+ 
(* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c))

((+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c) (+ 
(* a (* x x)) (* b x) c))

((+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) c))

((+ (* a (* x x)) (* b x) c))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
 (1 2 3 4) (+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b
 x) c) (+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b x) 
c))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
 (1 2 3 4) (+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b
 x) c) (+ (* a (* x x)) (* b x) c))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
 (1 2 3 4) (+ (* a (* x x)) (* b x) c) (+ (* a (* x x)) (* b
 x) c))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
 (1 2 3 4) (+ (* a (* x x)) (* b x) c))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
 (1 2 3 4))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)
)

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))

((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))

((1 2 3 4) (1 2 3 4) (1 2 3 4))

((1 2 3 4) (1 2 3 4))

*dbug*
((1 2 3 4))
((1 2 3 4))

((1 2 3 4))

((1 2 3 4))


((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b)


(define *dbug* '())
*dbug*
((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b)

(append *dbug* (list 4))
((+ #1=(* a #2=(* x x)) #3=(* b x) c) #1# #2# x x a #3# x b 
4)



(list '(2))
((2))

(())

(append (append '() 2) 3)
wrong-type-arg 
;append argument 1, 2, is an integer but sho
uld be a proper list
;
; '()                                
         
; (append (append '() 2) 3)                   



2

2

wrong-type-arg 
;append argument 1, 1, is an integer but sho
uld be a proper list
;
; (append 1 2)                       
         
; (append 1 2)                                



(+ (* a (+ x x)) b)
(procedure-source deriv)
(lambda (exp var) (cond ((number? exp) 0) ((variable? exp) (
if (same-variable? exp var) 1 0)) ((sum? exp) (make-sum (der
iv (addend exp) var) (deriv (augend exp) var))) ((product? e
xp) (make-sum (make-product (multiplier exp) (deriv (multipl
icand exp) var)) (make-product (deriv (multiplier exp) var) 
(multiplicand exp)))) (else (error "unknown expression type 
-- DERIV" exp))))

(list 1 2 3 4)
(1 2 3 4)
(append '(1 2 3) '(4 5 6))
(1 2 3 4 5 6)
(set! dbug '(1 2 3 4))

clearError()
continue()
(define *dbug* '(1 2 3 4))
*dbug*
(set! *dbug* '(3 4 5 6))
(3 4 5 6)
*dbug*


(+ (+ x x) b)

(+ a b)

1


newBuffer()
