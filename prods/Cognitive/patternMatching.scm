; atom? is not in a pair or null (empty)
(define (atom? x)
  (and (not (pair? x))
  (not (null? x))))

; Dictionaries 

(define (make-empty-dictionary) '())

(define (extend-dictionary pat dat dictionary)
  (let ((vname (variable-name pat)))
    (let ((v (assq vname dictionary)))
      (cond ((not v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))

(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (not v)
        var
        (cadr v))))

; Expressions

(define (compound? exp) (pair?   exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom?   exp))

; Patterns

(define (arbitrary-constant?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?c) #f))

(define (arbitrary-expression?  pattern)
  (if (pair? pattern) (eq? (car pattern) '? ) #f))

(define (arbitrary-variable?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?v) #f))

(define (variable-name pattern) (cadr pattern))

; Pattern Matching and Simplification

(define (match pattern expression dictionary)
  (cond ((and (null? pattern) (null? expression))
         dictionary)
        ((eq? dictionary 'failed) 'failed)
        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dictionary
                 'failed)
             'failed))
        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((arbitrary-expression? pattern)
         (extend-dictionary pattern expression dictionary))
        ((atom? expression) 'failed)
        (else
         (if (and (pair? pattern) (pair? expression))
             (match (cdr pattern)
                    (cdr expression)
                    (match (car pattern)
                           (car expression)
                           dictionary))
              'failed))))


; Skeletons & Evaluations

(define (skeleton-evaluation? skeleton)
  (if (pair? skeleton) (eq? (car skeleton) ':) #f))


(define (evaluation-expression evaluation) (cadr evaluation))

(define (instantiate skeleton dictionary)
  (cond ((null? skeleton) '())
        ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                   dictionary))
        (else (cons (instantiate (car skeleton) dictionary)
                    (instantiate (cdr skeleton) dictionary)))))

; Evaluate (dangerous magic)

(define (evaluate form dictionary)
  (if (atom? form)
      (lookup form dictionary)
      (apply (eval (lookup (car form) dictionary))
             (map (lambda (v) (lookup v dictionary))
                     (cdr form)))))


; Rules

(define (pattern  rule) (car  rule))
(define (skeleton rule) (cadr rule))

; Simplifier

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp   (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dictionary (match (pattern (car rules))
                                   exp
                                   (make-empty-dictionary))))
            (if (eq? dictionary 'failed)
                (scan (cdr rules))
                (simplify-exp (instantiate (skeleton (car rules))
                                           dictionary))))))
    (scan the-rules))
  simplify-exp)


; another way to write simplify-exp
(define (simplify-exp exp)
  (try-rules
    (if (compound? exp)
      (map simplify-exp exp)
      exp)))


; Symbolic Differentiation

(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )
    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
                                        (dd (: x2) (: v)))             )
    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
                                        (* (dd (: x1) (: v)) (: x2)))  )
    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n) (+ (: x) (: (- n 1))))
                                        (dd (: x) (: v)))              )
    ))

(define dsimp (simplifier deriv-rules))



;; Algebraic simplification

(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ))

(define asimp (simplifier algebra-rules))




(dsimp '(dd (+ x y) x))
(+ 1 0)

(+ 1 0)

(match '(+ (* (? x) (? y)) (? y)) '(+ (* 3 x) x) (make-empty-dictionary))
(dsimp '(dd (+ x y) x))

(dsimp '(dd (+ (+ x x) 1) x))

(+ (+ 1 1) 0)



(dsimp '(dd (+ (* a (* x x)) (* b x)) x))
(+ (+ (* a (+ (* x 1) (* 1 x))) (* 0 (* x x))) (+ (* b 1) (*
 0 x)))

(dd (+ (* a (* x x)) (* b x) c) x)

(match '(+ (* (? x) (? y)) (? y)) '(+ (* 3 x) x) (make-empty-dictionary))
((y x) (x 3))

;; x^2 -> 2x
(asimp (dsimp '(dd (* x x) x)))



(+ x x)

;; ax^2 -> 2ax
(asimp (dsimp '(dd (* a (* x x)) x)))
(+ (* a x) (* a x))

(+ (* a x) (* a x))

;; x^2 + x -> 2x + 1
(asimp (dsimp '(dd (+ (* x x) x) x)))
(+ 1 (+ x x))

;; ax^2 + bx -> 2ax + b
(asimp (dsimp '(dd (+ (* a (* x x)) (* b x)) x)))
(+ (* a x) (+ (* a x) b))

;; ax^2 + bx + c -> 2ax + b
(asimp (dsimp '(dd (+ (+ (* a (* x x)) (* b x)) c) x)))
(+ (* a x) (+ (* a x) b))

(dsimp '(dd (+ (+ (* a (* x x)) (* b x)) c) x))
(+ (+ (+ (* a (+ (* x 1) (* 1 x))) (* 0 (* x x))) (+ (* b 1)
 (* 0 x))) 0)


'(+ (* (? x) (? y)) (? y))

'(+ (* 3 x) x)

(match '(+ (* (? x) (? y)) (? y)) '(+ (* 3 x) x) (make-empty-dictionary))

(match '(+ (* (? x) (? y)) (? y)) '(+ (* 3 (+ 1 2)) (+ 1 2)) (make-empty-dictionary))

(evaluate '(+ x x) '((y x) (x 3)))

