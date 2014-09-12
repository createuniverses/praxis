(use srfi-13) ;string-downcase


;; Gauche already has it
;;(defmacro let1 (var val &body body)
;;  `(let ((,var ,val))
;;     ,@body))

(define-macro (split val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if (not (null? ,g))
         (let ((head (car ,g))
               (tail (cdr ,g)))
           ,yes)
         ,no))))

(define (pairs lst)
  (define (f lst acc)
    (split lst    
           (if tail
             (f (cdr tail) (cons (cons head (car tail)) acc))
             (reverse acc))
           (reverse acc)))
  (f lst '()))

(define (print-tag name alst closingp)
  (display #\<)
  (when closingp
    (display #\/))
  (display (string-downcase (x->string name)))
  (dolist [att alst]
    (format #t " ~a=\"~a\"" (string-downcase (x->string (car att))) (cdr att)))
  (display #\>))

(define-macro (tag name atts . body)
  `(begin (print-tag ',name
                     (list ,@(map (^x `(cons ',(car x) ,(cdr x)))
                                  (pairs atts)))
                     #f)
          ,@body
          (print-tag ',name '() #t)))

(define-macro (svg width height . body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg" 
                   "xmlns:xlink" "http://www.w3.org/1999/xlink"
                   height ,height width ,width)
	,@body))

(define (brightness col amt)
  (map (^x (min 255 (max 0 (+ x amt)))) col))

(define (svg-style color)
  (apply format "fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)" 
         (append color (brightness color -100))))

(define (polygon points color)
  (tag polygon
       (points (string-concatenate
                (map (^[tp] (format "~a,~a " (car tp) (cdr tp))) points))
        style (svg-style color))))

#|
;; Random walk chart example
(use srfi-27) ; random-integer
(use srfi-42)

(define (random-walk value length)
  (if (zero? length)
    '()
    (cons value (random-walk (if (zero? (random-integer 2))
                               (+ value 1)
                               (- value 1))
                             (- length 1)))))

(with-output-to-file "random_walk.svg"
  (^[] (svg 400 200
            (do-ec (: i 10)
                   (polygon (append
                             '((0 . 200))
                             (list-ec (: y (index x) (random-walk 100 400))
                                      (cons x y))
                             '((400 . 200)))
                            (list-ec (: i 3) (random-integer 256))))))
  :if-exists :supersede)
|#