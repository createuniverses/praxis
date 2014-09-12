(define *nodes* '((living-room (you are in the living-room.
                                    a wizard is snoring loudly on the couch.))
                  (garden (you are in a beautiful garden.
                               there is a well in front of you.))
                  (attic (you are in the attic.
                              there is a giant welding torch in the corner.))))
*nodes*

(define (describe-location location nodes)
  (cadr (assoc location nodes)))
describe-location

(define *edges* '((living-room (garden west door)  
                               (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))
*edges*

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
describe-path

(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges)))))
describe-paths

(define *objects* '(whiskey bucket frog chain))
*objects*

(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))
*object-locations*

(define (objects-at loc objs obj-loc)
  (define (is-at obj)
    (eq? (cadr (assoc obj obj-loc)) loc))
  (filter is-at objs))
objects-at
(objects-at *location*

  (define (is-at obj)
    (eq? (cadr (assoc obj *object-locations*)) *location*))
is-at
(is-at 'chain)
#f
#t

(define (objects-at loc objs obj-loc)
  (define (is-at obj)
    (eq? (cadr (assoc obj obj-loc)) loc))
  (remove-if-not is-at objs))
objects-at
(objects-at *location* *objects* *object-locations*)
(whiskey bucket)
(objects-at 'garden *objects* *object-locations*)
(frog chain)

(define (lt4 x) (< x 4))
lt4
(lt2 1)
#t
#f
#t
(remove-if-not (lambda (x) (< x 5)) '(1 2 3 4 5))
(1 2 3 4)
(5)
wrong-number-of-args
(4 5)
(#t #t #t)
(#t #t #t)
(#t)
syntax-error

(define (remove-if func lst)
   (map (lambda (x) (if (func x) (values) x)) lst))
(define (remove-if-not func lst)
   (map (lambda (x) (if (not (func x)) (values) x)) lst))
remove-if-not
remove-if
remove-if
remove-if

objects-at

(objects-at *location* *objects* *object-locations*)
syntax-error
wrong-type-arg
syntax-error
wrong-type-arg


(define (describe-obj obj)
  (append '(you see a) (list obj) '(on the floor.)))
describe-obj
(describe-obj 'wot)
(you see a wot on the floor.)

(define (describe-objects loc objs obj-loc)
  (define (describe-obj obj)
    (append '(you see a) (list obj) '(on the floor.)))
  (apply append (map describe-obj (objects-at loc objs obj-loc))))

(define *location* 'living-room)
*location*

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
(look)
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)

(describe-location *location* *nodes*)
(you are in the living-room. a wizard is snoring loudly on the couch.)
(describe-paths *location* *edges*)
(there is a door going west from here. there is a ladder going upstairs from here.)
(describe-objects *location* *objects* *object-locations*)
syntax-error


syntax-error

(append '(a) '(b) '(c))
(a b c)
wrong-type-arg

look

(let ((a 3)) a)
3
syntax-error
syntax-error
syntax-error

(eq? 1 1)
#t
(define-macro (if-let1 var expr then else)
  `(let ((,var ,expr))
     (if ,var ,then ,else)))
if-let1
if-let1
if-let1
if-let1

(macroexpand   (if-let1 next (find correct-way (cdr (assoc *location* *edges*)))
    (begin (set! *location* (car next)) 
           (look))
    '(you cannot go that way.)))
(let ((next (find correct-way (cdr (assoc *location* *edges*)))))
  (if next (begin (set! *location* (car next)) (look))
           '(you cannot go that way.)))
(let1 next (find correct-way (cdr (assoc *location* *edges*)))
  (if next (begin (set! *location* (car next)) (look))
          '(you cannot go that way.)))


syntax-error
wrong-type-arg
wrong-type-arg
1
3
3
wrong-type-arg
wrong-type-arg
1
(remove-if-not (lambda (x) (< x 4)) '(1 2 3 4 5 6))
(1 2 3)
(5 6)
5
syntax-error

(list? '())
#t
#t
(car '())
wrong-type-arg

(eq? '() '(1 2 3))
#f
#t
(if '(1 2 3) 1 2)
1
1
1

(define (find expr lst)
  (let ((r (remove-if-not expr lst)))
     (if (eq? r '())
        #f
       (car r))))
find

(remove-if-not (lambda (x) (< x 4)) '(4 5 6))
()

(if (eq? '() (remove-if-not (lambda (x) (< x 4)) '(3 4 5 6))) 99 88)
88
99
99
99
1
1


(find (lambda (x) (< x 4)) '(2 3 4 5 6))
2
3
#f
wrong-type-arg
3


find
find

(define-macro (let1 
(let1 a 3)
syntax-error

(define (walk direction)
  (define (correct-way edge)
    (eq? (cadr edge) direction))
  (if-let1 next (find correct-way (cdr (assoc *location* *edges*)))
    (begin (set! *location* (car next)) 
           (look))
    '(you cannot go that way.)))
walk
walk
walk
walk

(look)
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
(walk 'west)
(you are in a beautiful garden. there is a well in front of you. there is a door going east from here. you see a frog on the floor. you see a chain on the floor.)
(you cannot go that way.)
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
(you cannot go that way.)
(you are in the attic. there is a giant welding torch in the corner. there is a ladder going downstairs from here.)
(you cannot go that way.)
wrong-type-arg
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
(you are in the attic. there is a giant welding torch in the corner. there is a ladder going downstairs from here.)
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
(you are in a beautiful garden. there is a well in front of you. there is a door going east from here. you see a frog on the floor. you see a chain on the floor.)
wrong-type-arg
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
wrong-type-arg
wrong-type-arg
wrong-number-of-args
wrong-type-arg
wrong-type-arg
wrong-type-arg
wrong-type-arg
(you are in the living-room. a wizard is snoring loudly on the couch. there is a door going west from here. there is a ladder going upstairs from here. you see a whiskey on the floor. you see a bucket on the floor.)
(you are in a beautiful garden. there is a well in front of you. there is a door going east from here. you see a frog on the floor. you see a chain on the floor.)
wrong-type-arg
syntax-error
syntax-error
syntax-error
syntax-error
syntax-error

(pickup 'whiskey)
syntax-error


(define (pickup object)
  (cond [(memv object (objects-at *location* *objects* *object-locations*))
         (push! *object-locations* (list object 'body))
         `(you are now carrying the ,object)]
        [else '(you cannot get that.)]))
pickup

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
inventory

(define (have object)
  (memv object (cdr (inventory))))
have

(define (game-repl)
  (let1 cmd (game-read)
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(define (game-read)
  ;; Gauche specific - Gauche's REPL reader doesn't consume the newline after
  ;; the input, so (game-read) immediately after invoking (game-repl) reads
  ;; the empty input.  We detect the case and keep reading.
  (let1 cmd (read-from-string (string-append "(" (read-line) ")"))
    (define (quote-it x)
      (list 'quote x))
    (if (null? cmd)
      (game-read)
      (cons (car cmd) (map quote-it (cdr cmd))))))

(define *allowed-commands* '(look walk pickup inventory))
*allowed-commands*

(define (game-eval sexp)
  (if (memv (car sexp) *allowed-commands*)
    (eval sexp (current-module)) ; Gauche specific
    '(i do not know that command.)))
game-eval

(define (tweak-text lst caps lit)
  (if (null? lst)
    '()
    (let ([item (car lst)]
          [rest (cdr lst)])
      (cond [(eqv? item #\space) (cons item (tweak-text rest caps lit))]
            [(memv item '(#\! #\? #\.)) (cons item (tweak-text rest #t lit))]
            [(eqv? item #\") (tweak-text rest caps (not lit))]
            [lit (cons item (tweak-text rest #f lit))]
            [caps (cons (char-upcase item) (tweak-text rest #f lit))]
            [else (cons (char-downcase item) (tweak-text rest #f #f))]))))
tweak-text

(define (game-print lst)
  ($ display
     $ list->string
     $ tweak-text (string->list (string-trim-both (write-to-string lst) #[() ]))
                  #t #f)
  (newline))
read-error

