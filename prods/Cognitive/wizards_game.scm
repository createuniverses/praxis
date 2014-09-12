(define (remove-if func lst)
   (map (lambda (x) (if (func x) (values) x)) lst))

(define (remove-if-not func lst)
   (map (lambda (x) (if (not (func x)) (values) x)) lst))

(define (find expr lst)
  (let ((r (remove-if-not expr lst)))
     (if (eq? r '())
        #f
       (car r))))
       
(define-macro (if-let1 var expr then else)
  `(let ((,var ,expr))
     (if ,var ,then ,else)))

(define *nodes* '((living-room (you are in the living-room.
                                    a wizard is snoring loudly on the couch.))
                  (garden (you are in a beautiful garden.
                               there is a well in front of you.))
                  (attic (you are in the attic.
                              there is a giant welding torch in the corner.))))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define *edges* '((living-room (garden west door)  
                               (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges)))))

(define *objects* '(whiskey bucket frog chain))

(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(define (objects-at loc objs obj-loc)
  (define (is-at obj)
    (eq? (cadr (assoc obj obj-loc)) loc))
  (remove-if-not is-at objs))

(define (describe-objects loc objs obj-loc)
  (define (describe-obj obj)
    `(you see a ,obj on the floor.))
  (apply append (map describe-obj (objects-at loc objs obj-loc))))

(define *location* 'living-room)

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(define (walk direction)
  (define (correct-way edge)
    (eq? (cadr edge) direction))
  (if-let1 next (find correct-way (cdr (assoc *location* *edges*)))
    (begin (set! *location* (car next)) 
           (look))
    '(you cannot go that way.)))

(define *allowed-commands* '(look walk pickup inventory))

;; Here is a spel
(define-macro (walk2 dir) `(walk ',dir))


;; TODO

(define (pickup object)
  (cond [(memv object (objects-at *location* *objects* *object-locations*))
         (push! *object-locations* (list object 'body))
         `(you are now carrying the ,object)]
        [else '(you cannot get that.)]))

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(define (have object)
  (memv object (cdr (inventory))))

(define (game-repl)
  (let1 cmd (game-read)
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(define (game-eval sexp)
  (if (memv (car sexp) *allowed-commands*)
    (eval sexp (current-module)) ; Gauche specific
    '(i do not know that command.)))




