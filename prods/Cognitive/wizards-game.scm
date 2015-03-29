
; Required Common Lisp functions

(define (first l) (car l))
(define (second l) (car (cdr l)))
(define (third l) (car (cdr (cdr l))))

(define (remove-if func lst)
   (map (lambda (x) (if (func x) (values) x)) lst))

(define (remove-if-not func lst)
   (map (lambda (x) (if (not (func x)) (values) x)) lst))

(define-macro (push i l) `(set! ,l (append (list ,i) ,l)))

; Alternative defspel definitions
;(define defspel define-macro)
;(define defspel define-macro*)

; Wizard adventure

(define *objects* '(whiskey-bottle bucket frog chain))

(define *map* '((living-room
                 (you are in the living-room of a wizard's house. there is a wizard snoring loudly on the couch.)
                 (west door garden)
                 (upstairs stairway attic))
                (garden
                 (you are in a beautiful garden. there is a well in front of you.)
                 (east door living-room))
                (attic
                 (you are in the attic of the abandoned house. there is a giant welding torch in the corner.)
                 (downstairs stairway living-room))))

(define *object-locations* '((whiskey-bottle living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(define *location* 'living-room)

(define (describe-location location themap)
  (second (assoc location themap)))

(define (describe-path path)
  `(there is a ,(second path) going ,(first path) from here.))

(define (describe-paths location themap)
  (apply append (map describe-path (cddr (assoc location themap)))))

(define (is-at obj loc obj-loc)
  (eq? (second (assoc obj obj-loc)) loc))

(define (describe-floor loc objs obj-loc)
  (apply append (map (lambda (x) `(you see a ,x on the floor.))
                     (remove-if-not (lambda (x) (is-at x loc obj-loc))
                                    objs))))

(define (look)
  (append (describe-location *location* *map*)
          (describe-paths *location* *map*)
          (describe-floor *location* *objects* *object-locations*)))

(define (walk-direction direction)
  (let ((next (assoc direction (cddr (assoc *location* *map*)))))
    (cond (next (set! *location* (third next)) (look))
          (#t '(you cant go that way.)))))

(define-macro* (defspel :rest rest) `(define-macro* ,@rest))

(defspel (walk direction) `(walk-direction ',direction))

(define (pickup-object object)
  (cond ((is-at object *location* *object-locations*) (push (list object 'body) *object-locations*) `(you are now carrying the ,object))
        (#t '(you cannot get that.))))

(defspel (pickup object)
  `(pickup-object ',object))

(define (inventory)
  (remove-if-not (lambda (x)
                   (is-at x 'body *object-locations*))
                 *objects*))

(define (have object)
  (member object (inventory)))

(define *chain-welded* #f)

(define *bucket-filled* #f)

(defspel (game-action command subj obj place :rest rest)
  `(defspel (,command subject object)
     `(cond ((and (eq? *location* ',',place)
                  (eq? ',subject ',',subj)
                  (eq? ',object ',',obj)
                  (have ',',subj))
             ,@',rest)
            (#t '(i cant ,',command like that.)))))

(game-action weld chain bucket attic
             (cond ((and (have 'bucket) (set! *chain-welded* #t)) '(the chain is now securely welded to the bucket.))
                   (#t '(you do not have a bucket.))))

(game-action dunk bucket well garden
             (cond (*chain-welded* (set! *bucket-filled* #t) '(the bucket is now full of water))
                   (#t '(the water level is too low to reach.))))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end.))
                   (#t '(the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end.))))
 