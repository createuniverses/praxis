(load "./wizards_game")

(define-macro (game-action command subj obj place . body)
  `(define ,command
     (rlet1 ,command
       (^[subject object]
         (if (and (eq? *location* ',place)
                  (eq? subject ',subj)
                  (eq? object ',obj)
                  (have ',subj))
           ,@body
           '(i cant ,command like that.)))
       (unless (memv ',command *allowed-commands*)
         (push! *allowed-commands* ',command)))))

(define *chain-welded* #f)

(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
               (begin (set! *chain-welded* #t)
                      '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(define *bucket-filled* #f)

(game-action dunk bucket well garden
             (if *chain-welded* 
               (begin (set! *bucket-filled* #t)
                      '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond [(not *bucket-filled*) '(the bucket has nothing in it.)]
                   [(have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.)]
                   [else '(the wizard awakens from his slumber and greets you warmly. 
                               he hands you the magic low-carb donut- you win! the end.)]))


