#!/home/bil/snd-13/snd
!#

;; this is currently just a hack -- it can be used as follows
;;
;; gdb program | gdb-s7.scm
;;
;; where "program" has s7 embedded in it.
;; 
;; The script annotates the gdb output (stack traces in particular) inserting
;;   in bold-face whatever s7 values it finds.  These are otherwise simply
;;   pointers like args=0xda1c50, but this script turns that into  args[(1/12)]=0xda1c50
;;   (or whatever -- in this case "args" is a list holding the ratio 1/12).
;;   gdb interaction is unchanged -- the result looks like gdb acting normally,
;;   but printing out s7 values whenever they are encountered.
;;
;; The program that reads this script needs to accept input and print results -- the snd version
;;   mentioned above is built --without-gui.  Ideally I'd make an s7 terminal app like ruby. 
;;   If a version of the s7.html repl is used, write to stderr (*stderr*) not stdout.
;;
;; The script uses xdotool to communicate back to gdb.  This also could be simplified.
;;
;; another temporary assumption: the s7 interpreter is named "sc" or "s7"


(define escape (integer->char 27))
;(define (red-text str) (format *stderr* "~C[31m~A~C[0m" escape str escape))
(define (bold-text str) (format *stderr* "~C[1m~A~C[22m" escape str escape))

(define (gdb-output)
  (let ((line "")
	(text ""))
    ;; gather up gdb's response and return it
    (call-with-exit
     (lambda (return)
       (do ((c (read-char *stdin*) (read-char *stdin*)))
	   ((eof-object? c)
	    c)
	 (set! text (string-append text (string c)))
	 (if (char=? c #\newline)
	     (set! line "")
	     (begin
	       (set! line (string-append line (string c)))
	       (if (and (char=? c #\space)
			(or (string=? line "(gdb) ")
			    (string=? line "Quit anyway? (y or n) ")))
		   (return text)))))))))

(define (gdb-input)
  ;; echo the user typing immediately
  (do ((c (read-char *stdin*) (read-char *stdin*)))
      ((char=? c #\newline) 
       (write-char c *stderr*))
    (write-char c *stderr*)))

(define (gdb)
  (gdb-input)
  (gdb-output))


(define (annotate-gdb)
  (let ((text (gdb)))
    (let ((frame 0))

      ;; now look at the complete response one line at a time
      (call-with-input-string text
	(lambda (p)
	  (do ((line (read-line p #t) (read-line p #t))) ; #t=>include \n
	      ((eof-object? line))

	    ;; look for a frame number (we have to move to that frame to get the variable's value)
	    (if (char=? (line 0) #\#)
		(let ((sp (char-position #\space line)))
		  (set! frame (string->number (substring line 1 sp)))))

	    ;; look for sc -- all s7 calls will be of the form func(sc, args) presumably
	    (let ((pos (or (string-position "sc=0x" line)
			   (string-position "s7=0x" line)))
		  (out-pos 0))
	      (if pos

		  ;; found s7, look for args pointer(s)
		  (do ((epos (string-position "=0x" (substring line (+ pos 4))) 
			     (string-position "=0x" (substring line (+ pos 4))))
		       (s7-name (if (string-position "sc=0x" line) "sc" "s7")))
		      ((not epos))
		    (set! pos (+ epos pos 4))
		    
		    ;; found one, so move to the frame in question via xdotool
		    (system (format #f "xdotool type \"frame ~D\"" frame))
		    (system "xdotool key Return")
		    (gdb-output) ; toss the redundant output

		    ;; now use xdotool to make sure the presumed s7 pointer is actually one, then get its value
		    (let ((name-start pos))
		      (do ((i name-start (- i 1)))
			  ((or (char=? (line i) #\space) ; fix this! 
			       (= i 0))
			   (set! name-start (+ i 1))))
		      (let ((name (substring line name-start pos)))

			;; before calling s7_is_valid (which assumes it can derefence the pointer),
			;;   could we ask gdb if the pointer can be derefenced?  perhaps try to
			;;   print *ptr and look for an error message?
			;; or alternatively, in s7_is_valid, trap segfault then read the cell?
			;;   would this confuse gdb? unfortunately yes.

		       (system (format #f "xdotool type \"p s7_is_valid(~A, ~A)\"" s7-name name))
		       (system "xdotool key Return")
		       (let ((answer (gdb-output)))
			 (if (string-position "true" answer)
			     (begin

			       ;; get the s7 value of the pointed-to object
			       (system (format #f "xdotool type \"p s7_object_to_c_string(~A, ~A)\"" s7-name name))
			       (system "xdotool key Return")
			       (let ((value (gdb-output)))

				 ;; now strip away all the gdb junk, inserting just the value in the output
				 (let* ((start-pos (char-position #\" value))
					(end-pos (and start-pos (char-position #\" value (+ start-pos 1)))))
				   (if start-pos
				       (begin
					 (write-string (substring line out-pos pos) *stderr*)
					 (bold-text (format #f "[~A]" (substring value (+ start-pos 1) end-pos)))
					 (set! out-pos pos))))))))))))

	      ;; write out the unchanged end portion
	      (write-string (substring line out-pos) *stderr*))))))))


(catch #t
  (lambda ()
    (do () () (annotate-gdb)))
	
  (lambda args
    (if (not (eq? (car args) 'wrong-type-arg))
	(format *stderr* "oops: ~A~%" args))
    (exit)))


