#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	     :renamer (symbol-prefix-proc 'srfi-19:)))

;;;#############################################################
;;;#############################################################
;;;### expects 2 julian days (plain numbers)
;;;### differences between 2 julian days is in days (or a fraction of a day)
(define-public (julian-day-difference-to-string dend dstart)
  (define (local-process-sub-day day-fraction)
    (let ((nsecs (* day-fraction 24.0 60.0 60.0))
	  (nmins (truncate (* day-fraction 24.0 60.0)))
	  (nhours (truncate (* day-fraction 24.0))))
      (let ((nminutes
	     (* 0.0010 (truncate (* 1000.0 (- nmins (* nhours 60.0)))))))
	(let ((nseconds
	       (* 0.0010
		  (truncate
		   (* 1000.0 (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0))))))))
	  (begin
	    (if (<= nhours 0.0)
		(if (<= nminutes 0.0)
		    (format #f "~a seconds" nsecs)
		    (format #f "~a minutes, ~a seconds" nminutes nseconds))
		(if (<= nminutes 0.0)
		    (format #f "~a hours, ~a seconds" nhours nseconds)
		    (format #f "~a hours, ~a minutes, ~a seconds" nhours nminutes nseconds))
		))))))
  (if (and (number? dend) (number? dstart))
      (begin
	(let ((jd-diff (exact->inexact (- dend dstart))))
	  (if (< jd-diff 1.0)
	      (begin
		(let ((tstring (local-process-sub-day jd-diff)))
		  tstring
		  ))
	      (begin
		(let ((ndays (truncate jd-diff)))
		  (let ((dfract-diff (- jd-diff ndays)))
		    (let ((tstring (local-process-sub-day dfract-diff)))
		      (let ((ttstring (format #f "~a days, ~a" ndays tstring)))
			ttstring
			))))))))
      #f))

;;;#############################################################
;;;#############################################################
(define-public (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
(define (initialize-random-state)
  (let ((time (gettimeofday)))
    (set! *random-state*
	  (seed->random-state (+ (car time)
				 (cdr time))))
    ))

;;;#############################################################
;;;#############################################################
(define (roll-two-dice max-num)
  (let ((dnum1 (inexact->exact
		(truncate (+ 1.0 (random max-num)))))
	(dnum2 (inexact->exact
		(truncate (+ 1.0 (random max-num))))))
    (let ((sum (+ dnum1 dnum2)))
      (if (= dnum1 dnum2)
	  (list sum #t)
	  (list sum #f))
      )))

;;;#############################################################
;;;#############################################################
(define (display-random-two-dice-tests max-rolls)
  (let ((max-dice-num 6)
	(results-htable (make-hash-table 20)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-rolls))
	(begin
	  (let ((roll-list (roll-two-dice max-dice-num)))
	    (let ((this-roll (car roll-list)))
	      (let ((this-count (hash-ref results-htable this-roll 0)))
		(begin
		  (hash-set! results-htable this-roll (1+ this-count))
		  ))))
	  ))
      (hash-for-each
       (lambda(key value)
	 (display (ice9-format:format #f "(~a) roll = ~:d~%" key value)))
       results-htable)
      )))

;;;#############################################################
;;;#############################################################
;;; data-list format
;;; board format (list place-number place-code count total pcnt-double)
;;; checked - 12/23/2011, 12/28/2011, 3/11/2012
(define (initialize-list-data)
  (let ((code-list
	 (list
	  (list 0 "go" 0 0 0.0) (list 1 "a1" 0 0 0.0)
	  (list 2 "cc1" 0 0 0.0) (list 3 "a2" 0 0 0.0)
	  (list 4 "t1" 0 0 0.0) (list 5  "r1"   0 0 0.0)
	  (list 6 "b1"   0 0 0.0) (list 7 "ch1"   0 0 0.0)
	  (list 8 "b2"   0 0 0.0) (list 9 "b3"   0 0 0.0)
	  (list 10 "jail"  0 0 0.0) (list 11 "c1"   0 0 0.0)
	  (list 12 "u1"   0 0 0.0) (list 13 "c2"   0 0 0.0)
	  (list 14 "c3"   0 0 0.0) (list 15 "r2"   0 0 0.0)
	  (list 16 "d1"   0 0 0.0) (list 17 "cc2"   0 0 0.0)
	  (list 18 "d2"   0 0 0.0) (list 19 "d3"   0 0 0.0)
	  (list 20 "fp"   0 0 0.0) (list 21 "e1"   0 0 0.0)
	  (list 22 "ch2"  0 0 0.0) (list 23 "e2"   0 0 0.0)
	  (list 24 "e3"   0 0 0.0) (list 25 "r3"   0 0 0.0)
	  (list 26 "f1"   0 0 0.0) (list 27 "f2"   0 0 0.0)
	  (list 28 "u2"   0 0 0.0) (list 29 "f3"   0 0 0.0)
	  (list 30 "g2j"  0 0 0.0) (list 31 "g1"   0 0 0.0)
	  (list 32 "g2"   0 0 0.0) (list 33 "cc3"  0 0 0.0)
	  (list 34 "g3"   0 0 0.0) (list 35 "r4"   0 0 0.0)
	  (list 36 "ch3"  0 0 0.0) (list 37 "h1"   0 0 0.0)
	  (list 38 "t2"   0 0 0.0) (list 39 "h2"   0 0 0.0)
	  )))
    code-list
    ))

;;;#############################################################
;;;#############################################################
;;; cc-deck format (list op-code go-to-destination code-name)
(define (initialize-community-chest-deck)
  (let ((cc-list
	 (list
	  (list 1 0 "go")
	  (list 2 10 "jail")
	  (list 3 -1 "nop")
	  (list 4 -1 "nop")
	  (list 5 -1 "nop")
	  (list 6 -1 "nop")
	  (list 7 -1 "nop")
	  (list 8 -1 "nop")
	  (list 9 -1 "nop")
	  (list 10 -1 "nop")
	  (list 11 -1 "nop")
	  (list 12 -1 "nop")
	  (list 13 -1 "nop")
	  (list 14 -1 "nop")
	  (list 15 -1 "nop")
	  (list 16 -1 "nop"))
	 ))
    (let ((nlist-items (length cc-list)))
      (let ((remaining-list (copy-tree cc-list))
	    (remaining-items nlist-items)
	    (final-list (list)))
	(begin
	  (do ((ii 0 (1+ ii)))
	      ((>= ii nlist-items))
	    (begin
	      (if (> remaining-items 1)
		  (begin
		    (let ((this-index
			   (inexact->exact
			    (truncate (random remaining-items)))))
		      (let ((this-item (list-ref remaining-list this-index)))
			(begin
			  (set! final-list (cons this-item final-list))
			  (set! remaining-list (delete1! this-item remaining-list))
			  (set! remaining-items (1- remaining-items))
			  ))))
		  (begin
		    (let ((this-item (car remaining-list)))
		      (begin
			(set! final-list (cons this-item final-list))
			(delete1! this-item remaining-list)
			(set! remaining-items (1- remaining-items))
			))))
	      ))

	  (reverse final-list)
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; cc-deck format (list op-code go-to-destination code-name)
(define (initialize-chance-deck)
  (let ((move-list
	 (list
	  (list 1 0 "go")
	  (list 2 10 "jail")
	  (list 3 11 "c1")
	  (list 4 24 "e3")
	  (list 5 39 "h2")
	  (list 6 5 "r1")
	  (list 7 -1 "r")
	  (list 8 -1 "r")
	  (list 9 -1 "u")
	  (list 10 -3 "back 3")
	  (list 11 -1 "nop")
	  (list 12 -1 "nop")
	  (list 13 -1 "nop")
	  (list 14 -1 "nop")
	  (list 15 -1 "nop")
	  (list 16 -1 "nop")
	  )))
    (let ((nlist-items (length move-list)))
      (let ((remaining-list (copy-tree move-list))
	    (remaining-items nlist-items)
	    (final-list (list)))
	(begin
	  (do ((ii 0 (1+ ii)))
	      ((>= ii nlist-items))
	    (begin
	      (if (> remaining-items 1)
		  (begin
		    (let ((this-index
			   (inexact->exact
			    (truncate (random remaining-items)))))
		      (let ((this-item (list-ref remaining-list this-index)))
			(begin
			  (set! final-list (cons this-item final-list))
			  (set! remaining-list (delete1! this-item remaining-list))
			  (set! remaining-items (1- remaining-items))
			  ))))
		  (begin
		    (let ((this-item (car remaining-list)))
		      (begin
			(set! final-list (cons this-item final-list))
			(delete1! this-item remaining-list)
			(set! remaining-items (1- remaining-items))
			))))
	      ))

	  (reverse final-list)
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify simulation loop
;;; board format (list place-number place-code count total pcnt-double)
(define-syntax update-position-data
  (syntax-rules ()
    ((update-position-data board-list max-length new-position)
     (begin
       (let ((this-elem (list-ref board-list new-position)))
	 (let ((next-count (1+ (list-ref this-elem 2))))
	   (begin
	     (list-set! this-elem 2 next-count)
	     (list-set! board-list new-position this-elem)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; (list place-number place-code count total pcnt-double)
(define (test-update-position-data-1)
  (let ((sub-name "test-update-position-data-1")
	(test-list
	 (list
	  (list
	   (list (list 0 "go" 0 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0))
	   0
	   (list (list 0 "go" 1 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0)))
	  (list
	   (list (list 0 "go" 0 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0))
	   1
	   (list (list 0 "go" 0 0 0.0) (list 1 "a1" 1 0 0.0) (list 2 "a2" 0 0 0.0)))
	  (list
	   (list (list 0 "go" 0 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0))
	   2
	   (list (list 0 "go" 0 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 1 0 0.0)))
	  (list
	   (list (list 0 "go" 1 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0))
	   0
	   (list (list 0 "go" 2 0 0.0) (list 1 "a1" 0 0 0.0) (list 2 "a2" 0 0 0.0)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((board-list (list-ref a-list 0))
		 (new-position (list-ref a-list 1))
		 (shouldbe-list (list-ref a-list 2)))
	     (let ((old-board-list (copy-tree board-list))
		   (board-length (length board-list)))
	       (begin
		 (update-position-data board-list board-length new-position)

		 (if (not (equal? shouldbe-list board-list))
		     (begin
		       (display (format #f "~a : (~a) : error : for board = ~a, position = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index old-board-list new-position
					shouldbe-list board-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify simulation loop
(define-syntax top-card-to-bottom
  (syntax-rules ()
    ((top-card-to-bottom card-list)
     (begin
       (let ((top-card (car card-list))
	     (tail-list (cdr card-list)))
	 (let ((next-list (append tail-list (list top-card))))
	   (begin
	     (set! card-list next-list)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-top-card-to-bottom-1)
  (let ((sub-name "test-top-card-to-bottom-1")
	(test-list
	 (list
	  (list
	   (list (list 15 -1 "nop") (list 1 0 "go") (list 2 10 "jail"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 15 -1 "nop"))
	   )))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((test-deck (list-ref a-list 0))
		 (shouldbe-deck (list-ref a-list 1)))
	     (let ((old-test-deck (copy-tree test-deck)))
	       (begin
		 (top-card-to-bottom test-deck)

		 (if (not (equal? shouldbe-deck test-deck))
		     (begin
		       (display (format #f "~a : (~a) : error : for deck = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index old-test-deck
					shouldbe-deck result-deck))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; board format (list place-number place-code count total pcnt-double)
;;; speed up the simulation by hard-wiring the goto-next r or u cards
;;; (r-pos-list (list 5 15 25 35))
;;; (u-pos-list (list 12 28)))
(define (find-next-position board-list board-length
			    current-position code-string)
  (let ((next-position -1))
    (begin
      (if (string-ci=? code-string "r")
	  (begin
	    (cond
	     ((< current-position 5)
	      (set! next-position 5))
	     ((< current-position 15)
	      (set! next-position 15))
	     ((< current-position 25)
	      (set! next-position 25))
	     ((< current-position 35)
	      (set! next-position 35))
	     (else
	      (set! next-position 5)
	      ))
	    ))
      (if (string-ci=? code-string "u")
	  (begin
	    (cond
	     ((< current-position 12)
	      (set! next-position 12))
	     ((< current-position 28)
	      (set! next-position 28))
	     (else
	      (set! next-position 12)
	      ))
	    ))
      next-position
      )))

;;;#############################################################
;;;#############################################################
;;; board format (list place-number place-code count total pcnt-double)
(define (test-find-next-position-1)
  (let ((sub-name "test-find-next-position-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list 0 "r" 5) (list 1 "r" 5) (list 2 "r" 5) (list 3 "r" 5)
	  (list 4 "r" 5) (list 5 "r" 15) (list 16 "r" 25)
	  (list 26 "r" 35) (list 36 "r" 5) (list 39 "r" 5)
	  (list 1 "u" 12) (list 11 "u" 12) (list 12 "u" 28)
	  (list 29 "u" 12)
	  ))
	(test-label-index 0))
    (let ((board-length (length board-list)))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (let ((curr-pos (list-ref a-list 0))
		   (code-string (list-ref a-list 1))
		   (shouldbe-position (list-ref a-list 2)))
	       (let ((result-position (find-next-position
				       board-list board-length
				       curr-pos code-string)))
		 (begin
		   (if (not (equal? shouldbe-position result-position))
		       (begin
			 (display (format #f "~a : (~a) : error : initial position = ~a, code-string = ~a, shouldbe position = ~a, result = ~a~%"
					  sub-name test-label-index curr-pos code-string
					  shouldbe-position result-position))
			 (quit)
			 ))
		   )))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	))))

;;;#############################################################
;;;#############################################################
;;; community-chest-deck format (list op-code go-to-destination code-name)
(define-syntax handle-community-chest
  (syntax-rules ()
    ((handle-community-chest board-list board-length
			     position-counter next-position
			     consecutive-doubles-count
			     community-deck)
     (begin
       (let ((top-card (car community-deck)))
	 (let ((dest (list-ref top-card 1))
	       (code-name (list-ref top-card 2)))
	   (begin
	     (cond
	      ((string-ci=? code-name "go")
	       (begin
		 (set! position-counter dest)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ((string-ci=? code-name "jail")
	       (begin
		 (set! consecutive-doubles-count 0)
		 (set! position-counter dest)
		 (update-position-data board-list board-length position-counter)
		 ))
	      (else
	       (begin
		 (set! position-counter next-position)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ))))
       (top-card-to-bottom community-deck)
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (test-handle-community-chest-1)
  (let ((sub-name "test-handle-community-chest-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   0 5 0
	   0 0 (list 0 "go" 1 0 0.0))
	  (list
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 5 0
	   0 0 (list 0 "go" 1 0 0.0))
	  (list
	   (list (list 2 10 "jail") (list 3 -1 "nop") (list 1 0 "go"))
	   1 7 1
	   10 0 (list 10 "jail" 1 0 0.0))
	  (list
	   (list (list 2 10 "jail") (list 3 -1 "nop") (list 1 0 "go"))
	   12 18 1
	   10 0 (list 10 "jail" 1 0 0.0))
	  ))
	(test-label-index 0))
    (let ((board-length (length board-list)))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (let ((cc-deck (list-ref a-list 0))
		   (position-counter (list-ref a-list 1))
		   (next-position (list-ref a-list 2))
		   (consecutive-doubles-count (list-ref a-list 3))
		   (shouldbe-position (list-ref a-list 4))
		   (shouldbe-consecutive-doubles-count (list-ref a-list 5))
		   (shouldbe-list (list-ref a-list 6))
		   (this-board (copy-tree board-list)))
	       (begin
		 (handle-community-chest this-board board-length
					 position-counter next-position
					 consecutive-doubles-count
					 cc-deck)

		 (if (not (equal? shouldbe-position position-counter))
		     (begin
		       (display (format #f "~a : (~a) : error : position counter, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index
					shouldbe-position position-counter))
		       (quit)
		       ))
		 (if (not (equal? shouldbe-consecutive-doubles-count consecutive-doubles-count))
		     (begin
		       (display (format #f "~a : (~a) : error : consecutive-doubles-count, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index
					shouldbe-consecutive-doubles-count consecutive-doubles-count))
		       (quit)
		       ))
		 (let ((result-list (list-ref this-board shouldbe-position)))
		   (if (not (equal? shouldbe-list result-list))
		       (begin
			 (display (format #f "~a : (~a) : error : board list at position ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index shouldbe-position
					  shouldbe-list result-list))
			 (quit)
			 )))
		 ))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	))))

;;;#############################################################
;;;#############################################################
;;; chance-deck format (list op-code go-to-destination code-name)
(define-syntax handle-chance
  (syntax-rules ()
    ((handle-chance board-list board-length
		    position-counter next-position
		    consecutive-doubles-count
		    chance-deck cc-deck)
    (begin
       (let ((top-card (car chance-deck)))
	 (let ((dest (list-ref top-card 1))
	       (code-name (list-ref top-card 2)))
	   (begin
	     (cond
	      ((string-ci=? code-name "go")
	       (begin
		 (set! position-counter dest)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ((string-ci=? code-name "jail")
	       (begin
		 (set! consecutive-doubles-count 0)
		 (set! position-counter dest)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ((or (string-ci=? code-name "c1")
		   (string-ci=? code-name "e3")
		   (string-ci=? code-name "h2")
		   (string-ci=? code-name "r1"))
	       (begin
		 (set! position-counter dest)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ((or (string-ci=? code-name "r")
		   (string-ci=? code-name "u"))
	       (begin
		 (let ((next-position-counter
			(find-next-position board-list board-length
					    position-counter code-name)))
		   (begin
		     (set! position-counter next-position-counter)
		     (update-position-data board-list board-length position-counter)
		     ))
		 ))
	      ((string-ci=? code-name "back 3")
	       (begin
		 (let ((next-position-counter (- next-position 3)))
		   (begin
		     (if (< next-position-counter 0)
			 (set! next-position-counter (+ next-position-counter board-length)))

		     (cond
		      ((= next-position-counter 30)
		       (begin
			 ;;; go to jail
			 (set! consecutive-doubles-count 0)
			 (set! next-position-counter 10)
			 ))
		      ((or (= next-position-counter 2)
			   (= next-position-counter 17)
			   (= next-position-counter 33))
		       (begin
			 ;;; chance deck says go back 3 spaces, and we land on community chest
			 (handle-community-chest board-list board-length
						 position-counter next-position-counter
						 consecutive-doubles-count
						 cc-deck)
			 ))
		      (else
		       (begin
			 (set! position-counter next-position-counter)
			 (update-position-data board-list board-length position-counter)
			 ))
		      ))
		   )))
	      (else
	       (begin
		 (set! position-counter next-position)
		 (update-position-data board-list board-length position-counter)
		 ))
	      ))))
       (top-card-to-bottom chance-deck)
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; board format (list place-number place-code count total pcnt-double)
(define (test-handle-chance-1)
  (let ((sub-name "test-handle-chance-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
          ;;; test 0
	  (list
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   0 5 0
	   0 0 (list 0 "go" 1 0 0.0))
	  (list
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 12 0
	   0 0 (list 0 "go" 1 0 0.0))
	  (list
	   (list (list 2 10 "jail") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 12 1
	   10 0 (list 10 "jail" 1 0 0.0))
	  (list
	   (list (list 2 10 "jail") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   12 20 1
	   10 0 (list 10 "jail" 1 0 0.0))
	  (list
	   (list (list 3 11 "c1") (list 7 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 10 0
	   11 0 (list 11 "c1" 1 0 0.0))
          ;;; test 5
	  (list
	   (list (list 3 11 "c1") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   12 15 0
	   11 0 (list 11 "c1" 1 0 0.0))
	  (list
	   (list (list 4 24 "e3") (list 7 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 15 0
	   24 0 (list 24 "e3" 1 0 0.0))
	  (list
	   (list (list 4 24 "e3") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   25 35 0
	   24 0 (list 24 "e3" 1 0 0.0))
	  (list
	   (list (list 5 39 "h2") (list 7 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 5 0
	   39 0 (list 39 "h2" 1 0 0.0))
	  (list
	   (list (list 5 39 "h2") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   39 5 0
	   39 0 (list 39 "h2" 1 0 0.0))
          ;;; test 10
	  (list
	   (list (list 6 5 "r1") (list 7 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 6 0
	   5 0 (list 5 "r1" 1 0 0.0))
	  (list
	   (list (list 6 5 "r1") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   6 7 0
	   5 0 (list 5 "r1" 1 0 0.0))
	  (list
	   (list (list 7 -1 "r") (list 7 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 8 0
	   5 0 (list 5 "r1" 1 0 0.0))
	  (list
	   (list (list 7 -1 "r") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   6 9 0
	   15 0 (list 15 "r2" 1 0 0.0))
	  (list
	   (list (list 7 -1 "r") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   16 10 0
	   25 0 (list 25 "r3" 1 0 0.0))
          ;;; test 15
	  (list
	   (list (list 7 -1 "r") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   26 31 0
	   35 0 (list 35 "r4" 1 0 0.0))
	  (list
	   (list (list 8 -1 "r") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   36 38 0
	   5 0 (list 5 "r1" 1 0 0.0))
	  (list
	   (list (list 9 -1 "u") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   1 39 0
	   12 0 (list 12 "u1" 1 0 0.0))
	  (list
	   (list (list 9 -1 "u") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   13 23 0
	   28 0 (list 28 "u2" 1 0 0.0))
	  (list
	   (list (list 9 -1 "u") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   29 33 0
	   12 0 (list 12 "u1" 1 0 0.0))
          ;;; test 20
	  (list
	   (list (list 10 -3 "back 3") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   5 7 0
	   4 0 (list 4 "t1" 1 0 0.0))
	  (list
	   (list (list 10 -3 "back 3") (list 3 -1 "nop") (list 1 0 "go"))
	   (list (list 1 0 "go") (list 2 10 "jail") (list 3 -1 "nop"))
	   39 7 0
	   4 0 (list 4 "t1" 1 0 0.0))
	  ))
	(test-label-index 0))
    (let ((board-length (length board-list)))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (let ((chance-deck (list-ref a-list 0))
		   (cc-deck (list-ref a-list 1))
		   (position-counter (list-ref a-list 2))
		   (next-position (list-ref a-list 3))
		   (consecutive-doubles-count (list-ref a-list 4))
		   (shouldbe-position (list-ref a-list 5))
		   (shouldbe-consecutive-doubles-count (list-ref a-list 6))
		   (shouldbe-list (list-ref a-list 7))
		   (this-board (copy-tree board-list)))
	       (begin
		 (handle-chance this-board board-length
				position-counter next-position
				consecutive-doubles-count
				chance-deck cc-deck)

		 (if (not (equal? shouldbe-position position-counter))
		     (begin
		       (display (format #f "~a : (~a) : error : position counter, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index
					shouldbe-position position-counter))
		       (quit)
		       ))
		 (if (not (equal? shouldbe-consecutive-doubles-count consecutive-doubles-count))
		     (begin
		       (display (format #f "~a : (~a) : error : consecutive-doubles-count, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index
					shouldbe-consecutive-doubles-count consecutive-doubles-count))
		       (quit)
		       ))
		 (let ((result-list (list-ref this-board shouldbe-position)))
		   (if (not (equal? shouldbe-list result-list))
		       (begin
			 (display (format #f "~a : (~a) : error : board list at position ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index shouldbe-position
					  shouldbe-list result-list))
			 (quit)
			 )))
		 ))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	))))

;;;#############################################################
;;;#############################################################
;;; board format (list place-number place-code count total pcnt-double)
(define (calculate-percentiles board-list num-orbits)
  (let ((final-list
	 (map
	  (lambda (alist)
	    (let ((count (list-ref alist 2)))
	      (begin
		(list-set! alist 3 num-orbits)
		(let ((pcnt (* 100.0
			       (/ count num-orbits))))
		  (begin
		    (list-set! alist 4 pcnt)
		    ))
		alist
		)))
	  board-list)))
    (begin
      final-list
      )))

;;;#############################################################
;;;#############################################################
;;; board format (list place-number place-code count total pcnt-double)
(define (test-calculate-percentiles-1)
  (let ((sub-name "test-calculate-percentiles-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list
	   (list
	    (list 0 "a1" 2 0 0.0)
	    (list 1 "a2" 3 0 0.0)
	    (list 2 "a3" 4 0 0.0))
	   10
	   (list
	    (list 0 "a1" 2 10 20.0)
	    (list 1 "a2" 3 10 30.0)
	    (list 2 "a3" 4 10 40.0))
	   )))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((board-list (list-ref a-list 0))
		 (num-orbits (list-ref a-list 1))
		 (shouldbe-list-list (list-ref a-list 2)))
	     (begin
	       (let ((result-list-list (calculate-percentiles board-list num-orbits)))
		 (let ((shouldbe-length (length shouldbe-list-list))
		       (result-length (length result-list-list)))
		   (begin
		     (if (not (equal? shouldbe-length result-length))
			 (begin
			   (display (format #f "~a : (~a) : error : board list = ~a, num-orbits = ~a, shouldbe list = ~a, result = ~a : shouldbe length = ~a, result length = ~a~%"
					    sub-name test-label-index board-list num-orbits
					    shouldbe-list-list result-list-list
					    shouldbe-length result-length))
			   (quit)
			   ))

		     (do ((ii 0 (1+ ii)))
			 ((>= ii shouldbe-length))
		       (begin
			 (let ((shouldbe-alist (list-ref shouldbe-list-list ii))
			       (result-alist (list-ref result-list-list ii)))
			   (begin
			     (if (not (equal? shouldbe-alist result-alist))
				 (begin
				   (display (format #f "~a : (~a) : error : board list = ~a, num-orbits = ~a, shouldbe list = ~a, result = ~a : error in position ~a, shouldbe = ~a, result = ~a~%"
						    sub-name test-label-index board-list num-orbits
						    shouldbe-list-list result-list-list
						    ii shouldbe-alist result-alist))
				   (quit)
				   ))
			     ))
			 ))
		     )))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (top-three-list-items board-list)
  (let ((result-list (list)))
    (let ((sorted-board-list
	   (sort-list board-list
		      (lambda(a b) (> (list-ref a 2) (list-ref b 2))))
	   ))
      (begin
	(do ((ii 0 (1+ ii)))
	    ((>= ii 3))
	  (begin
	    (set! result-list
		  (append result-list (list (list-ref sorted-board-list ii))))
	    ))
	result-list
	))
    ))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (test-top-three-list-items-1)
  (let ((sub-name "test-top-three-list-items-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list
	   (list
	    (list 0 "a1" 2 0 0.0)
	    (list 1 "a2" 3 0 0.0)
	    (list 2 "a3" 4 0 0.0)
	    (list 3 "a4" 1 0 0.0)
	    (list 4 "a5" 1 0 0.0)
	    (list 5 "a6" 5 0 0.0))
	   (list
	    (list 5 "a6" 5 0 0.0)
	    (list 2 "a3" 4 0 0.0)
	    (list 1 "a2" 3 0 0.0))
	   )))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((board-list (list-ref a-list 0))
		 (shouldbe-list-list (list-ref a-list 1)))
	     (begin
	       (let ((result-list-list (top-three-list-items board-list)))
		 (let ((shouldbe-length (length shouldbe-list-list))
		       (result-length (length result-list-list)))
		   (begin
		     (if (not (equal? shouldbe-length result-length))
			 (begin
			   (display (format #f "~a : (~a) : error : board list = ~a, shouldbe list = ~a, result = ~a : shouldbe length = ~a, result length = ~a~%"
					    sub-name test-label-index board-list
					    shouldbe-list-list result-list-list
					    shouldbe-length result-length))
			   (quit)
			   ))

		     (do ((ii 0 (1+ ii)))
			 ((>= ii shouldbe-length))
		       (begin
			 (let ((shouldbe-alist (list-ref shouldbe-list-list ii))
			       (result-alist (list-ref result-list-list ii)))
			   (begin
			     (if (not (equal? shouldbe-alist result-alist))
				 (begin
				   (display (format #f "~a : (~a) : error : board list = ~a, shouldbe list = ~a, result = ~a : error in position ~a, shouldbe = ~a, result = ~a~%"
						    sub-name test-label-index board-list
						    shouldbe-list-list result-list-list
						    ii shouldbe-alist result-alist))
				   (quit)
				   ))
			     ))
			 ))
		     )))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (top-three-list-to-number-code three-list)
  (let ((result-string ""))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii 3))
	(begin
	  (let ((this-elem (list-ref three-list ii)))
	    (let ((this-num (list-ref this-elem 0)))
	      (let ((num-string (ice9-format:format #f "~2,'0d" this-num)))
		(begin
		  (set! result-string (string-append result-string num-string))
		  ))
	      ))
	  ))
      result-string
      )))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (test-top-three-list-to-number-code-1)
  (let ((sub-name "test-top-three-list-to-number-code-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list
	   (list
	    (list 5 "a6" 5 0 0.0)
	    (list 2 "a3" 4 0 0.0)
	    (list 1 "a2" 3 0 0.0))
	   "050201")
	  (list
	   (list
	    (list 5 "a6" 5 0 0.0)
	    (list 2 "a3" 4 0 0.0)
	    (list 11 "a2" 3 0 0.0))
	   "050211")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((three-list (list-ref a-list 0))
		 (shouldbe-string (list-ref a-list 1)))
	     (let ((result-string (top-three-list-to-number-code three-list)))
	       (begin
		 (if (not (equal? shouldbe-string result-string))
		     (begin
		       (display (format #f "~a : (~a) : error : three list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index three-list
					shouldbe-string result-string))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))


;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (simulation-loop board-list max-dice-rolls max-dice-num)
  (let ((board-length (length board-list))
	(chance-deck (initialize-chance-deck))
	(community-deck (initialize-community-chest-deck))
	(position-counter 0)
	(consecutive-doubles-count 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii max-dice-rolls))
	(begin
	  (let ((roll-list (roll-two-dice max-dice-num)))
	    (let ((roll-sum (list-ref roll-list 0))
		  (is-double (list-ref roll-list 1)))
	      (let ((next-position
		     (modulo (+ position-counter roll-sum) board-length)))
		(begin
		  (if (equal? is-double #t)
		      (begin
			(set! consecutive-doubles-count (1+ consecutive-doubles-count)))
		      (begin
			(set! consecutive-doubles-count 0)
			))

		  (if (>= consecutive-doubles-count 3)
		      (begin
			;;; go directly to jail
			(set! consecutive-doubles-count 0)
			(set! position-counter 10)
			(update-position-data board-list board-length position-counter))
		      (begin
                        ;;; (list place-number place-code count total-orbits pcnt-double)
			(let ((this-elem (list-ref board-list next-position)))
			  (let ((position-code-name (list-ref this-elem 1)))
			    (begin
                              ;;; handle exceptions (community chest, chance, and goto jail)
			      (cond
			       ((string-ci=? position-code-name "g2j")
				(begin
				  (set! consecutive-doubles-count 0)
				  (set! position-counter 10)
				  (update-position-data board-list board-length position-counter)
				  ))
			       ((not (equal? (string-contains-ci position-code-name "cc") #f))
				(begin
                                  ;;; community chest
				  (handle-community-chest board-list board-length
							  position-counter next-position
							  consecutive-doubles-count
							  community-deck)
				  ))
			       ((not (equal? (string-contains-ci position-code-name "ch") #f))
				(begin
                                  ;;; chance
				  (handle-chance board-list board-length
						 position-counter next-position
						 consecutive-doubles-count
						 chance-deck community-deck)
				  ))
			       (else
				(begin
				  (set! position-counter next-position)
				  (update-position-data board-list board-length position-counter)
				))
			       ))
			    ))
			))
		  ))
	      ))
	  ))

      (if (> (list-ref (list-ref board-list 30) 2) 0)
	  (begin
	    (display (format #f "warning goto jail board results = ~a~%" (list-ref board-list 30)))
	    (force-output)
	    ))

      (calculate-percentiles board-list max-dice-rolls)
      (copy-tree board-list)
      )))

;;;#############################################################
;;;#############################################################
(define (calc-std-error float-list)
  (let ((flength (length float-list)))
    (begin
      (if (> flength 1)
	  (begin
	    (let ((favg (/ (srfi-1:fold + 0.0 float-list) flength))
		  (flen-m1 (- flength 1.0)))
	      (let ((ssq (/ (srfi-1:fold (lambda (this-num prev)
					   (+ (* this-num this-num) prev))
					 0.0 float-list)
			    flen-m1))
		    (favg2 (/ (* flength favg favg) flen-m1)))
		(let ((std-err (sqrt (- ssq favg2))))
		  (begin
		    (if (< ssq favg2)
			(begin
			  (display (format #f "calc-std-error : error for list ~a, avg^2=~a, ssq=~a~%" float-list favg2 ssq))
			  (force-output)))
		    std-err
		    )))
	      ))
	  (begin
	    -1
	    ))
      )))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
(define (test-calc-std-error-1)
  (let ((sub-name "test-calc-std-error-1")
	(board-list (initialize-list-data))
	(test-list
	 (list
	  (list (list 1.0 2.0 3.0) 1.0)
	  (list (list 1.0 2.0 3.0 4.0) 1.29099)
	  ))
	(test-label-index 0)
	(tolerance 1e-5))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((float-list (list-ref a-list 0))
		 (shouldbe-std-err (list-ref a-list 1)))
	     (let ((result-std-err (calc-std-error float-list)))
	       (begin
		 (if (> (abs (- shouldbe-std-err result-std-err)) tolerance)
		     (begin
		       (display (format #f "~a : (~a) : error : float list = ~a, shouldbe = ~a, result = ~a, difference = ~a, tolerance = ~a~%"
					sub-name test-label-index float-list
					shouldbe-std-err result-std-err
					(- shouldbe-std-err result-std-err) tolerance))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-single-board-element a-list)
  (let ((place-number (list-ref a-list 0))
	(place-name (list-ref a-list 1))
	(count (list-ref a-list 2))
	(total (list-ref a-list 3))
	(pcnt-list (list-ref a-list 4)))
    (let ((favg (/ (srfi-1:fold + 0.0 pcnt-list) (length pcnt-list)))
	  (fstd-err (calc-std-error pcnt-list)))
      (begin
	(display (ice9-format:format
		  #f "    ~a (~1,2f%) = square ~2,'0d  (~:d out of ~:d, std error = ~1,4e)~%"
		  place-name favg
		  place-number count total fstd-err))
	))
    ))

;;;#############################################################
;;;#############################################################
;;; board-list format (list place-number place-code count total pcnt-double)
;;; with-error-list format (list place-number place-code cummulative-count cummulative-totals pcnt-list)
(define (main-std-error-loop board-list max-dice-rolls split-num max-dice-num)
  (let ((result-htable (make-hash-table split-num)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii split-num))
	(begin
	  (let ((this-board (copy-tree board-list)))
	    (let ((revised-board-list (simulation-loop this-board max-dice-rolls max-dice-num)))
	      (begin
		(for-each
		 (lambda (a-list)
		   (let ((place-number (list-ref a-list 0))
			 (place-name (list-ref a-list 1))
			 (count (list-ref a-list 2))
			 (total (list-ref a-list 3))
			 (pcnt-double (list-ref a-list 4)))
		     (let ((this-list (hash-ref result-htable place-name
						(list place-number place-name 0 0 (list)))))
		       (begin
			 (list-set! this-list 2 (+ count (list-ref this-list 2)))
			 (list-set! this-list 3 (+ total (list-ref this-list 3)))
			 (list-set! this-list 4
				    (cons
				     (* 100.0 (/ count total))
				     (list-ref this-list 4)))
			 (hash-set! result-htable place-name this-list)
			 ))))
		 revised-board-list)))
	    )))

      (let ((result-list (hash-map->list (lambda(key value) value) result-htable)))
	(let ((sorted-list (sort result-list
				 (lambda (a b)
				   (> (list-ref a 2) (list-ref b 2)))
				 )))
	  (let ((top-three-list (list-head sorted-list 3))
		(last-four-list (list-tail sorted-list (- (length board-list) 4))))
	    (let ((top-three-code (top-three-list-to-number-code top-three-list)))
	      (begin
		(display (format #f "three most popular squares~%"))
		(for-each
		 (lambda (a-list)
		   (begin
		     (display-single-board-element a-list)
		     )) top-three-list)

		(display (format #f "three most popular squares modal string = ~a~%" top-three-code))
		(force-output)

		(display (format #f "least popular squares~%"))
		(for-each
		 (lambda (a-list)
		   (begin
		     (display-single-board-element a-list)
		     )) last-four-list)
		(force-output)
		)))
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax run-test
  (syntax-rules ()
    ((run-test test-function counter)
     (begin
       (test-function)
       (set! counter (1+ counter))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax time-code
  (syntax-rules ()
    ((time-code body)
     (begin
       (let ((start-jday (srfi-19:current-julian-day)))
	 (begin
	   body

	   (let ((end-jday (srfi-19:current-julian-day)))
	     (begin
	       (display (format #f "elapsed time = ~a : ~a~%"
				(julian-day-difference-to-string end-jday start-jday)
				(date-time-to-string (srfi-19:current-date))))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Problem 084 - In the game, Monopoly, the standard board is set up in the following way:~%"))
    (newline)
    (display (format #f "  GO  A1  CC1  A2  T1  R1  B1  CH1  B2  B3  JAIL~%"))
    (display (format #f "  H2                                         C1~%"))
    (display (format #f "  T2                                         U1~%"))
    (display (format #f "  H1                                         C2~%"))
    (display (format #f "  CH3                                        C3~%"))
    (display (format #f "  R4                                         R2~%"))
    (display (format #f "  G3                                         D1~%"))
    (display (format #f "  CC3                                       CC2~%"))
    (display (format #f "  G2                                         D2~%"))
    (display (format #f "  G1                                         D3~%"))
    (display (format #f "  G2J  F3  U2  F2  F1  R3  E3  E2  CH2  E1   FP~%"))
    (newline)
    (display (format #f "A player starts on the GO square and adds the scores on two 6-sided dice to determine the number of squares they advance in a clockwise direction. Without any further rules we would expect to visit each square with equal probability: 2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH (chance) changes this distribution.~%"))
    (newline)
    (display (format #f "In addition to G2J, and one card from each of CC and CH, that orders the player to go directly to jail, if a player rolls three consecutive doubles, they do not advance the result of their 3rd roll. Instead they proceed directly to jail.~%"))
    (newline)
    (display (format #f "At the beginning of the game, the CC and CH cards are shuffled. When a player lands on CC or CH they take a card from the top of the respective pile and, after following the instructions, it is returned to the bottom of the pile. There are sixteen cards in each pile, but for the purpose of this problem we are only concerned with cards that order a movement; any instruction not concerned with movement will be ignored and the player will remain on the CC/CH square.~%"))
    (newline)
    (display (format #f "Community Chest (2/16 cards):~%"))
    (display (format #f "  1. Advance to GO~%"))
    (display (format #f "  2. Go to JAIL~%"))
    (display (format #f "Chance (10/16 cards):~%"))
    (display (format #f "  1. Advance to GO~%"))
    (display (format #f "  2. Go to JAIL~%"))
    (display (format #f "  3. Go to C1~%"))
    (display (format #f "  4. Go to E3~%"))
    (display (format #f "  5. Go to H2~%"))
    (display (format #f "  6. Go to R1~%"))
    (display (format #f "  7. Go to next R (railway company)~%"))
    (display (format #f "  8. Go to next R~%"))
    (display (format #f "  9. Go to next U (utility company)~%"))
    (display (format #f " 10. Go back 3 squares.~%"))
    (newline)
    (display (format #f "The heart of this problem concerns the likelihood of visiting a particular square. That is, the probability of finishing at that square after a roll. For this reason it should be clear that, with the exception of G2J for which the probability of finishing on it is zero, the CH squares will have the lowest probabilities, as 5/8 request a movement to another square, and it is the final square that the player finishes at on each roll that we are interested in. We shall make no distinction between 'Just Visiting' and being sent to JAIL, and we shall also ignore the rule about requiring a double to 'get out of jail', assuming that they pay to get out on their next turn.~%"))
    (newline)
    (display (format #f "By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate these two-digit numbers to produce strings that correspond with sets of squares.~%"))
    (newline)
    (display (format #f "Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00. So these three most popular squares can be listed with the six-digit modal string: 102400.~%"))
    (newline)
    (display (format #f "If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-update-position-data-1 counter)
	   (run-test test-top-card-to-bottom-1 counter)
	   (run-test test-find-next-position-1 counter)
	   (run-test test-handle-community-chest-1 counter)
	   (run-test test-handle-chance-1 counter)
	   (run-test test-calculate-percentiles-1 counter)
	   (run-test test-top-three-list-items-1 counter)
	   (run-test test-top-three-list-to-number-code-1 counter)
	   (run-test test-calc-std-error-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (let ((max-tests 10000))
      (begin
	(time-code
	 (begin
	   (display-random-two-dice-tests max-tests)
	   ))
	))

    (display (format #f "Output:~%"))
    (newline)
    (force-output)

    (let ((board-list (initialize-list-data))
	  (max-dice-rolls 100000)
	  (split-num 1000)
	  (max-dice-num 6))
      (begin
	(time-code
	 (begin
	   (initialize-random-state)
	   (display (format #f "6-sided dice~%"))
	   (main-std-error-loop board-list max-dice-rolls split-num max-dice-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((board-list (initialize-list-data))
	  (max-dice-rolls 100000)
	  (split-num 1000)
	  (max-dice-num 4))
      (begin
	(time-code
	 (begin
	   (initialize-random-state)
	   (display (format #f "4-sided dice~%"))
	   (main-std-error-loop board-list max-dice-rolls split-num max-dice-num)
	   ))
	))

    (newline)
    ))
