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
(define (calc-number-of-rectangles grid-rows grid-cols)
  (begin
    (let ((term1 (euclidean/ (* (1+ grid-rows) grid-rows) 2))
          (term2 (euclidean/ (* (1+ grid-cols) grid-cols) 2)))
      (let ((nblocks (* term1 term2)))
        (begin
          nblocks
          )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-calc-number-of-rectangles-1)
  (let ((sub-name "test-calc-number-of-rectangles-1")
	(test-list
	 (list
          (list 1 1 1)
          (list 1 2 3)
          (list 1 3 6)
          (list 1 4 10)
	  (list 2 3 18)
          (list 2 4 30)
	  (list 3 3 36)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((grid-rows (list-ref a-list 0))
		 (grid-cols (list-ref a-list 1))
		 (shouldbe (list-ref a-list 2)))
	     (let ((result (calc-number-of-rectangles
			    grid-rows grid-cols)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : grid r/c = ~a/~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index grid-rows grid-cols
                         shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (loop-over-grids max-grid-rows max-grid-cols target-count)
  (let ((min-count -1)
	(min-rows -1)
	(min-cols -1)
	(min-difference target-count)
	(second-count -1)
	(second-rows -1)
	(second-cols -1)
	(second-difference target-count)
	(break-flag #f))
    (begin
      (do ((grid-rows 1 (1+ grid-rows)))
	  ((or (> grid-rows max-grid-rows)
	       (equal? break-flag #t)))
	(begin
	  (do ((grid-cols grid-rows (1+ grid-cols)))
	      ((or (> grid-cols max-grid-cols)
		   (equal? break-flag #t)))
	    (begin
	      (let ((this-count
                     (calc-number-of-rectangles grid-rows grid-cols)))
		(let ((this-difference
                       (abs (- target-count this-count))))
		  (begin
		    (cond
		     ((< this-difference min-difference)
		      (begin
			(set! second-count min-count)
			(set! second-difference min-difference)
			(set! second-rows min-rows)
			(set! second-cols min-cols)

			(set! min-count this-count)
			(set! min-difference this-difference)
			(set! min-rows grid-rows)
			(set! min-cols grid-cols)
			))
		     ((< this-difference second-difference)
		      (begin
			(set! second-count this-count)
			(set! second-difference this-difference)
			(set! second-rows grid-rows)
			(set! second-cols grid-cols)
			)))

		    (if (= this-difference 0)
			(set! break-flag #t))
		    )))
	      ))
	  ))

      (list min-count min-rows min-cols min-difference
	    second-count second-rows second-cols second-difference)
      )))

;;;#############################################################
;;;#############################################################
;;;  (list min-count min-rows min-cols min-difference)
(define (main-loop max-grid-rows max-grid-cols target-count)
  (let ((best-count (loop-over-grids max-grid-rows max-grid-cols target-count)))
    (let ((min-count (list-ref best-count 0))
	  (min-rows (list-ref best-count 1))
	  (min-cols (list-ref best-count 2))
	  (min-difference (list-ref best-count 3))
	  (second-count (list-ref best-count 4))
	  (second-rows (list-ref best-count 5))
	  (second-cols (list-ref best-count 6))
	  (second-difference (list-ref best-count 7)))
      (begin
	(display (ice9-format:format
		  #f "grid [~:d, ~:d], area = ~:d : contains ~:d rectangles : difference of ~:d from ~:d~%"
		  min-rows min-cols (* min-rows min-cols) min-count
		  min-difference target-count))
	(display (ice9-format:format
		  #f "next grid [~:d, ~:d], area = ~:d : contains ~:d rectangles : difference of ~:d from ~:d~%"
		  second-rows second-cols (* second-rows second-cols) second-count
		  second-difference target-count))

	(force-output)
	))
    ))

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
    (display (format #f "Problem 085 - By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:~%"))
    (newline)
    (display (format #f "Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.~%"))
    (newline)
    (display (format #f "This problem can be solved by two independent sums, one over the min range and one over the maximum range.~%"))
    (display (format #f "Over the min range, one needs to sum over the size of the blocks as well as the position of the block. Sum_size=1_to_n(Sum_pos=1_to_(n-size+1) (1)) = Sum_size=1_to_n((n-size+1)) = (n+1)*n/2.  The sum over the maximum is similar.~%"))
    (display (format #f "The number of rectangles in an nxm grid is (n+1)*n/2 * (m+1)*m/2~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-number-of-rectangles-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (newline)
    (force-output)

    (let ((max-grid-rows 10)
	  (max-grid-cols 10)
	  (target-count 100))
      (begin
	(main-loop max-grid-rows max-grid-cols target-count)
	))

    (newline)
    (force-output)

    (let ((max-grid-rows 100)
	  (max-grid-cols 100)
	  (target-count 2000000))
      (begin
	(time-code
	 (begin
	   (main-loop max-grid-rows max-grid-cols target-count)
	   ))
	))

    (newline)
    ))
