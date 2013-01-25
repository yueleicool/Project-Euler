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

;;; srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;#############################################################
;;;#############################################################
;;;### expects 2 julian days (plain numbers)
;;;### differences between 2 julian days is in days (or a fraction of a day)
(define (julian-day-difference-to-string dend dstart)
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
(define (current-date-time-string)
  (let ((this-datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (count-cubes-in-layer this-layer init-xx init-yy init-zz)
  (let ((l1 (* 2 (+ (* init-xx init-yy) (* init-xx init-zz)
		    (* init-yy init-zz))))
	(delta (* 4 (+ init-xx init-yy init-zz))))
    (begin
      (cond
       ((< this-layer 1) #f)
       ((= this-layer 1) l1)
       ((= this-layer 2) (+ l1 delta))
       (else
	(begin
	  (let ((ntmp (1- this-layer)))
	    (let ((ntmp-2 (* 4 (1- ntmp) ntmp)))
	      (let ((result (+ l1 (* ntmp delta) ntmp-2)))
		(begin
		  result
		  ))
	      ))
	  )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-cubes-in-layer-1)
  (let ((sub-name "test-count-cubes-in-layer-1")
	(test-list
	 (list
	  (list 1 3 2 1 22) (list 2 3 2 1 46)
	  (list 3 3 2 1 78) (list 4 3 2 1 118)
	  (list 1 5 1 1 22) (list 1 5 3 1 46)
	  (list 1 7 2 1 46) (list 1 11 1 1 46)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((level-number (list-ref alist 0))
		 (init-xx (list-ref alist 1))
		 (init-yy (list-ref alist 2))
		 (init-zz (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (let ((result (count-cubes-in-layer level-number
						 init-xx init-yy init-zz)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : level number = ~a, xx = ~a, yy = ~a, zz = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index level-number
					init-xx init-yy init-zz
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
(define-syntax update-hash-lists
  (syntax-rules ()
    ((update-hash-lists levels-htable loop-flag
			ll xx yy zz max-l1-limit)
     (begin
       (let ((lcount (count-cubes-in-layer ll xx yy zz)))
	 (begin
	   (if (<= lcount max-l1-limit)
	       (begin
		 (let ((llist (hash-ref levels-htable lcount (list))))
		   (begin
		     (hash-set! levels-htable lcount
				(cons (list ll xx yy zz) llist))
		     )))
	       (begin
		 (set! loop-flag #f)
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (populate-level-count-list-hash! levels-htable max-l1-limit
					 max-levels max-zz)
  (begin
    (hash-clear! levels-htable)

    (do ((zz 1 (1+ zz)))
	((> zz max-zz))
      (begin
	(do ((yy zz (1+ yy)))
	    ((> yy max-zz))
	  (begin
	    (let ((xx-loop-flag #t))
	      (begin
		(do ((xx yy (1+ xx)))
		    ((or (> xx max-zz)
			 (equal? xx-loop-flag #f)))
		  (begin
		    (let ((ll-1 (+ (* xx (+ yy zz)) (* yy zz))))
		      (begin
			(if (> ll-1 max-l1-limit)
			    (begin
			      (set! xx-loop-flag #f))
			    (begin
			      (let ((loop-flag #t))
				(begin
				  (do ((ll 1 (1+ ll)))
				      ((or (> ll max-levels)
					   (equal? loop-flag #f)))
				    (begin
				      (update-hash-lists levels-htable loop-flag
							 ll xx yy zz max-l1-limit)
				      ))
				  ))
			      ))
			))
		    ))
		))
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax update-hash-counts
  (syntax-rules ()
    ((update-hash-counts
      counts-htable loop-flag
      this-layer ll-count-1 delta max-l1-limit)
     (begin
       (let ((ntmp (1- this-layer)))
	 (let ((ntmp-2 (* 4 (1- ntmp) ntmp)))
	   (let ((layer-count (+ ll-count-1 (* ntmp delta) ntmp-2)))
	     (begin
	       (if (<= layer-count max-l1-limit)
		   (begin
		     (let ((prev-count (hash-ref counts-htable layer-count 0)))
		       (begin
			 (hash-set! counts-htable layer-count (1+ prev-count))
			 )))
		   (begin
		     (set! loop-flag #f)
		     ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (populate-level-count-hash! counts-htable max-l1-limit
				    max-levels max-zz status-num)
  (let ((start-jday (srfi-19:current-julian-day)))
    (begin
      (hash-clear! counts-htable)

      (do ((zz 1 (1+ zz)))
	  ((> zz max-zz))
	(begin
	  (if (zero? (modulo zz status-num))
	      (begin
		(let ((end-jday (srfi-19:current-julian-day)))
		  (begin
		    (display (ice9-format:format #f "~:d / ~:d : " zz max-zz))
		    (display (format #f "elapsed time = ~a : ~a~%"
				     (julian-day-difference-to-string end-jday start-jday)
				     (current-date-time-string)))
		    (force-output)
		    (set! start-jday end-jday)
		    ))
		))

	  (do ((yy zz (1+ yy)))
	      ((> yy max-zz))
	    (begin
	      (let ((xx-loop-flag #t)
                    (ytmp (+ yy zz))
                    (yz-tmp (* yy zz)))
		(begin
		  (do ((xx yy (1+ xx)))
		      ((or (> xx max-zz)
			   (equal? xx-loop-flag #f)))
		    (begin
		      (let ((ll-1 (* 2 (+ (* xx ytmp) yz-tmp)))
			    (delta (* 4 (+ xx ytmp))))
			(begin
			  (if (> ll-1 max-l1-limit)
			      (begin
				(set! xx-loop-flag #f))
			      (begin
                                ;;; update layer-1 count
				(let ((prev-count (hash-ref counts-htable ll-1 0)))
				  (begin
				    (hash-set! counts-htable ll-1 (1+ prev-count))
				    ))
                                ;;; update layer-2 count
				(let ((level-2 (+ ll-1 delta)))
				  (let ((prev-count (hash-ref counts-htable level-2 0)))
				    (begin
				      (hash-set! counts-htable level-2 (1+ prev-count))
				      )))

				(let ((nn-loop-flag #t))
				  (begin
				    ;;; start from layer-3
				    (do ((ll 3 (1+ ll)))
					((or (> ll max-levels)
					     (equal? nn-loop-flag #f)))
				      (begin
					(update-hash-counts
                                         counts-htable nn-loop-flag
                                         ll ll-1 delta max-l1-limit)
					))
				    ))
				))
			  ))
		      ))
		  ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define-syntax process-lists
  (syntax-rules ()
    ((process-lists level-htable target-level-count
		    max-levels max-zz debug-flag)
     (begin
       (let ((result-list-list #f)
	     (min-nn -1))
	 (begin
	   (hash-for-each
	    (lambda (key value)
	      (begin
		(if (= (length value) target-level-count)
		    (begin
		      (if (or (< min-nn 0)
			      (< key min-nn))
			  (begin
			    (set! min-nn key)
			    (set! result-list-list (list key value))
			    ))
		      ))
		)) level-htable)

	   (if (< min-nn 0)
	       (begin
		 (display
                  (ice9-format:format
                   #f "no results found for number of cubes=~:d, with max-level=~:d, max-zz=~:d~%"
                   target-level-count max-levels max-zz))
		 (force-output))
	       (begin
		 (if (equal? debug-flag #t)
		     (begin
		       (let ((ii-count 0)
			     (lcount (car result-list-list))
			     (tail-list (cadr result-list-list)))
			 (begin
			   (for-each
			    (lambda (r-list)
			      (begin
				(let ((r-level (list-ref r-list 0))
				      (r-xx (list-ref r-list 1))
				      (r-yy (list-ref r-list 2))
				      (r-zz (list-ref r-list 3)))
				  (begin
				    (set! ii-count (1+ ii-count))
				    (display
                                     (ice9-format:format
                                      #f "  (~:d) layer=~:d : cuboid=~:dx~:dx~:d : (number of cubes=~:d)~%"
                                      ii-count r-level
                                      r-xx r-yy r-zz
                                      lcount))
				    ))
				)) tail-list)
			   (newline)

			   (display (ice9-format:format #f "~:d is the least value of n such that C(n) = ~:d.~%"
							min-nn target-level-count))
			   (force-output)
			   ))
		       ))
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax process-counts
  (syntax-rules ()
    ((process-counts level-htable target-level-count
		     max-levels max-zz debug-flag)
     (begin
       (let ((result-count 0)
	     (min-nn -1))
	 (begin
	   (hash-for-each
	    (lambda (key value)
	      (begin
		(if (= value target-level-count)
		    (begin
		      (if (or (< min-nn 0)
			      (< key min-nn))
			  (begin
			    (set! min-nn key)
			    (set! result-count value)
			    ))
		      (if (equal? debug-flag #t)
			  (begin
			    (display (format #f "  C(~:d) = ~:d~%" key value))
			    ))
		      ))
		)) level-htable)

	   (if (< min-nn 0)
	       (begin
		 (display (ice9-format:format #f "no results found for number of cubes=~:d, with max-level=~:d, max-zz=~:d~%"
					      target-level-count max-levels max-zz))
		 (force-output))
	       (begin
		 (display (ice9-format:format #f "~:d is the least value of n such that C(n) = ~:d.~%"
					      min-nn target-level-count))
		 (force-output)
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (reproduce-problem-statement)
  (let ((target-level-count 10)
	(max-l1-limit 1000000)
	(max-levels 5)
	(max-zz 50))
    (let ((level-htable (make-hash-table)))
      (begin
	(populate-level-count-list-hash! level-htable max-l1-limit
					 max-levels max-zz)

	(let ((nn-list (list 22 46 78 118)))
	  (begin
	    (for-each
	     (lambda (nn)
	       (begin
		 (let ((result-list (hash-ref level-htable nn #f)))
		   (let ((rlen (length result-list)))
		     (begin
		       (display (ice9-format:format #f "C(~:d) = ~:d~%"
						    nn rlen))
		       )))
		 )) nn-list)
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop target-level-count max-l1-limit
		   max-levels max-zz status-num debug-flag)
  (let ((level-htable (make-hash-table)))
    (begin
      (if (equal? debug-flag #t)
	  (begin
	    (populate-level-count-list-hash! level-htable max-l1-limit
					     max-levels max-zz)

	    (process-lists level-htable target-level-count
			   max-levels max-zz debug-flag))
	  (begin
	    (populate-level-count-hash! level-htable max-l1-limit
					max-levels max-zz status-num)

	    (process-counts level-htable target-level-count
			    max-levels max-zz #t)
	    ))
      )))

;;;#############################################################
;;;#############################################################
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
				(current-date-time-string)))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Project Euler 126 - The minimum number of cubes to cover every visible face on a cuboid measuring 3 x 2 x 1 is twenty-two.~%"))
    (newline)
    (display (format #f "If we then add a second layer to this solid it would require forty-six cubes to cover every visible face, the third layer would require seventy-eight cubes, and the fourth layer would require one-hundred and eighteen cubes to cover every visible face.~%"))
    (newline)
    (display (format #f "However, the first layer on a cuboid measuring 5 x 1 x 1 also requires twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1, 7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.~%"))
    (newline)
    (display (format #f "We shall define C(n) to represent the number of cuboids that contain n cubes in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.~%"))
    (newline)
    (display (format #f "It turns out that 154 is the least value of n for which C(n) = 10.~%"))
    (newline)
    (display (format #f "Find the least value of n for which C(n) = 1000.~%"))
    (newline)
    (display (format #f "The solution was found at https://bitbucket.org/shlomif/project-euler/src/99dcadd842e5/project-euler/126/126-planning.txt, see also http://yambi.jp/wiki/index.php?Project%20Euler%20Problem126~%"))
    (newline)
    (display (format #f "The for a 3x2x1 initial cuboid, the number of cubes needed for the first layer is L1=2*(wx*wy+wx*wz+wy*wz)=2*(3*2+3*1+2*1)=22. The number of cubes needed for the second layer is L2=2*(wx*wy+wx*wz+wy*wz)+4*(wx+wy+wz)=L1+4*(3+2+1)=22+24=46. The number of cubes needed for the third layer is L3=2*(wx*wy+wx*wz+wy*wz)+4*(wx+wy+wz)+4*(wx+wy+wz)+8 = L1+2*delta+8=22+48+8=78, (where delta=4*(wx+wy+wz)). The number of cubes needed for the fourth layer is L4=L1+3*delta+1*8+2*8=22+72+24=118.~%"))
    (newline)
    (display (format #f "So the equation for calculating the number of cubes for the n-th level (n>2), is Ln=L1+(n-1)*delta+((n-2)*(n-1)/2)*8~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-count-cubes-in-layer-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (newline)
    (force-output)

    (time-code
     (begin
       (reproduce-problem-statement)
       ))

    (newline)
    (force-output)

    (let ((target-level-count 10)
	  (max-l1-limit 100000)
	  (max-levels 10)
	  (max-zz 50)
	  (status-num 1000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop target-level-count max-l1-limit
		      max-levels max-zz status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-level-count 1000)
	  (max-l1-limit 30000)
	  (max-levels 1000)
	  (max-zz 5000)
	  (status-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop target-level-count max-l1-limit
		      max-levels max-zz status-num debug-flag)
	   ))
	))

    (newline)
    ))
