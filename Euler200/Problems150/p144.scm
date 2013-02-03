#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### This is free and unencumbered software released into the public domain.
;;;###
;;;### Anyone is free to copy, modify, publish, use, compile, sell, or
;;;### distribute this software, either in source code form or as a compiled
;;;### binary, for any purpose, commercial or non-commercial, and by any
;;;### means.
;;;###
;;;### In jurisdictions that recognize copyright laws, the author or authors
;;;### of this software dedicate any and all copyright interest in the
;;;### software to the public domain. We make this dedication for the benefit
;;;### of the public at large and to the detriment of our heirs and
;;;### successors. We intend this dedication to be an overt act of
;;;### relinquishment in perpetuity of all present and future rights to this
;;;### software under copyright law.
;;;###
;;;### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;### IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;### OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;### ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;### OTHER DEALINGS IN THE SOFTWARE.
;;;###
;;;### For more information, please refer to <http://unlicense.org/>
;;;###

;;;######################################################
;;;######################################################
;;;###                                                ###
;;;###  project euler 144                             ###
;;;###                                                ###
;;;###  last updated December 24, 2012                ###
;;;###                                                ###
;;;###  written by Robert Haramoto                    ###
;;;###                                                ###
;;;######################################################
;;;######################################################

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;### ls - find all test subroutines to run
(use-modules ((ice-9 ls)
	      :renamer (symbol-prefix-proc 'ice-9-ls:)))

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
	     (* 0.0010
                (truncate (* 1000.0 (- nmins (* nhours 60.0)))))))
	(let ((nseconds
	       (* 0.0010
		  (truncate
		   (* 1000.0
                      (- nsecs
                         (+ (* nhours 60.0 60.0)
                            (* nminutes 60.0))))))))
	  (begin
	    (if (<= nhours 0.0)
                (begin
                  (if (<= nminutes 0.0)
                      (begin
                        (format #f "~a seconds" nsecs))
                      (begin
                        (format #f "~a minutes, ~a seconds" nminutes nseconds)
                        )))
                (begin
                  (if (<= nminutes 0.0)
                      (begin
                        (format #f "~a hours, ~a seconds" nhours nseconds))
                      (begin
                        (format #f "~a hours, ~a minutes, ~a seconds"
                                nhours nminutes nseconds)
                        ))
                  ))
            ))
        )))
  (if (and (number? dend) (number? dstart))
      (begin
	(let ((jd-diff (exact->inexact (- dend dstart))))
          (begin
            (if (< jd-diff 1.0)
                (begin
                  (let ((tstring (local-process-sub-day jd-diff)))
                    (begin
                      tstring
                      )))
                (begin
                  (let ((ndays (truncate jd-diff)))
                    (let ((dfract-diff (- jd-diff ndays)))
                      (let ((tstring (local-process-sub-day dfract-diff)))
                        (let ((ttstring (format #f "~a days, ~a" ndays tstring)))
                          (begin
                            ttstring
                            )))
                      ))
                  ))
            )))
      (begin
        #f
        )))

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
;;; ellipse hardwired as 4x^2+y^2=100
(define (is-point-on-ellipse? xx-0 yy-0 tolerance)
  (let ((sum (+ (* 4.0 xx-0 xx-0) (* yy-0 yy-0))))
    (begin
      (if (< (abs (- 100.0 sum)) tolerance)
	  (begin
	    #t)
	  (begin
	    #f
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-point-on-ellipse-1)
  (let ((sub-name "test-is-point-on-ellipse-1")
	(test-list
	 (list
	  (list 0.0 10.0 #t)
	  (list 5.0 0.0 #t)
	  (list 0.0 -10.0 #t)
	  (list -5.0 0.0 #t)
	  (list 0.0 0.0 #f)
	  (list 1.0 2.0 #f)
	  ))
	(tolerance 1e-12)
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((xx-0 (list-ref this-list 0))
		 (yy-0 (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (is-point-on-ellipse? xx-0 yy-0 tolerance)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : xx-0=~a, yy-0=~a, "
                         sub-name test-label-index xx-0 yy-0))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
                                shouldbe result))
		       (set! ok-flag #f)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define-syntax check-point
  (syntax-rules ()
    ((check-point xx-2 yy-2 xx-1 yy-1 m-out tolerance)
     (begin
       (if (not (is-point-on-ellipse? xx-2 yy-2 tolerance))
	   (begin
	     (display
              (format #f "check-point error : (xx-2=~a, yy-2=~a) "
                      xx-2 yy-2))
	     (display
              (format #f "point not on ellipse 4x^2+y^2=100.  "))
	     (display
              (format #f "Start from (xx-1=~a, yy-1=~a), slope_out=~a~%"
                      xx-1 yy-1 m-out))
	     (force-output)
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax calc-point
  (syntax-rules ()
    ((calc-point xx-1 yy-1 m-out tolerance output-list)
     (begin
       (let ((m-out-2 (* m-out m-out)))
	 (let ((aa (+ 4.0 m-out-2))
	       (bb (* 2.0 m-out (- yy-1 (* m-out xx-1))))
	       (cc (- (+ (* m-out-2 xx-1 xx-1) (* yy-1 yy-1))
		      (+ (* 2.0 m-out xx-1 yy-1) 100.0))))
	   (let ((denom (/ 1.0 (* 2.0 aa)))
		 (bb-m-4ac (- (* bb bb) (* 4.0 aa cc))))
	     (let ((term1 (* -1.0 bb))
		   (term2 (sqrt bb-m-4ac)))
	       (begin
		 (if (>= bb-m-4ac 0.0)
		     (begin
		       (let ((xx-soln-1 (* denom (+ term1 term2)))
			     (xx-soln-2 (* denom (- term1 term2))))
			 (begin
			   (if (< (abs (- xx-soln-1 xx-1)) tolerance)
			       (begin
				 (let ((xx-2 xx-soln-2)
				       (yy-2 (+ (* m-out (- xx-soln-2 xx-1))
						yy-1)))
				   (begin
				     (set! output-list (list xx-2 yy-2))
				     (check-point xx-2 yy-2 xx-1 yy-1 m-out tolerance)
				     )))
			       (begin
				 (let ((xx-2 xx-soln-1)
				       (yy-2 (+ (* m-out (- xx-soln-1 xx-1))
						yy-1)))
				   (begin
				     (set! output-list (list xx-2 yy-2))
				     (check-point xx-2 yy-2 xx-1 yy-1 m-out tolerance)
				     ))
				 ))
			   )))
		     (begin
		       (display
                        (format #f "error condition: no real solution for "))
		       (display
                        (format #f "xx-1=~a, yy-1=~a, m-out=~a, b^2-4ac=~a~%"
                                xx-1 yy-1 m-out bb-m-4ac))
		       (force-output)
		       (quit)
		       ))
		   ))
	       ))
	   ))
       )))

;;;#############################################################
;;;#############################################################
;;; (x0, y0) is the previous point on the ellipse
;;; (x1, y1) is the current point on the ellipse (where ray struck last)
;;; (x2, y2) is the next point on the ellipse
(define (calculate-next-point xx-0 yy-0 xx-1 yy-1)
  (let ((result-list (list))
	(tolerance 1e-10))
    (begin
      (cond
       ((< (abs xx-1) tolerance)
	(begin
	  ;;; normal slope is vertical (infinite)
	  (if (< (abs xx-0) tolerance)
	      (begin
		;;; incoming ray is straight down
		;;; outgoing ray is straight back up
		(list xx-0 yy-0))
	      (begin
		;;; incoming ray is non-vertical
		(let ((m-out (* -1.0
				(/ (- yy-1 yy-0) (- xx-1 xx-0)))))
		  (let ((output-list (list)))
		    (begin
		      (calc-point xx-1 yy-1 m-out tolerance output-list)
		      output-list
		      )))
		))
	  ))
       (else
	(begin
	  (if (< (abs (- xx-1 xx-0)) tolerance)
	      (begin
                ;;; input line is a vertical
		(let ((m-normal (/ yy-1 (* 4.0 xx-1))))
		  (let ((m-normal-2 (* m-normal m-normal)))
		    (let ((m-numer (1- m-normal-2))
			  (m-denom (* 2.0 m-normal)))
		      (begin
			(if (> (abs m-denom) tolerance)
			    (begin
			      (let ((m-out (/ m-numer m-denom))
				    (output-list (list)))
				(begin
				  (calc-point xx-1 yy-1 m-out tolerance output-list)
				  output-list
				  )))
			    (begin
			      ;;; output line is a vertical
			      (check-point xx-1 yy-0 xx-1 yy-1 999999.99 tolerance)
			      (list xx-1 yy-0)
			      ))
			))
		    )))
	      (begin
		(let ((m-in (/ (- yy-1 yy-0) (- xx-1 xx-0)))
		      (m-normal (/ yy-1 (* 4.0 xx-1))))
		  (let ((m-normal-2 (* m-normal m-normal)))
		    (let ((m-numer (+ (* m-in (1- m-normal-2))
				      (* 2.0 m-normal)))
			  (m-denom (- (1+ (* 2.0 m-in m-normal))
				      m-normal-2)))
		      (begin
			(if (> (abs m-denom) tolerance)
			    (begin
			      (let ((m-out (/ m-numer m-denom))
				    (output-list (list)))
				(begin
				  (calc-point xx-1 yy-1 m-out tolerance output-list)
				  output-list
				  )))
			    (begin
			      ;;; output line is a vertical
			      (check-point xx-1 yy-1 xx-1 yy-1 999999.99 tolerance)
			      (list xx-1 yy-1)
			      ))
			))
		    ))
		))
	  )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-next-point-1)
  (let ((sub-name "test-calculate-next-point-1")
	(test-list
	 (list
	  (list 0.0 10.0 0.0 -10.0 1e-12 (list 0.0 10.0))
	  (list 0.0 10.0 5.0 0.0 1e-12 (list 0.0 -10.0))
	  (list 5.0 0.0 0.0 -10.0 1e-12 (list -5.0 0.0))
	  (list 0.0 -10.0 -5.0 0.0 1e-12 (list 0.0 10.0))
	  (list 0.0 10.0 -5.0 0.0 1e-12 (list 0.0 -10.0))
	  (list -5.0 0.0 0.0 -10.0 1e-12 (list 5.0 0.0))
	  (list 0.0 -10.0 5.0 0.0 1e-12 (list 0.0 10.0))
	  (list 0.0 10.1 1.4 -9.6 1e-2 (list -3.99 -6.02))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((xx-0 (list-ref this-list 0))
		 (yy-0 (list-ref this-list 1))
		 (xx-1 (list-ref this-list 2))
		 (yy-1 (list-ref this-list 3))
		 (tolerance (list-ref this-list 4))
		 (shouldbe-list (list-ref this-list 5)))
	     (let ((result-list (calculate-next-point xx-0 yy-0 xx-1 yy-1)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
	       (begin
		 (if (not (equal? slen rlen))
		     (begin
		       (display
                        (format #f "~a : (~a) : error : "
                                sub-name test-label-index))
		       (display
                        (format #f "xx-0=~a, yy-0=~a, xx-1=~a, yy-1=~a, "
                                xx-0 yy-0 xx-1 yy-1))
		       (display
                        (format #f "shouldbe=~a, result=~a, "
                                shouldbe-list result-list))
		       (display
                        (format #f "length discrepancy, shouldbe=~a, result=~a~%"
                                slen rlen))
		       (set! ok-flag #f)
		       ))
		 (do ((ii 0 (1+ ii)))
		     ((>= ii slen))
		   (begin
		     (let ((s-elem (list-ref shouldbe-list ii))
			   (r-elem (list-ref result-list ii)))
		       (begin
			 (if (> (abs (- s-elem r-elem)) tolerance)
			     (begin
                               (display
                                (format #f "~a : (~a) : error : "
                                        sub-name test-label-index))
                               (display
                                (format #f "xx-0=~a, yy-0=~a, xx-1=~a, yy-1=~a, "
                                        xx-0 yy-0 xx-1 yy-1))
                               (display
                                (format #f "shouldbe=~a, result=~a, "
                                        shouldbe-list result-list))
                               (display
                                (format
                                 #f "discrepancy, at element shouldbe=~a, result=~a~%"
                                 s-elem r-elem))
			       (set! ok-flag #f)
			       ))
			 ))
		     ))
		 ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define-syntax display-status-info
  (syntax-rules ()
    ((display-status-info counter xx yy start-jday)
     (begin
       (let ((end-jday (srfi-19:current-julian-day)))
	 (begin
	   (display
            (ice-9-format:format
             #f "count = ~:d : last point (~2,3f, ~2,3f) : "
             counter xx yy))
	   (display
            (format #f "elapsed time = ~a : ~a~%"
                    (julian-day-difference-to-string end-jday start-jday)
                    (current-date-time-string)))
	   (force-output)
	   (set! start-jday end-jday)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop xx-0 yy-0 xx-1 yy-1 min-xx max-xx
		   status-num debug-flag)
  (let ((count 1)
	(ll-xx-0 xx-0)
	(ll-yy-0 yy-0)
	(ll-xx-1 xx-1)
	(ll-yy-1 yy-1)
	(min-yy (sqrt (- 100.0 (* 4.0 min-xx min-xx))))
	(max-yy (sqrt (- 100.0 (* 4.0 max-xx max-xx))))
	(tolerance 1e-10)
	(continue-loop-flag #t)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (while
       (equal? continue-loop-flag #t)
       (begin
	 (if (and (>= ll-xx-1 min-xx)
		  (<= ll-xx-1 max-xx)
		  (>= ll-yy-1 min-yy))
	     (begin
	       ;;; ray has escaped the white cell
	       ;;; adjust count, which counted the escape point
	       (set! count (1- count))

	       (set! continue-loop-flag #f))
	     (begin
	       ;;; ray still within white cell
	       (let ((next-list
		      (calculate-next-point
		       ll-xx-0 ll-yy-0 ll-xx-1 ll-yy-1)))
		 (let ((next-xx (list-ref next-list 0))
		       (next-yy (list-ref next-list 1)))
		   (begin
		     (if (equal? debug-flag #t)
			 (begin
			   (check-point next-xx next-yy
                                        ll-xx-1 ll-yy-1 88888888.88 tolerance)
			   (display
                            (ice-9-format:format
                             #f "(~:d) last point = (~2,3f, ~2,3f), next point = (~2,3f, ~2,3f)~%"
                             count ll-xx-1 ll-yy-1 next-xx next-yy))
			   (force-output)
			   ))

		     (set! count (1+ count))

		     (set! ll-xx-0 ll-xx-1)
		     (set! ll-yy-0 ll-yy-1)

		     (set! ll-xx-1 next-xx)
		     (set! ll-yy-1 next-yy)
		     )))
	       ))

	 (if (zero? (modulo count status-num))
	     (begin
	       (display-status-info count ll-xx-1 ll-yy-1 start-jday)
	       ))
	 ))

      (display (ice-9-format:format
		#f "The number of times the beam hits the cell wall is = ~:d~%"
		count))
      (force-output)
      )))

;;;###################################################
;;;###################################################
;;;###
;;;###  find-all-tests - returns a list of "test-*"
;;;###  methods
;;;###
(define-public (find-all-tests)
  (define (local-make-test-list input-list result-list)
    (if (or
	 (null? input-list)
	 (not (list? input-list))
	 (< (length input-list) 1))
	(begin
	  result-list)
	(begin
	  (let ((this-func (car input-list))
		(tail-list (cdr input-list)))
            (begin
              (if (symbol? this-func)
                  (begin
                    (let ((tstring (symbol->string this-func)))
                      (if (string-prefix-ci? "test-" tstring)
                          (begin
                            (local-make-test-list
                             tail-list (append (list this-func) result-list)))
                          (begin
                            (local-make-test-list tail-list result-list)
                            ))
                      ))
		(begin
		  (local-make-test-list tail-list result-list)
		  ))
              ))
          )))
  (let ((lfunc-list (ice-9-ls:lls)))
    (begin
      (let ((test-func-list (local-make-test-list lfunc-list (list))))
	test-func-list
	))
    ))

;;;#############################################################
;;;#############################################################
(define (display-all-test-names st-list test-htable)
  (begin
    (let ((count (length st-list)))
      (begin
        (for-each
         (lambda (atest)
           (begin
             (let ((sname (symbol->string atest)))
               (let ((pr-name (hash-ref test-htable sname sname)))
                 (begin
                   (display (format #f "  ~a~%" pr-name))
                   )))
             )) st-list)

        (display (format #f "total number of tests = ~a~%" count))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (run-all-tests debug-flag)
  (begin
    (let ((nsuccess 0)
          (nfailures 0)
          (ntotals 0)
          (test-htable (make-hash-table 50))
          (t-list (find-all-tests)))
      (let ((st-list
             (sort
              t-list
              (lambda (a b)
                (begin
                  (string-ci<? (symbol->string a)
                               (symbol->string b))
                  ))))
            (count (length t-list)))
        (begin
          (hash-clear! test-htable)

          (for-each
           (lambda (atest)
             (begin
               (let ((a-func (primitive-eval atest)))
                 (let ((bresult (a-func))
                       (s-name (symbol->string atest)))
                   (begin
                     (if (equal? bresult #t)
                         (begin
                           (set! nsuccess (1+ nsuccess))
                           (hash-set! test-htable s-name
                                      (format #f "ok! ~a" s-name)))
                         (begin
                           (set! nfailures (1+ nfailures))
                           (hash-set! test-htable
                                      s-name
                                      (format #f "error! *** ~a ***" s-name))
                           ))
                     (set! ntotals (1+ ntotals))
                     )))
               )) st-list)

          (if (equal? debug-flag #t)
              (begin
                (display-all-test-names st-list test-htable)
                ))

          (display (format #f "=====================================================~%"))
          (display (format #f "Test Summary~%"))
          (display (ice-9-format:format
                    #f "successful tests = ~:d (~,1f%)~%"
                    nsuccess (* 100.0 (/ nsuccess ntotals))))
          (display (ice-9-format:format
                    #f "failed tests = ~:d (~,1f%)~%"
                    nfailures (* 100.0 (/ nfailures ntotals))))
          (display (ice-9-format:format
                    #f "total tests = ~:d (~,1f%)~%"
                    (+ nsuccess nfailures)
                    (* 100.0 (/ (+ nsuccess nfailures) ntotals))))
          (display (format #f "=====================================================~%"))
          ))
      )))

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
    (display (format #f "Project Euler 144 - In laser physics, a 'white cell' is a mirror system that acts as a delay line for the laser beam. The beam enters the cell, bounces around on the mirrors, and eventually works its way back out.~%"))
    (newline)
    (display (format #f "The specific white cell we will be considering is an ellipse with the equation 4x^2 + y^2 = 100~%"))
    (newline)
    (display (format #f "The section corresponding to 0.01 <= x <= +0.01 at the top is missing, allowing the light to enter and exit through the hole.~%"))
    (newline)
    (display (format #f "The light beam in this problem starts at the point (0.0,10.1) just outside the white cell, and the beam first impacts the mirror at (1.4,-9.6).~%"))
    (newline)
    (display (format #f "Each time the laser beam hits the surface of the ellipse, it follows the usual law of reflection 'angle of incidence equals angle of reflection.' That is, both the incident and reflected beams make the same angle with the normal line at the point of incidence.~%"))
    (newline)
    (display (format #f "In the figure on the left, the red line shows the first two points of contact between the laser beam and the wall of the white cell; the blue line shows the line tangent to the ellipse at the point of incidence of the first bounce.~%"))
    (newline)
    (display (format #f "The slope m of the tangent line at any point (x,y) of the given ellipse is: m = -4x/y~%"))
    (newline)
    (display (format #f "The normal line is perpendicular to this tangent line at the point of incidence.~%"))
    (newline)
    (display (format #f "The animation on the right shows the first 10 reflections of the beam.~%"))
    (newline)
    (display (format #f "How many times does the beam hit the internal surface of the white cell before exiting?~%"))
    (newline)
    (display (format #f "The equations needed to describe the reflections within the ellipse can be found at http://www.codecogs.com/reference/maths/geometry/coordinate/ellipse.php, which gives the equations for the tangent and normal lines at each point on the ellipse.  We also need to find the angle that the normal and the ray make, which is given at http://www.ahristov.com/tutorial/geometry-games/angle-between-lines.html~%"))
    (newline)
    (display (format #f "When the ray hits point (x, y), the slope of the line can be found initially as m say, and the slope of the tangent line is -4x/y, so the slope of the normal line is m_n=+y/(4x), so we can find the angle between the two lines as phi = arctan((m-m_n)/(1+m*m_n)). Since the angle of incidence equals the angle of reflection, this says arctan((m-m_n)/(1+m*m_n)) = arctan((m_n-m_out)/(1+m_n*m_out)) or (m-m_n)/(1+m*m_n) = (m_n-m_out)/(1+m_n*m_out).  Re-arranging we have m_out=(m*(m_n^2-1)+2m_n)/(1+2m*m_n-m_n^2)~%"))
    (newline)
    (display (format #f "Since we know the point (x1, y1), and we have a new slope, we can find the equation for the next line (y-y1) = m_out*(x-x1).  Next we calculate the intersection of this new line with the ellipse.  Let (x2, y2) be the intersection between this reflected ray and the ellipse, then (y2-y1) = m_out*(x2-x1) and 4x2^2+y2^2=100.  4x2^2+(m_out*(x2-x1)+y1)^2=100, 4x2^2+m_out^2*(x2^2-2x2*x1+x1^2)+2m_out*(x2-x1)*y1+y1^2=100, (4+m_out^2)*x2^2+(-2m_out^2*x1+2m_out*y1)*x2+(m_out^2*x1^2-2m_out*x1*y1+y1^2-100)=0.  Let a=(4+m_out^2), b=2*m_out(-m_out*x1+y1), c=m_out^2*x1^2-2m_out*x1*y1+y1^2-100, the equation becomes ax2^2+bx2+c=0, and we can use the quadratic equation to find the two points on the ellipse where the line intersects, x=-b/2a +/- (sqrt(b^2-4ac))/2a.  One point should be the initial point x1, the other should be the next point where the beam hits the internal surface of the white cell.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((debug-flag #t))
      (begin
	(time-code
	 (begin
	   (run-all-tests debug-flag)
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((xx-0 0.0)
	  (yy-0 10.1)
	  (xx-1 1.4)
	  (yy-1 -9.6)
	  (min-xx -0.01)
	  (max-xx 0.01)
	  (status-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop xx-0 yy-0 xx-1 yy-1 min-xx max-xx
		      status-num debug-flag)
	   ))
	))

    (newline)
    ))
