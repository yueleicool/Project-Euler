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
;;;###  project euler 141                             ###
;;;###                                                ###
;;;###  last updated December 19, 2012                ###
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
	  (s2 (string-downcase
               (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (progressive-number-list nn nn-root)
  (let ((result-list (list))
	(continue-loop-flag #t))
    (begin
      (do ((dd 2 (1+ dd)))
	  ((or (>= dd nn-root)
	       (equal? continue-loop-flag #f)))
	(begin
	  (let ((dd-2 (* dd dd)))
	    (begin
	      (srfi-11:let-values
	       (((qq rr) (euclidean/ nn dd)))
	       (begin
		 (if (and (>= qq dd) (> rr 0))
		     (begin
		       (if (= (* qq rr) dd-2)
			   (begin
			     (set! result-list (list dd qq rr))
			     (set! continue-loop-flag #f)
			     ))
		       ))
		 ))
	      ))
	  ))

      (if (equal? continue-loop-flag #t)
	  (begin
	    #f)
	  (begin
	    result-list
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-progressive-number-list-1)
  (let ((sub-name "test-progressive-number-list-1")
	(test-list
	 (list
	  (list 2 #f) (list 3 #f)
	  (list 9 (list 2 4 1))
	  (list 58 (list 6 9 4))
	  (list 10404 (list 72 144 36))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list
                    (progressive-number-list
                     nn (exact-integer-sqrt nn))))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format #f "~a : (~a) : error : nn=~a, "
                                sub-name test-label-index nn))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
                                shouldbe-list result-list))
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
(define (is-progressive-number? nn nn-root)
  (let ((result-list (progressive-number-list nn nn-root)))
    (begin
      (if (and (list? result-list)
	       (> (length result-list) 1))
	  (begin
	    #t)
	  (begin
	    #f
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-progressive-number-1)
  (let ((sub-name "test-is-progressive-number-1")
	(test-list
	 (list
	  (list 2 #f) (list 3 #f)
	  (list 58 #t)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (is-progressive-number?
                     nn (exact-integer-sqrt nn))))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format #f "~a : (~a) : error : nn=~a, "
                                sub-name test-label-index nn))
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
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info nn dd qq rr)
     (begin
       (let ((r1 (/ qq dd)))
	 (begin
	   (display
            (ice-9-format:format
             #f "  nn=~:d, quotient=~:d, divisor=~:d, "
             nn qq dd))
	   (display
            (ice-9-format:format
             #f "remainder=~a, ratio=~a~%" rr r1))
	   (force-output)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax display-status-info
  (syntax-rules ()
    ((display-status-info sq-counter max-ii sum count start-jday)
     (begin
       (let ((end-jday (srfi-19:current-julian-day)))
	 (begin
	   (display
            (ice-9-format:format #f "~:d / ~:d : sum = ~:d, count = ~:d : "
                                 sq-counter max-ii sum count))
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
(define-syntax run-inner-vloop
  (syntax-rules ()
    ((run-inner-vloop tt ss vv ss-3 vv-num max-num
                      count sum debug-flag)
     (begin
       (do ((vv 1 (1+ vv)))
           ((> vv-num max-num))
         (begin
           (set! vv-num (* vv tt (+ (* ss-3 vv) tt)))

           (let ((vv-la (logand vv-num 15)))
             (begin
               (if (or (= vv-la 0) (= vv-la 1)
                       (= vv-la 4) (= vv-la 9))
                   (begin
                     (let ((mm-2 vv-num)
                           (mm (exact-integer-sqrt vv-num)))
                       (begin
                         (if (and (= mm-2 (* mm mm))
                                  (< mm-2 max-num))
                             (begin
                               (set! count (1+ count))
                               (set! sum (+ sum mm-2))

                               (if (equal? debug-flag #t)
                                   (begin
                                     (let ((rr (* vv tt tt))
                                           (qq (* ss vv tt))
                                           (dd (* ss ss vv)))
                                       (begin
                                         (display-debug-info mm-2 qq dd rr)
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
(define (main-loop max-num status-num debug-flag)
  (let ((count 0)
	(sum 0)
	(tt-num -1)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (do ((tt 1 (1+ tt)))
	  ((> tt-num max-num))
	(begin
	  (let ((tt-p1 (1+ tt))
		(ss-num -1))
	    (begin
	      (set! tt-num (+ (* tt tt-p1 tt-p1 tt-p1) (* tt tt)))

	      (do ((ss (1+ tt) (1+ ss)))
		  ((> ss-num max-num))
		(begin
		  (let ((ss-3 (* ss ss ss))
			(vv-num -1))
		    (begin
		      (set! ss-num (* tt (+ ss-3 tt)))
		      (if (= (gcd tt ss) 1)
			  (begin
                            (run-inner-vloop
                             tt ss vv ss-3 vv-num max-num
                             count sum debug-flag)
			    ))
		      ))
		  ))
	      ))

	  (if (zero? (modulo tt status-num))
	      (begin
		(display-status-info tt-num max-num sum count start-jday)
		))
	  ))

      (display
       (ice-9-format:format
        #f "The sum of progressive perfect squares = ~:d. "
        sum))
      (display
       (ice-9-format:format
        #f "Found ~:d progressive perfect squares, " count))
      (display
       (ice-9-format:format #f "(for numbers less than ~:d).~%"
                            max-num))
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
    (display (format #f "Project Euler 141 - A positive integer, n, is divided by d and the quotient and remainder are q and r respectively. In addition d, q, and r are consecutive positive integer terms in a geometric sequence, but not necessarily in that order.~%"))
    (newline)
    (display (format #f "For example, 58 divided by 6 has quotient 9 and remainder 4. It can also be seen that 4, 6, 9 are consecutive terms in a geometric sequence (common ratio 3/2).  We will call such numbers, n, progressive.~%"))
    (newline)
    (display (format #f "Some progressive numbers, such as 9 and 10404 = 102^2, happen to also be perfect squares.  The sum of all progressive perfect squares below one hundred thousand is 124657.~%"))
    (newline)
    (display (format #f "Find the sum of all progressive perfect squares below one trillion (10^12).~%"))
    (newline)
    (display (format #f "The solution was found at http://yambi.jp/wiki/index.php?Project%20Euler%20Problem141~%"))
    (newline)
    (display (format #f "Assume that n=q*d+r, and that r<q<d, where d a divisor less than sqrt(n), q is the quotient, and r is the remainder.  The requirement that n is progressive means that d/q = q/r = s/t >= 1.  Then d=qs/t, and q=rs/t, so d=rs^2/t^2, and n=q*d+r=s^3*r^2/t^3+r.  The requirement that n is a square, means that n = m^2 = s^3*r^2/t^3+r.  Let r=v*t^2, then m^2 = (s^3*v^2*t+v*t^2) or m^2 = v*t*(s^3*v+t).~%"))
    (newline)
    (display (format #f "Reversing the variables, gives r=v*t^2, q=s*v*t, d=s^2*v~%"))
    (display (format #f "n = m^2 = v*t*(s^3*v+t)~%"))
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

    (let ((max-num 100000)
	  (status-num 1000000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num (inexact->exact 1e12))
	  (status-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num status-num debug-flag)
	   ))
	))

    (newline)
    ))
