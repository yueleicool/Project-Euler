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
;;;###  project euler 145                             ###
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
    (let ((s1
           (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2
           (string-downcase
            (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (even-digits-count nn)
  (begin
    (cond
     ((<= nn 0) #f)
     ((odd? nn) #f)
     (else
      (begin
	(let ((kk (euclidean/ nn 2)))
	  (let ((result 20))
	    (begin
	      (do ((ii (1- kk) (1- ii)))
		  ((<= ii 0))
		(begin
		  (set! result (* result 30))
		  ))

	      result
	      )))
	))
     )))

;;;#############################################################
;;;#############################################################
(define (test-even-digits-count-1)
  (let ((sub-name "test-even-digits-count-1")
	(test-list
	 (list
	  (list 0 #f) (list 2 20) (list 4 600)
	  (list 6 18000) (list 8 540000)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((nn (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (even-digits-count nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index nn
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
(define (odd-digits-count nn)
  (begin
    (cond
     ((<= nn 0) #f)
     ((= nn 1) 0)
     ((even? nn) #f)
     (else
      (let ((rr (modulo nn 4))
	    (kk (quotient nn 4)))
	(begin
	  (cond
	   ((= rr 3)
	    (begin
	      (let ((result 100))
		(begin
		  (do ((ii 0 (1+ ii)))
		      ((>= ii kk))
		    (begin
		      (let ((next-result (* result 500)))
			(begin
			  (set! result next-result)
			  ))
		      ))
		  result
		  ))
	      ))
	   (else
	    (begin
	      0
	      ))
	   ))
	))
     )))

;;;#############################################################
;;;#############################################################
(define (test-odd-digits-count-1)
  (let ((sub-name "test-odd-digits-count-1")
	(test-list
	 (list
	  (list 0 #f) (list 1 0) (list 3 100)
	  (list 5 0) (list 7 50000) (list 9 0)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((nn (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (odd-digits-count nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index nn
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
(define (main-loop max-digits debug-flag)
  (let ((count 0)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (do ((nn 2 (1+ nn)))
	  ((> nn max-digits))
	(begin
	  (if (even? nn)
	      (begin
		(let ((this-count (even-digits-count nn)))
		  (begin
		    (set! count (+ count this-count))
		    )))
	      (begin
		(let ((this-count (odd-digits-count nn)))
		  (begin
		    (set! count (+ count this-count))
		    ))
		))
	  ))

      (display (ice-9-format:format
		#f "The number of reversible numbers = ~:d (below 10^~:d)~%"
		count max-digits))
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

          (if (> ntotals 0)
              (begin
                (display
                 (format #f "=====================================================~%"))
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
                (display
                 (format #f "=====================================================~%"))
                ))
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
    (display (format #f "Project Euler 145 - Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).~%"))
    (newline)
    (display (format #f "There are 120 reversible numbers below one-thousand.~%"))
    (newline)
    (display (format #f "How many reversible numbers are there below one-billion (10^9)?~%"))
    (newline)
    (display (format #f "The solution was found at http://www.javaist.com/blog/2009/08/29/project-euler-problem-145-in-python-analytic/~%"))
    (newline)
    (display (format #f "A number can be represented as n = a0 + a1*10 + a2*10^2 + a3*10^3 +...~%  And when adding digits, it's helpful to break it up the sum of digits into 2 cases, if the terms are all greater than or less than 10."))
    (newline)
    (display (format #f "Two digit reversible numbers: we must have n+reverse(n) = a0+a1*10^1+a1+a0*10^1 = (a0+a1)+(a0+a1)*10^1, where (a0+a1)<10 and odd.  (a0+a1) cannot be greater than 10, since there will be a carry from the first digit into the ten's digit, and if (a0+a1) is odd, then the 10's factor (a0+a1)+1 will be even.  Possible values for (a0, a1) are (1, 2), (1, 4), (1, 6), (1, 8), (2, 3), (2, 5), (2, 7), (3, 4), (3, 6), (4, 5) plus the reverse numbers, for a total of 20.~%"))
    (newline)
    (display (format #f "Three digit reversible numbers: we must have n+reverse(n) = (a0+a2)+(a1+a1)*10^1+(a0+a2)*10^2.  Note that since (a1+a1) is always even, the only way for there to be three digit reversible numbers is if (a1+a1)<10, and if a0+a2>10 and odd, (for example 409 or 904).  Possible values for (a0, a2) are (2, 9), (3, 8), (4, 7), (4, 9), (5, 6), (5, 8), (6, 7), (6, 9), (7, 8), (8, 9) plus the reverses, 20 possible values in all.  For a1, there are 5 possible values (0 through 4), so the possible solutions are 20*5=100.~%"))
    (newline)
    (display (format #f "Four digit reversible numbers: we must have n+reverse(n) = (a0+a3)+(a1+a2)*10^1+(a1+a2)*10^2+(a0+a3)*10^3.  Case (1) all terms are less than 10 and are odd.  There are 20 pairs of (a0, a3) (like in the two digit case), and there are 30 pairs of (a1, a2), (20 pairs 2-digit pairs, plus (0, 1), (0, 3), (0, 5), (0, 7), (0, 9) and the reverse order).  So there are 20*30=600 solutions. Case (2) (a0+a3)>10 and odd, then the 10's factor must be even and (a1+a2)>10, but this will require that the 100's factor produce a carry over into the 10^3 term, turning it from an odd number into an even number, so it can't be a solution.~%"))
    (newline)
    (display (format #f "Five digit reversible numbers: we must have n+reverse(n) = (a0+a4)+(a1+a3)*10+(a2+a2)*10^2+(a1+a3)*10^3+(a0+a4)*10^4.  Case (1) all terms are odd and less less than 10, this has no solution since the 10^2 term is always even.  Case (2) (a1+a3)>10 since (a2+a2) always even, and you need a carry to increase (a2+a2) by 1. a0+a4 must be odd from the 10^0 factor, but with a carry from (a1+a3)>10 on the 10^3 factor, we see that the 10^4 factor will always be even.  So there cannot be any five digit reversible numbers.~%"))
    (newline)
    (display (format #f "Six digit reversible numbers: we must have n+reverse(n) = (a0+a5)+(a1+a4)*10^1+(a2+a3)*10^2+(a2+a3)*10^3+(a1+a4)*10^4+(a0+a5)*10^5.  Case (1) If all sums are less than 10 and odd then solutions will be possible.  There are 20*30*30=18,000 solutions.  Case (2) If (a0+a5)>10 and odd, then (a1+a4) (the 10's factor), must be even, and if (a1+a4)>10, the 10^4 factor will carry a one over to the 10^5 term, which will make it even, so (a1+a4)>10 is not allowed.  (a1+a4)<10 and even.  (a2+a3)>10 since the 10^4 factor is even, but this is not allowed since the two consecutive (a2+a3) terms will cause one term to be odd and the other to be even. This means that case (2) has no solutions.~%"))
    (newline)
    (display (format #f "Seven digit reversible numbers: we must have n+reverse(n) = (a0+a6)+(a1+a5)*10^1+(a2+a4)*10^2+(a3+a3)*10^3+(a2+a4)*10^4+(a1+a5)*10^5+(a0+a6)*10^6.  Case (1) There are no solutions when all factors are less than 10, since (a3+a3) will always be even.  Case (2) (a0+a6)>10 and odd : (a1+a5)<10 and even, so (a2+a4)>10 and odd, since the 10^4 factor needs a carry to make it odd, (a3+a3)>=10.  Possible odd pairs for (a0+a6)>10 are (2, 9), (3, 8), (4, 7), (4, 9), (5, 6), (5, 8), (6, 7), (6, 9), (7, 8), (8, 9), plus reverses, 20 in all. The allowable even pairs for (a1+a5) are (0, 2), (0, 4), (0, 6), (0, 8), (2, 4), (2, 6), (1, 3), (1, 5), (1, 7), (3, 5), (0, 0), (1, 1), (2, 2), (3, 3) (4, 4), plus reverses, 25 in all.  The allowable odd pairs for (a2+a4)>10 and odd are the same as for (a0+a6), 20 pairs. If (a3+a3)>=10, then a3 can be 5, 6, 7, 8, 9, or 5 possible values.  This means there are 20*25*20*5=50,000.  Case (3) (a0+a6)<10 and odd : (a1+a5)<10 and odd, (a2+a4)>10 so that the carry can make the (a3+a3) term odd, but the 10^4 term will turn the 10^5 term into an even number, so there are no solutions when (a0+a6)<10.~%"))
    (newline)
    (display (format #f "Eight digit reversible numbers: we must have n+reverse(n) = (a0+a7)+(a1+a6)*10^1+(a2+a5)*10^2+(a3+a4)*10^3+(a3+a4)*10^4+(a2+a5)*10^5+(a1+a6)*10^6+(a0+a7)*10^7.  Case (1) all factors are less than 10 and odd, there are 20*30*30*30=540,000.  Case (2) (a0+a7)>10 and odd : (a1+a6) even, if (a1+a6)>10, then it will carry over to the 10^7 factor and make it even, so (a1+a6)<10 and even.  Must have (a2+a5)>10 since the 10^6 factor is even, (a2+a5) odd because the 10^1 factor (a1+a6)<10 will not contribute a carry.  Must have (a3+a4)<10 and even because of the carry from the (a2+a5)>10, but since there is no carry from the 10^3 term, the 10^4 term will remain even. So there are no solutions for case (2).~%"))
    (newline)
    (display (format #f "Nine digit reversible numbers: we must have n+reverse(n) = (a0+a8)+(a1+a7)*10^1+(a2+a6)*10^2+(a3+a5)*10^3+(a4+a4)*10^4+(a3+a5)*10^5+(a2+a6)*10^6+(a1+a7)*10^7+(a0+a8)*10^8.  Case (1) there are no solutions if all factors are less than 10 and odd since the 10^4 factor is always even.  Case (2) (a0+a8)>10 and odd : (a1+a7)<10 and even because of the 10^7 term, must have (a2+a6)>10 because of the 10^6 term, and odd because of the 10^2 term.  Then (a3+a5)>10 because of the 10^5 term and even because of the 10^3 term (need to absorb a carry from (a2+a6)), this will make the 10^4 factor odd.  a4>5 and even, but will turn the 10^6 factor (a2+a6) even, so there are no solutions for case (2).~%"))
    (newline)
    (display (format #f "There are some patterns to observe.  For an even number of digits n=2k, k>=1, there are 20*30^(k-1) solutions.~%"))
    (newline)
    (display (format #f "For an odd number of digits there are two distinct types, one with solutions and one without.  For those with solutions (n=3, or n=7), say n=4k+3, k>=0, then there are an odd number of terms on either side of the central term (the (a1+a1) factor for 3 digit numbers, or the (a3+a3) factor for 7 digit numbers).  This is what is required since each term greater than 10 needs a neighboring term that is less than 10, until you reach one before the center term, it must be odd and greater than 10, the center term will be even, and one will be the same as the one before. The number of solutions are 20^(k+1)*25^k*5.~%"))
    (newline)
    (display (format #f "For those odd number of digits without solutions, there are an even number of factors on either side of the central term (the (a2+a2) term for 5 digit numbers or the (a4+a4) term for the 9 digit numbers), and it can be seen it cannot support the delicate balance of odd greater than 10/even less than 10 terms.~%"))
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

    (let ((max-digits 3)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-digits debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-digits 9)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-digits debug-flag)
	   ))
	))

    (newline)
    ))
