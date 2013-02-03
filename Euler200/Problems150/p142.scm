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
;;;###  project euler 142                             ###
;;;###                                                ###
;;;###  last updated December 20, 2012                ###
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
;;;### uses the algorithm found at http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer
;;;### (logand 1^2 15) = 1, (logand 2^2 15) = 4, (logand 3^2 15) 9,
;;;### (logand 4^2 15) = 0, (logand 5^2 15) = 9, (logand 6^2 15) = 4,
;;;### (logand 7^2 15) = 1, (logand 8^2 15) = 0, (logand 9^2 15) = 1,
;;;### (logand 10^2 15) = 4, (logand 11^2 15) = 9, (logand 12^2 15) = 0,...
(define (is-perfect-square? anum)
  (let ((lan (logand anum 15)))
    (begin
      (cond
       ((< anum 0)
        (begin
          #f
          ))
       ((or (equal? lan 0) (equal? lan 1)
            (equal? lan 4) (equal? lan 9))
        (begin
          (let ((ltmp (exact-integer-sqrt anum)))
            (begin
              (equal? (* ltmp ltmp) anum)
              ))
          ))
       (else
        (begin
          #f
          )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-perfect-square-1)
  (let ((sub-name "test-is-perfect-square-1")
	(test-list
	 (list
          (list -10 #f) (list 0 #t) (list 1 #t)
	  (list 2 #f) (list 3 #f) (list 4 #t) (list 5 #f)
          (list 6 #f) (list 7 #f) (list 8 #f) (list 9 #t)
          (list 10 #f) (list 11 #f) (list 12 #f) (list 13 #f)
          (list 14 #f) (list 15 #f) (list 16 #t) (list 17 #f)
          (list 18 #f) (list 19 #f) (list 20 #f) (list 21 #f)
          (list 22 #f) (list 23 #f) (list 24 #f) (list 25 #t)
          (list 26 #f) (list 27 #f) (list 28 #f) (list 29 #f)
          (list 30 #f) (list 31 #f) (list 32 #f) (list 33 #f)
          (list 36 #t) (list 49 #t) (list 64 #t) (list 81 #t)
          (list 100 #t) (list 121 #t) (list 144 #t) (list 169 #t)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe-bool (list-ref this-list 1)))
	     (let ((result-bool (is-perfect-square? nn)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display
                        (format #f "~a : (~a) : error : nn=~a, "
                                sub-name test-label-index nn))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
                                (if shouldbe-bool "true" "false")
                                (if result-bool "true" "false")))
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
(define-syntax display-is-perfect-square
  (syntax-rules ()
    ((display-is-perfect-square sq-num dstring)
     (begin
       (if (>= sq-num 0)
           (begin
             (let ((bflag (is-perfect-square? sq-num))
                   (sqrt-num (exact-integer-sqrt sq-num)))
               (begin
                 (display
                  (ice-9-format:format #f "~a = ~:d = ~d^2~a~%"
                                       dstring sq-num sqrt-num
                                       (if bflag "" "******error*******")
                                       ))
                 ))
             ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info xx yy zz rsum min-xx min-yy min-zz min-sum count)
     (begin
       (display
        (ice-9-format:format
         #f "  (xx, yy, zz) = (~:d, ~:d, ~:d), sum = ~:d, "
         xx yy zz rsum))
       (display
        (ice-9-format:format
         #f "min = (~:d, ~:d, ~:d), sum = ~:d, count=~:d~%"
         min-xx min-yy min-zz min-sum count))
       (display-is-perfect-square (+ xx yy) "x+y")
       (display-is-perfect-square (- xx yy) "x-y")
       (display-is-perfect-square (+ xx zz) "x+z")
       (display-is-perfect-square (- xx zz) "x-z")
       (display-is-perfect-square (+ yy zz) "y+z")
       (display-is-perfect-square (- yy zz) "y-z")
       (force-output)
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
(define-syntax display-plus-line
  (syntax-rules ()
    ((display-plus-line astring min-xx min-yy)
     (begin
       (let ((xy (+ min-xx min-yy)))
	 (let ((xy-sqr (exact-integer-sqrt xy)))
	   (begin
	     (display (ice-9-format:format
		       #f "  ~a = ~:d = ~:d^2~%" astring xy xy-sqr))
	     (force-output)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax display-minus-line
  (syntax-rules ()
    ((display-minus-line astring min-xx min-yy)
     (begin
       (let ((xy (- min-xx min-yy)))
	 (let ((xy-sqr (exact-integer-sqrt xy)))
	   (begin
	     (display (ice-9-format:format
		       #f "  ~a = ~:d = ~:d^2~%" astring xy xy-sqr))
	     (force-output)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax process-inner-loop
  (syntax-rules ()
    ((process-inner-loop max-num aa-2 bb bb-2
                         min-sum min-xx min-yy min-zz
                         count debug-flag)
     (begin
       (let ((end-loop-flag #f)
             (start-ee (+ bb 2))
             (xtmp (+ aa-2 aa-2 bb-2)))
         (begin
           (do ((ee start-ee (+ ee 2)))
               ((or (> ee max-num)
                    (equal? end-loop-flag #t)))
             (begin
               (let ((ee-2 (* ee ee)))
                 (let ((xx (/ (+ xtmp ee-2) 2))
                       (yy (/ (+ ee-2 bb-2) 2))
                       (zz (/ (- ee-2 bb-2) 2)))
                   (begin
                     (let ((sum (+ xx yy zz)))
                       (begin
                         (if (and
                              (is-perfect-square? (+ xx yy))
                              (is-perfect-square? (+ xx zz))
                              (is-perfect-square? (- xx zz)))
                             (begin
                               (set! count (1+ count))

                               (if (equal? debug-flag #t)
                                   (begin
                                     (display-debug-info
                                      xx yy zz sum min-xx min-yy min-zz
                                      min-sum count)
                                     ))

                               (if (or (< min-sum 0)
                                       (< sum min-sum))
                                   (begin
                                     (set! min-sum sum)
                                     (set! min-xx xx)
                                     (set! min-yy yy)
                                     (set! min-zz zz)
                                     ))
                               ))

                         (if (and (> min-sum 0)
                                  (> sum min-sum))
                             (begin
                               (set! end-loop-flag #t)
                               ))
                         ))
                     ))
                 )))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num status-num debug-flag)
  (let ((min-xx -1)
	(min-yy -1)
	(min-zz -1)
	(min-sum -1)
	(count 0)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (do ((aa 1 (1+ aa)))
          ((> aa max-num))
        (begin
          (let ((aa-2 (* aa aa)))
            (begin
              (do ((bb (1+ aa) (1+ bb)))
                  ((> bb max-num))
                (begin
                  (let ((bb-2 (* bb bb)))
                    (begin
                      (if (is-perfect-square? (+ aa-2 bb-2))
                          (begin
                            (process-inner-loop
                             max-num aa-2 bb bb-2
                             min-sum min-xx min-yy min-zz
                             count debug-flag)
                            ))
                      ))
                  ))
              ))

	  (if (zero? (modulo aa status-num))
	      (begin
		(display-status-info aa max-num min-sum count
				     start-jday)
		))
	  ))

      (if (> min-sum 0)
          (begin
            (display (ice-9-format:format
                      #f "The minimum sum = ~:d, (x, y, z) = (~:d, ~:d, ~:d).~%"
                      min-sum min-xx min-yy min-zz))
            (force-output)

            (display-minus-line "x-y" min-xx min-yy)
            (display-minus-line "y-z" min-yy min-zz)
            (display-minus-line "x-z" min-xx min-zz)
            (display-plus-line "y+z" min-yy min-zz)
            (display-plus-line "x+y" min-xx min-yy)
            (display-plus-line "x+z" min-xx min-zz))
          (begin
            (display (format #f "no results found.~%"))
            ))

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
    (display (format #f "Project Euler 142 - Find the smallest x + y + z with integers x > y > z > 0 such that x + y, x - y, x + z, x - z, y + z, y - z are all perfect squares.~%"))
    (newline)
    (display (format #f "Let x-y=a^2, y-z=b^2, x-z=c^2, x+y=d^2, y+z=e^2, x+z=f^2.~%"))
    (newline)
    (display (format #f "(1) x=(a^2+d^2)/2, (2) y=(b^2+e^2)/2, (3) z=(f^2-c^2)/2~%"))
    (display (format #f "(4) x=(c^2+f^2)/2, (5) y=(d^2-a^2)/2, (6) z=(e^2-b^2)/2.~%"))
    (newline)
    (display (format #f "from x=(a^2+d^2)/2=(c^2+f^2)/2 we see that f^2=a^2+d^2-c^2.~%"))
    (display (format #f "from y=(b^2+e^2)/2=(d^2-a^2)/2 we see that d^2=e^2+a^2+b^2. (7)~%"))
    (display (format #f "from z=(f^2-c^2)/2=(e^2-b^2)/2 we see that f^2=e^2-b^2+c^2.~%"))
    (display (format #f "from d^2=x+y=(c^2+f^2+b^2+e^2)/2=(c^2+e^2-b^2+c^2+b^2+e^2)/2=e^2+c^2~%"))
    (display (format #f "from (7), d^2=e^2+c^2=e^2+a^2+b^2 -> a^2+b^2=c^2 (pythagorean theorem)~%"))
    (display (format #f "which is from the identity (x-y) + (y-z) = (x-z)~%"))
    (display (format #f "noticed by http://d.hatena.ne.jp/inamori/20100802/p1~%"))
    (newline)
    (display (format #f "So we can eliminate c, d, and f.~%"))
    (display (format #f "(8) x=(2a^2+b^2+e^2)/2, (9) y=(e^2+b^2)/2, (10) z=(e^2-b^2)/2~%"))
    (display (format #f "Since x, y, and z are integers, we see that when b is odd, then~%"))
    (display (format #f "e must be odd, and when b is even, e must be even.  This will help~%"))
    (display (format #f "to reduce the amount of work done in the innner-most loop.~%"))
    (newline)
    (display (format #f "from x>y, (2a^2+b^2+e^2)/2>(e^2+b^2)/2, always true.~%"))
    (display (format #f "from y>z, (e^2+b^2)/2>(e^2-b^2)/2, always true.~%"))
    (display (format #f "from z>0, (e^2-b^2)/2>0, implies e^2>b^2~%"))
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

    (let ((max-num 1000)
	  (status-num 200)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)
    ))
