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

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

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
;;; assume input-string is in lower-case
(define (populate-roman-char-hash-table! roman-char-htable)
  (let ((rlist
	 (list
	  (list #\i 1) (list #\v 5)
	  (list #\x 10) (list #\l 50)
	  (list #\c 100) (list #\d 500)
	  (list #\m 1000))))
    (begin
      (hash-clear! roman-char-htable)

      (for-each
       (lambda (a-list)
	 (let ((this-char (list-ref a-list 0))
	       (this-value (list-ref a-list 1)))
	   (begin
	     (hash-set! roman-char-htable this-char this-value)
	     )))
       rlist)
      )))

;;;#############################################################
;;;#############################################################
;;; assume input-string is in lower-case
(define (roman-string-to-number input-string roman-char-htable)
  (let ((input-length (string-length input-string))
	(char-list (string->list input-string))
	(result-num 0)
	(previous-num -1))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii input-length))
	(begin
	  (let ((this-char (list-ref char-list ii)))
	    (let ((this-num (hash-ref roman-char-htable this-char -1)))
	      (begin
		(if (< this-num 0)
		    (begin
		      (display (format #f "roman-string-to-number(~a) error : invalid character ~a~%"
				       input-string this-char))
		      (force-output)
		      (quit)
		      ))

		(if (< previous-num 0)
		    (begin
		      (set! previous-num this-num))
		    (begin
		      (if (< previous-num this-num)
			  (begin
			    ;;; then we have a subtraction pair, like "IV" or "IX"
			    (let ((sub-num (- this-num previous-num)))
			      (begin
				(set! result-num (+ result-num sub-num))
				(set! previous-num -1)
				)))
			  (begin
			    (set! result-num (+ result-num previous-num))
			    (set! previous-num this-num)
			    ))
		      ))
		)))
	  ))
      (if (> previous-num 0)
	  (begin
	    (set! result-num (+ result-num previous-num))
	    ))

      result-num
      )))

;;;#############################################################
;;;#############################################################
(define (test-roman-string-to-number-1)
  (let ((sub-name "test-roman-string-to-number-1")
	(test-list
	 (list
	  (list "i" 1) (list "ii" 2) (list "iii" 3)
	  (list "iv" 4) (list "v" 5) (list "vi" 6)
	  (list "vii" 7) (list "viii" 8) (list "ix" 9)
	  (list "x" 10) (list "xi" 11) (list "xii" 12)
	  (list "xiii" 13) (list "xiv" 14) (list "xv" 15)
	  (list "xvi" 16) (list "xvii" 17) (list "xviii" 18)
	  (list "xix" 19) (list "xx" 20)
	  (list "xxix" 29) (list "il" 49) (list "lil" 99)
	  (list "ic" 99) (list "ci" 101) (list "cix" 109)
	  (list "cxl" 140) (list "cil" 149) (list "lc" 50)
	  (list "cxc" 190) (list "cic" 199) (list "d" 500)
	  (list "id" 499) (list "m" 1000) (list "mmx" 2010)
	  (list "iiiiiiiiiiiiiiii" 16) (list "viiiiiiiiiii" 16)
	  (list "vviiiiii" 16) (list "xiiiiii" 16)
	  (list "vvvi" 16) (list "xvi" 16)
	  ))
	(roman-char-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (populate-roman-char-hash-table! roman-char-htable)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((rstring (string-downcase (list-ref alist 0)))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (roman-string-to-number rstring roman-char-htable)))
	       (if (not (equal? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for string = ~s, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index rstring shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; use most efficient representation
(define (number-to-roman-string input-num)
  (let ((tmp-num input-num)
	(result-string ""))
    (begin
      (while
       (> tmp-num 0)
       (begin
	 (cond
	  ((>= tmp-num 1000)
	   (begin
	     (set! result-string (string-append result-string "m"))
	     (set! tmp-num (- tmp-num 1000))
	     ))
	  ((and (< tmp-num 1000) (>= tmp-num 900))
	   (begin
	     ;;; should use a subtractive pair CM if it's 900 <= n < 1000
	     (set! result-string (string-append result-string "cm"))
	     (set! tmp-num (- tmp-num 900))
	     ))
	  ((>= tmp-num 500)
	   (begin
	     (set! result-string (string-append result-string "d"))
	     (set! tmp-num (- tmp-num 500))
	     ))
	  ((and (< tmp-num 500) (>= tmp-num 400))
	   (begin
	     ;;; should use a subtractive pair CD if it's 400 <= n < 500
	     (set! result-string (string-append result-string "cd"))
	     (set! tmp-num (- tmp-num 400))
	     ))
	  ((>= tmp-num 100)
	   (begin
	     (set! result-string (string-append result-string "c"))
	     (set! tmp-num (- tmp-num 100))
	     ))
	  ((and (< tmp-num 100) (>= tmp-num 90))
	   (begin
	     ;;; should use a subtractive pair XC if it's 90 <= n < 100
	     (set! result-string (string-append result-string "xc"))
	     (set! tmp-num (- tmp-num 90))
	     ))
	  ((>= tmp-num 50)
	   (begin
	     (set! result-string (string-append result-string "l"))
	     (set! tmp-num (- tmp-num 50))
	     ))
	  ((and (< tmp-num 50) (>= tmp-num 40))
	   (begin
	     ;;; should use a subtractive pair XL if it's 40 <= n < 50
	     (set! result-string (string-append result-string "xl"))
	     (set! tmp-num (- tmp-num 40))
	     ))
	  ((>= tmp-num 10)
	   (begin
	     (set! result-string (string-append result-string "x"))
	     (set! tmp-num (- tmp-num 10))
	     ))
	  ((= tmp-num 9)
	   (begin
	     ;;; should use a subtractive pair IX if it's 9 = n
	     (set! result-string (string-append result-string "ix"))
	     (set! tmp-num (- tmp-num 9))
	     ))
	  ((>= tmp-num 5)
	   (begin
	     (set! result-string (string-append result-string "v"))
	     (set! tmp-num (- tmp-num 5))
	     ))
	  ((= tmp-num 4)
	   (begin
	     ;;; should use a subtractive pair IV if it's 4 = n
	     (set! result-string (string-append result-string "iv"))
	     (set! tmp-num (- tmp-num 4))
	     ))
	  ((>= tmp-num 1)
	   (begin
	     (set! result-string (string-append result-string "i"))
	     (set! tmp-num (- tmp-num 1))
	     ))
	  )))

      result-string
      )))

;;;#############################################################
;;;#############################################################
(define (test-number-to-roman-string-1)
  (let ((sub-name "test-number-to-roman-string-1")
	(test-list
	 (list
	  (list "i" 1) (list "ii" 2) (list "iii" 3)
	  (list "iv" 4) (list "v" 5) (list "vi" 6)
	  (list "vii" 7) (list "viii" 8) (list "ix" 9)
	  (list "x" 10) (list "xi" 11) (list "xii" 12)
	  (list "xiii" 13) (list "xiv" 14) (list "xv" 15)
	  (list "xvi" 16) (list "xvii" 17) (list "xviii" 18)
	  (list "xix" 19) (list "xx" 20) (list "xxix" 29)
	  (list "xxxix" 39) (list "xlix" 49) (list "lix" 59)
	  (list "lxviii" 68) (list "lxxviii" 78) (list "lxxxix" 89)
	  (list "xc" 90) (list "xci" 91) (list "xcviii" 98)
	  (list "xcix" 99) (list "c" 100) (list "ci" 101)
	  (list "cix" 109) (list "cxix" 119) (list "cxxix" 129)
	  (list "cxl" 140) (list "cxlix" 149) (list "l" 50)
	  (list "cxc" 190) (list "cxcix" 199) (list "d" 500)
	  (list "cdxcix" 499) (list "m" 1000) (list "mmx" 2010)
	  (list "mmmm" 4000) (list "cmxcix" 999)
	  (list "mmmmmmmmmcmxcix" 9999)
	  (list "xvi" 16)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((shouldbe (list-ref alist 0))
		 (test-num (list-ref alist 1)))
	     (let ((result (number-to-roman-string test-num)))
	       (if (not (string-ci=? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for num = ~a, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index test-num shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; assume input-string is in lower-case
(define (simplify-roman-string input-string roman-char-htable)
  (let ((num-value (roman-string-to-number input-string roman-char-htable)))
    (let ((simple-string (number-to-roman-string num-value)))
      (begin
	simple-string
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-simplify-roman-string-1)
  (let ((sub-name "test-simplify-roman-string-1")
	(test-list
	 (list
	  (list "MMMMDCLXXII" "mmmmdclxxii")
	  (list "MMDCCCLXXXIII" "mmdccclxxxiii")
	  (list "MMMDLXVIIII" "mmmdlxix")
	  (list "MMMMDXCV" "mmmmdxcv")
	  (list "DCCCLXXII" "dccclxxii")
	  (list "MMCCCVI" "mmcccvi")
	  (list "iiiiiiiiiiiiiiii" "xvi") (list "viiiiiiiiiii" "xvi")
	  (list "vviiiiii" "xvi") (list "xiiiiii" "xvi")
	  (list "vvvi" "xvi") (list "xvi" "xvi")
	  ))
	(roman-char-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (populate-roman-char-hash-table! roman-char-htable)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((roman-string (string-downcase (list-ref alist 0)))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (simplify-roman-string roman-string roman-char-htable)))
	       (if (not (equal? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for roman-string = ~a, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index roman-string shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; assume input-string is in lower-case
(define (count-chars-saved input-string roman-char-htable)
  (let ((simple-string (simplify-roman-string input-string roman-char-htable)))
    (let ((old-slen (string-length input-string))
	  (new-slen (string-length simple-string)))
      (begin
	(- old-slen new-slen)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-chars-saved-1)
  (let ((sub-name "test-count-chars-saved-1")
	(test-list
	 (list
	  (list "MMMMDCLXXII" 0)
	  (list "MMDCCCLXXXIII" 0)
	  (list "MMMDLXVIIII" 3)
	  (list "MMMMDXCV" 0)
	  (list "DCCCLXXII" 0)
	  (list "MMCCCVI" 0)
	  (list "iiiiiiiiiiiiiiii" 13) (list "viiiiiiiiiii" 9)
	  (list "vviiiiii" 5) (list "xiiiiii" 4)
	  (list "vvvi" 1) (list "xvi" 0)
	  ))
	(roman-char-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (populate-roman-char-hash-table! roman-char-htable)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((roman-string (string-downcase (list-ref alist 0)))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (count-chars-saved roman-string roman-char-htable)))
	       (if (not (equal? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for roman-string = ~a, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index roman-string shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; returns a list of names, lower case
;;;	  (list "iiiiiiiiiiiiiiii" 13) (list "viiiiiiiiiii" 9)
;;;	  (list "vviiiiii" 5) (list "xiiiiii" 4)
;;;	  (list "vvvi" 1) (list "xvi" 0)
(define (list-loop roman-numerals-list debug-flag)
  (let ((roman-char-htable (make-hash-table 10))
	(chars-saved 0)
	(counter 0))
    (begin
      (populate-roman-char-hash-table! roman-char-htable)

      (for-each
       (lambda(rstring)
	 (begin
	   (let ((this-string (string-downcase rstring)))
	     (let ((this-saved (count-chars-saved this-string roman-char-htable)))
	       (begin
		 (set! chars-saved (+ chars-saved this-saved))
		 (set! counter (1+ counter))
		 (if (equal? debug-flag #t)
		     (begin
		       (let ((next-string (simplify-roman-string this-string roman-char-htable)))
			 (begin
			   (display
			    (ice9-format:format #f "(~:d) ~s -> ~s : chars saved = ~:d : so far chars saved = ~:d~%"
						counter this-string next-string
						this-saved chars-saved))
			   (force-output)
			   ))
		       ))
		 )))
	   ))
       roman-numerals-list)

      (display (ice9-format:format #f "total number of roman numerals = ~:d~%" counter))
      (display (ice9-format:format #f "total characters saved = ~:d~%" chars-saved))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
;;; returns a list of names, lower case
(define (read-in-file fname debug-flag)
  (let ((roman-list (list)))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\n\r") (ice9-rdelim:read-delimited ",\n\r")))
		    ((eof-object? line))
		  (begin
		    (cond
		     ((not (eof-object? line))
		      (let ((this-string (string-downcase (string-delete #\" line))))
			(begin
			  (if (and (string? this-string)
				   (> (string-length this-string) 0))
			      (begin
				(set! roman-list (cons this-string roman-list))
				))
			  )))
		     )))
		))
	    ))

      (if (and (list? roman-list) (> (length roman-list) 0))
	  (begin
	    (set! roman-list (reverse roman-list))
	    (list-loop roman-list debug-flag)
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
    (display (format #f "Project Euler 89: - The rules for writing Roman numerals allow for many ways of writing each number (see About Roman Numerals...). However, there is always a 'best' way of writing a particular number.~%"))
    (newline)
    (display (format #f "For example, the following represent all of the legitimate ways of writing the number sixteen:~%"))
    (newline)
    (display (format #f "IIIIIIIIIIIIIIII~%"))
    (display (format #f "VIIIIIIIIIII~%"))
    (display (format #f "VVIIIIII~%"))
    (display (format #f "XIIIIII~%"))
    (display (format #f "VVVI~%"))
    (display (format #f "XVI~%"))
    (newline)
    (display (format #f "The last example being considered the most efficient, as it uses the least number of numerals.~%"))
    (newline)
    (display (format #f "The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals; that is, they are arranged in descending units and obey the subtractive pair rule (see About Roman Numerals... for the definitive rules for this problem).~%"))
    (newline)
    (display (format #f "Find the number of characters saved by writing each of these in their minimal form.~%"))
    (newline)
    (display (format #f "Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((start-jday (srfi-19:current-julian-day))
	  (counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-roman-string-to-number-1 counter)
	   (run-test test-number-to-roman-string-1 counter)
	   (run-test test-simplify-roman-string-1 counter)
	   (run-test test-count-chars-saved-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))

    (let ((test-list
	   (list "iiiiiiiiiiiiiiii" "viiiiiiiiiii"
		 "vviiiiii" "xiiiiii" "vvvi" "xvi"))
	  (debug-flag #t))
      (begin
	(list-loop test-list debug-flag)
	))

    (newline)
    (force-output)

    (let ((filename "roman.txt")
	  (start-jday (srfi-19:current-julian-day))
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (display (format #f "processing file ~a~%" filename))
	   (force-output)
	   (read-in-file filename debug-flag)
	   ))
	))

    (newline)
    ))
