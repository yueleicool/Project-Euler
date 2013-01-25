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

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (srfi-11:let-values
       (((next-num this-digit) (euclidean/ this-num 10)))
       (begin
	 (local-loop next-num (cons this-digit acc-list))
	 )))))
  (let ((result-list (local-loop this-num (list))))
    result-list
    ))

;;;#############################################################
;;;#############################################################
(define (test-split-digits-list-1)
  (let ((sub-name "test-split-digits-list-1")
	(test-list
	 (list
	  (list 3 (list 3)) (list 4 (list 4)) (list 5 (list 5))
	  (list 13 (list 1 3)) (list 14 (list 1 4)) (list 15 (list 1 5))
	  (list 23 (list 2 3)) (list 24 (list 2 4)) (list 25 (list 2 5))
	  (list 123 (list 1 2 3)) (list 1234 (list 1 2 3 4)) (list 98765 (list 9 8 7 6 5))
	  (list 341608987 (list 3 4 1 6 0 8 9 8 7))
	  (list 116696699999166169 (list 1 1 6 6 9 6 6 9 9 9 9 9 1 6 6 1 6 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (split-digits-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index test-num
					shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (digit-list-to-number llist)
  (let ((this-sum
	 (srfi-1:fold
	  (lambda (this-elem previous)
	    (+ this-elem (* 10 previous)))
	  0 llist)))
    this-sum
    ))

;;;#############################################################
;;;#############################################################
(define (test-digit-list-to-number-1)
  (let ((sub-name "test-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1) 1)
	  (list (list 1 2) 12)
	  (list (list 1 2 3) 123)
	  (list (list 4 5 6 7) 4567)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
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
(define (squared-numbers-pairs-hash! squares-htable start-num end-num)
  (let ((intermediate-htable (make-hash-table 100)))
    (begin
      ;;; first load up intermediate-htable with pairs of squares
      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((num-square (* ii ii)))
	    (let ((pair-list (list ii num-square))
		  (num-list (sort (split-digits-list num-square) <)))
	      (let ((sorted-num (digit-list-to-number num-list)))
		(let ((this-list (hash-ref intermediate-htable num-list (list))))
		  (begin
		    (if (equal? (member pair-list this-list) #f)
			(begin
			  (hash-set! intermediate-htable num-list (cons pair-list this-list))
			  ))
		    )))
	      ))
	  ))

      ;;; return only those that have multiple pairs
      (hash-clear! squares-htable)

      (hash-for-each
       (lambda (key value)
	 (begin
	   (if (> (length value) 1)
	       (begin
		 (hash-set! squares-htable key value)
		 ))
	   )) intermediate-htable)
      )))

;;;#############################################################
;;;#############################################################
(define (test-squared-numbers-pairs-hash-1)
  (let ((sub-name "test-squared-numbers-pairs-hash-1")
	(test-list
	 (list
	  (list 1 100 (list (list (list 1 2 6 9)
				  (list (list 36 1296)
					(list 54 2916)
					(list 96 9216)))))
	  ))
	(pairs-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! pairs-htable)

	   (let ((start-num (list-ref alist 0))
		 (end-num (list-ref alist 1))
		 (shouldbe-list-list (list-ref alist 2)))
	     (begin
	       (squared-numbers-pairs-hash! pairs-htable start-num end-num)

	       (for-each
		(lambda (b-list)
		  (begin
		    (let ((key (list-ref b-list 0))
			  (shouldbe (list-ref b-list 1)))
		      (let ((result (hash-ref pairs-htable key (list))))
			(begin
			  (if (not (equal? (length shouldbe) (length result)))
			      (begin
				(display (format #f "~a : (~a) : length error : key = ~a, shouldbe = ~a, result = ~a~%"
						 sub-name test-label-index key shouldbe result))
				(quit)
				))
			  (for-each
			   (lambda (shouldbe-pair)
			     (begin
			       (if (equal? (member shouldbe-pair result) #f)
				   (begin
				     (display (format #f "~a : (~a) : error : key = ~a, missing word ~a, shouldbe = ~a, result = ~a~%"
						      sub-name test-label-index key
						      shouldbe-pair shouldbe result))
				     (quit)
				     ))
			       )) shouldbe)
			  )))
		    )) shouldbe-list-list)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (word-list-to-multiple-string-hash! multiple-htable word-list)
  (let ((intermediate-htable (make-hash-table 100)))
    (begin
      (for-each
       (lambda (this-word)
	 (begin
	   (let ((sorted-word
		  (list->string
		   (sort (string->list this-word) char<?))))
	     (let ((this-word-list (hash-ref intermediate-htable sorted-word #f)))
	       (begin
		 (if (equal? this-word-list #f)
		     (begin
		       (hash-set! intermediate-htable sorted-word (list this-word)))
		     (begin
		       (set! this-word-list (append this-word-list (list this-word)))
		       (hash-set! intermediate-htable sorted-word this-word-list)
		       ))
		 )))
	   )) word-list)

      (hash-clear! multiple-htable)

      (hash-for-each
       (lambda (key value)
	 (begin
	   (if (and (list? value) (> (length value) 1))
	       (begin
		 (hash-set! multiple-htable key value)
		 ))
	   )) intermediate-htable)

      (hash-clear! intermediate-htable)
      )))

;;;#############################################################
;;;#############################################################
(define (test-word-list-to-multiple-string-hash-1)
  (let ((sub-name "test-word-list-to-multiple-string-hash-1")
	(test-list
	 (list
	  (list (list "care" "race")
		(list (list "acer" (list "care" "race"))))
	  (list (list "cab" "bac" "edfg")
		(list (list "abc" (list "cab" "bac"))))
	  ))
	(wl-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! wl-htable)

	   (let ((wlist (list-ref alist 0))
		 (shouldbe-list-list (list-ref alist 1)))
	     (begin
	       (word-list-to-multiple-string-hash! wl-htable wlist)
	       (for-each
		(lambda (b-list)
		  (begin
		    (let ((key (list-ref b-list 0))
			  (shouldbe (list-ref b-list 1)))
		      (let ((result (hash-ref wl-htable key (list))))
			(begin
			  (if (not (equal? (length shouldbe) (length result)))
			      (begin
				(display (format #f "~a : (~a) : length error : key = ~a, shouldbe = ~a, result = ~a~%"
						 sub-name test-label-index key shouldbe result))
				(quit)
				))
			  (for-each
			   (lambda (shouldbe-word)
			     (begin
			       (if (equal? (member shouldbe-word result) #f)
				   (begin
				     (display (format #f "~a : (~a) : error : key = ~a, missing word ~a, shouldbe = ~a, result = ~a~%"
						      sub-name test-label-index key shouldbe-word shouldbe result))
				     (quit)
				     ))
			       )) shouldbe)
			  )))
		    )) shouldbe-list-list)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; returns a list of lists
(define (read-in-file fname)
  (let ((results-list (list))
	(counter 0))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\r\n") (ice9-rdelim:read-delimited ",\r\n")))
		    ((eof-object? line))
		  (begin
		    (if (and (not (eof-object? line))
			     (> (string-length line) 0))
			(begin
			  (let ((this-string (string-downcase
					      (string-delete #\" line))))
			    (begin
			      (set! counter (1+ counter))
			      (set! results-list
				    (cons this-string results-list))
			      ))
			  ))
		    ))
		))

	    (display (ice9-format:format #f "read in ~a words from ~a~%" counter fname))
	    (newline)
	    (force-output)

	    (reverse results-list))
	  (begin
	    (list)
	    )))))

;;;#############################################################
;;;#############################################################
(define (add-to-chars-htable! char-htable dlen dlist slist)
  (let ((ok-flag #t)
	(tmp-htable (make-hash-table 100)))
    (begin
      (hash-clear! char-htable)

      (do ((ii 0 (1+ ii)))
	  ((>= ii dlen))
	(begin
	  (let ((digit (list-ref dlist ii))
		(char (list-ref slist ii)))
	    (begin
	      (let ((this-digit (hash-ref char-htable char #f)))
		(begin
		  (if (equal? this-digit #f)
		      (begin
			(hash-set! char-htable char digit))
		      (begin
			;;; don't allow the same character to have different digits
			(if (not (equal? this-digit digit))
			    (set! ok-flag #f))
			))
		  ))
	      ))
	  ))

      ;;; don't allow different characters to have the same digit
      (do ((ii 0 (1+ ii)))
	  ((>= ii dlen))
	(begin
	  (let ((digit (list-ref dlist ii))
		(char (list-ref slist ii)))
	    (begin
	      (let ((this-char (hash-ref tmp-htable digit #f)))
		(begin
		  (if (equal? this-char #f)
		      (begin
			(hash-set! tmp-htable digit char))
		      (begin
			(if (not (equal? this-char char))
			    (set! ok-flag #f))
			))
		  ))
	      ))
	  ))

      (if (equal? ok-flag #f)
	  (hash-clear! char-htable))
      )))

;;;#############################################################
;;;#############################################################
(define (digital-substitution slist char-htable)
  (let ((sum 0)
	(ok-flag #t))
    (begin
      (for-each
       (lambda (this-char)
	 (let ((this-digit (hash-ref char-htable this-char #f)))
	   (if (equal? this-digit #f)
	       (begin
		 (set! ok-flag #f))
	       (begin
		 (set! sum (+ (* 10 sum) this-digit))
		 ))
	   )) slist)
      (if (equal? ok-flag #f)
	  (begin
	    #f)
	  (begin
	    sum
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-digital-substitution-1)
  (let ((sub-name "test-digital-substitution-1")
	(test-list
	 (list
	  (list (list #\c #\a #\r #\e) (list 1 2 9 6)
		(list #\c #\a #\r #\e) 1296)
	  (list (list #\c #\a #\r #\e) (list 1 2 9 6)
		(list #\r #\a #\c #\e) 9216)
	  ))
	(char-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! char-htable)

	   (let ((slist (list-ref alist 0))
		 (dlist (list-ref alist 1))
		 (char-list (list-ref alist 2))
		 (shouldbe (list-ref alist 3)))
	     (let ((dlen (length dlist)))
	       (begin
		 (add-to-chars-htable! char-htable dlen dlist slist)

		 (let ((result (digital-substitution char-list char-htable)))
		   (begin
		     (if (not (equal? shouldbe result))
			 (begin
			   (display (format #f "~a : (~a) : error : slist = ~a, dlist = ~a, char-list = ~a, shouldbe = ~a, result = ~a~%"
					    sub-name test-label-index slist dlist
					    char-list shouldbe result))
			   (force-output)
			   (quit)
			   ))
		     )))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (hash-to-string char-htable)
  (let ((sresult ""))
    (begin
      (hash-for-each
       (lambda (key value)
	 (begin
	   (let ((s1 (string key))
		 (s2 (number->string value)))
	     (begin
	       (if (string-ci=? sresult "")
		   (begin
		     (set! sresult (string-append "{ " s1 " -> " s2)))
		   (begin
		     (set! sresult (string-append sresult ", " s1 " -> " s2))
		     ))
	       ))
	   )) char-htable)
      (string-append sresult " }")
      )))

;;;#############################################################
;;;#############################################################
;;; format of squares-list-list is (list (list n1 n1^2) (list n2 n2^2),...)
(define (extract-squares-only squares-list-list)
  (let ((result-list
	 (map
	  (lambda (a-list)
	    (begin
	      (list-ref a-list 1)
	      )) squares-list-list)
	 ))
    result-list
    ))

;;;#############################################################
;;;#############################################################
;;; define macro to simplify the code
(define-syntax check-other-words
  (syntax-rules ()
    ((check-other-words this-number this-word char-htable dlen dlist slist
			words-list only-squares-list complete-squares-list
			max-square-number max-square-list
			max-square-word-list max-string-map)
     (begin
       (let ((slist (string->list this-word)))
	 (begin
	   (add-to-chars-htable! char-htable dlen dlist slist)
	   (for-each
	    (lambda (next-word)
	      (begin
		(if (not (string-ci=? this-word next-word))
		    (begin
		      (let ((ns-list (string->list next-word)))
			(let ((this-sum (digital-substitution ns-list char-htable)))
			  (begin
			    (if (and (not (equal? this-sum #f))
				     (not (equal? (member this-sum only-squares-list) #f)))
				(begin
				  (if (> this-sum max-square-number)
				      (begin
					(set! max-square-number this-sum)
					(set! max-square-word-list (list this-word next-word))
					(set! max-string-map (hash-to-string char-htable))
					(for-each
					 (lambda (b-list)
					   (begin
					     (let ((b-num (list-ref b-list 0))
						   (b-square (list-ref b-list 1)))
					       (begin
						 (if (equal? this-sum b-square)
						     (set! max-square-list (list this-number b-num)))
						 ))
					     )) complete-squares-list)
					))
				  ))
			    )))
		      ))
		)) words-list)
	   ))
       ))
    ))


;;;#############################################################
;;;#############################################################
(define (find-square-anagram nums-list words-list)
  (let ((digit-list (list-ref nums-list 0))
	(squares-list-list (list-ref nums-list 1))
	(char-htable (make-hash-table 100))
	(max-square-number 0)
	(max-square-list (list))
	(max-square-word-list (list))
	(max-string-map ""))
    (let ((dlen (length digit-list))
	  (slen (string-length (car words-list)))
	  (only-squares-list (extract-squares-only squares-list-list)))
      (begin
	(if (= dlen slen)
	    (begin
	      (for-each
	       (lambda (nlist)
		 (begin
		   (let ((this-num (list-ref nlist 0))
			 (this-square (list-ref nlist 1)))
		     (let ((dlist (split-digits-list this-square)))
		       (begin
			 (for-each
			  (lambda (this-word)
			    (begin
			      (check-other-words this-num this-word char-htable dlen dlist slist
						 words-list only-squares-list squares-list-list
						 max-square-number max-square-list
						 max-square-word-list max-string-map)
			      )) words-list)
			 ))
		     ))) squares-list-list)

	      (list max-square-number max-square-list max-square-word-list max-string-map))
	    (begin
	      #f
	      ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-find-square-anagram-1)
  (let ((sub-name "test-find-square-anagram-1")
	(test-list
	 (list
	  (list (list (list 1 2 6 9)
		      (list (list 36 1296)
			    (list 54 2916)
			    (list 96 9216)))
		(list "care" "race")
		(list 9216 (list 36 96) (list "care" "race") "{ r -> 9, a -> 2, c -> 1, e -> 6 }"))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-list (list-ref alist 0))
		 (words-list (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (let ((result (find-square-anagram num-list words-list)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : (~a) : error : num-list = ~a, words-list = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index num-list words-list
					  shouldbe result))
			 (force-output)
			 (quit)
			 ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; define macro to simplify the code
(define-syntax process-nums-list-words-list
  (syntax-rules ()
    ((process-nums-list-words-list nums-list-list words-key words-list dlen
				   max-square-number max-square-number-pair
				   max-square-word-pair max-string-map)
     (begin
       (let ((slen (string-length words-key)))
	 (begin
	   (if (= dlen slen)
	       (begin
		 (let ((result (find-square-anagram nums-list-list words-list)))
		   (if (and (not (equal? result #f)) (list? result))
		       (begin
			 (let ((square-number (list-ref result 0))
			       (square-list (list-ref result 1))
			       (square-word-list (list-ref result 2))
			       (string-map (list-ref result 3)))
			   (begin
			     (if (> square-number max-square-number)
				 (begin
				   (set! max-square-number square-number)
				   (set! max-square-number-pair square-list)
				   (set! max-square-word-pair square-word-list)
				   (set! max-string-map string-map)
				   ))
			     ))
			 )))
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (process-hashes nums-htable words-htable)
  (let ((max-square-number 0)
	(max-square-number-pair (list))
	(max-square-word-pair (list))
	(max-string-map ""))
    (begin
      (hash-for-each
       (lambda (nums-key nums-list)
	 (begin
	   (let ((dlen (length nums-key)))
	     (begin
	       (hash-for-each
		(lambda (words-key words-list)
		  (begin
		    (process-nums-list-words-list
		     (list nums-key nums-list)
		     words-key words-list dlen
		     max-square-number max-square-number-pair
		     max-square-word-pair max-string-map)
		    )) words-htable)

	       ))
	   )) nums-htable)

      (list max-square-number max-square-number-pair max-square-word-pair max-string-map)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop filename start-num end-num)
  (let ((words-htable (make-hash-table 100))
	(multiples-htable (make-hash-table 100)))
    (let ((words-list (read-in-file filename)))
      (begin
	(word-list-to-multiple-string-hash! words-htable words-list)
	(squared-numbers-pairs-hash! multiples-htable start-num end-num)

	(let ((result (process-hashes multiples-htable words-htable)))
	  (begin
	    (if (and (not (equal? result #f)) (list? result))
		(begin
		  (let ((max-square-number (list-ref result 0))
			(max-square-number-pair (list-ref result 1))
			(max-square-word-pair (list-ref result 2))
			(max-string-map (list-ref result 3)))
		    (begin
		      (display (ice9-format:format #f "the largest square number formed is ~:d~%"
						   max-square-number))
		      (display (ice9-format:format #f "used the words ~a, and the numbers ~a~%"
						   max-square-word-pair max-square-number-pair))
		      (display (ice9-format:format #f "map = ~a~%" max-string-map))
		      (let ((dlen (length max-square-word-pair)))
			(begin
			  (do ((ii 0 (1+ ii)))
			      ((>= ii dlen))
			    (begin
			      (let ((this-word (list-ref max-square-word-pair ii))
				    (this-num (list-ref max-square-number-pair ii)))
				(begin
				  (display (ice9-format:format #f "  ~s : ~:d^2 = ~:d~%"
							       this-word this-num (* this-num this-num)))
				  ))
			      ))
			  ))
		      (force-output)
		      ))
		  ))
	    ))
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
    (display (format #f "Project Euler 98 - By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a square number: 1296 = 36^2. What is remarkable is that, by using the same digital substitutions, the anagram, RACE, also forms a square number: 9216 = 96^2. We shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted, neither may a different letter have the same digital value as another letter.~%"))
    (newline)
    (display (format #f "Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).~%"))
    (newline)
    (display (format #f "What is the largest square number formed by any member of such a pair?~%"))
    (newline)
    (display (format #f "NOTE: All anagrams formed must be contained in the given text file.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-digit-list-to-number-1 counter)
	   (run-test test-squared-numbers-pairs-hash-1 counter)
	   (run-test test-word-list-to-multiple-string-hash-1 counter)
	   (run-test test-digital-substitution-1 counter)
	   (run-test test-find-square-anagram-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((filename "words.txt")
	  (words-htable (make-hash-table 100))
	  (multiples-htable (make-hash-table 100))
	  (counter 0))
      (begin
	(time-code
	 (begin
	   (let ((words-list (read-in-file filename)))
	     (begin
	       (word-list-to-multiple-string-hash! multiples-htable words-list)

	       (hash-for-each
		(lambda (key value)
		  (begin
		    (set! counter (1+ counter))
		    (display (format #f "(~a) ~a => ~a~%" counter key value))
		    )) multiples-htable)
	       ))
	   ))
	))

    (newline)
    (force-output)

    (let ((filename "words.txt")
	  (start-num 1)
	  (end-num 96))
      (begin
	(time-code
	 (begin
	   (main-loop filename start-num end-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((filename "words.txt")
	  (start-num 1)
	  (end-num 100000))
      (begin
	(time-code
	 (begin
	   (main-loop filename start-num end-num)
	   ))
	))

    (newline)
    ))
