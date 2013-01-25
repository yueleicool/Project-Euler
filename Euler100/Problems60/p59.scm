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

;;;### ice-9 rdelim - for read-delimited
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
;;; ascii code limits A - 65, Z - 90, a - 97, z - 122
;;; 0 - 48, 9 - 57

;;;#############################################################
;;;#############################################################
(define (populate-frequency-hash! letter-htable char1 ii-th ii-delta integer-list)
  (let ((char-count 0)
	(trial-char-int (char->integer char1))
	(int-len (length integer-list)))
    (begin
      (hash-clear! letter-htable)

      (do ((ii ii-th (+ ii ii-delta)))
	  ((>= ii int-len))
	(begin
	  (let ((this-pwd-int (list-ref integer-list ii)))
	    (let ((translated-char (logxor this-pwd-int trial-char-int)))
	      (begin
		(if (not (equal? translated-char #\space))
		    (begin
		      (let ((key (integer->char translated-char)))
			(let ((hvalue (hash-ref letter-htable key 0)))
			  (begin
			    (hash-set! letter-htable key (1+ hvalue))
			    ))
			)))
		)))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (find-max-count-in-hash letter-htable)
  (let ((max-count 0)
	(result-list (list)))
    (begin
      (hash-for-each
       (lambda (key value)
	 (begin
	   (if (> value max-count)
	       (begin
		 (set! max-count value)
		 (set! result-list (list key value))
		 ))
	   )) letter-htable)

      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-max-count-in-hash-1)
  (let ((sub-name "test-find-max-count-in-hash-1")
	(test-list
	 (list
	  (list (list (list #\a 10) (list #\b 20) (list #\c 25)
		      (list #\d 30) (list #\e 40) (list #\f 50)
		      (list #\space 100))
		(list #\space 100))
	  (list (list (list #\a 10) (list #\b 20) (list #\c 25)
		      (list #\d 30) (list #\e 40) (list #\f 50)
		      (list #\space -1))
		(list #\f 50))
	  ))
	(letter-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (hash-clear! letter-htable)

	   (let ((hash-list (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (begin
	       (for-each
		(lambda (b-list)
		  (begin
		    (let ((key (list-ref b-list 0))
			  (value (list-ref b-list 1)))
		      (begin
			(hash-set! letter-htable key value)
			))
		    )) hash-list)

	       (let ((result (find-max-count-in-hash letter-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : hash list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index hash-list
					  shouldbe result))
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
(define-syntax max-value-from-hash
  (syntax-rules ()
    ((max-value-from-hash letter-htable result-list)
     (begin
       (let ((a-result (find-max-count-in-hash letter-htable)))
	 (let ((key-char (list-ref a-result 0))
	       (key-count (list-ref a-result 1)))
	   (begin
	     (set! result-list (cons a-result result-list))
	     (hash-set! letter-htable key-char -1)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (top-three-letters letter-htable)
  (let ((result-list (list)))
    (begin
      (max-value-from-hash letter-htable result-list)
      (max-value-from-hash letter-htable result-list)
      (max-value-from-hash letter-htable result-list)

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-top-three-letters-1)
  (let ((sub-name "test-top-three-letters-1")
	(test-list
	 (list
	  (list (list (list #\a 10) (list #\b 20) (list #\c 25)
		      (list #\d 30) (list #\e 40) (list #\f 50)
		      (list #\space 100))
		(list (list #\space 100) (list #\f 50) (list #\e 40)))
	  (list (list (list #\a 10) (list #\b 20) (list #\c 25)
		      (list #\d 30) (list #\e 40) (list #\f 50)
		      (list #\space -1))
		(list (list #\f 50) (list #\e 40) (list #\d 30)))
	  ))
	(letter-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (hash-clear! letter-htable)

	   (let ((hash-list (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (begin
	       (for-each
		(lambda (b-list)
		  (begin
		    (let ((key (list-ref b-list 0))
			  (value (list-ref b-list 1)))
		      (begin
			(hash-set! letter-htable key value)
			))
		    )) hash-list)

	       (let ((result (top-three-letters letter-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : hash list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index hash-list
					  shouldbe result))
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
(define (score-results letter-htable total-num top-chars-list-list)
  (let ((score 0.0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((key-char (list-ref a-list 0))
		 (key-pcnt (list-ref a-list 1)))
	     (let ((lcount (hash-ref letter-htable key-char 0.0)))
	       (let ((lscore (abs (- key-pcnt (/ lcount total-num)))))
		 (begin
		   (set! score (+ score lscore))
		   ))
	       ))
	   )) top-chars-list-list)
      score
      )))

;;;#############################################################
;;;#############################################################
(define (find-top-three-scores scores-htable)
  (let ((char-0 #f)
	(min-score-0 -1)
	(char-1 #f)
	(min-score-1 -1)
	(char-2 #f)
	(min-score-2 -1)
	(result-list (list)))
    (begin
      (hash-for-each
       (lambda (key value)
	 (begin
	   (cond
	    ((not (equal? (member 0 key) #f))
	     (begin
	       (if (or (< min-score-0 0)
		       (<= value min-score-0))
		   (begin
		     (set! char-0 (list-ref key 0))
		     (set! min-score-0 value)
		     ))
	       ))
	    ((not (equal? (member 1 key) #f))
	     (begin
	       (if (or (< min-score-1 0)
		       (<= value min-score-1))
		   (begin
		     (set! char-1 (list-ref key 0))
		     (set! min-score-1 value)
		     ))
	       ))
	    ((not (equal? (member 2 key) #f))
	     (begin
	       (if (or (< min-score-2 2)
		       (<= value min-score-2))
		   (begin
		     (set! char-2 (list-ref key 0))
		     (set! min-score-2 value)
		     ))
	       )))
	   )) scores-htable)

      (list char-0 char-1 char-2)
      )))

;;;#############################################################
;;;#############################################################
(define (unencrypt-text coded-text-list three-chars-list)
  (let ((text-len (length coded-text-list))
	(result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii text-len))
	(begin
	  (let ((decode-char (list-ref three-chars-list (modulo ii 3)))
		(encrypt-int (list-ref coded-text-list ii)))
	    (let ((decode-int (char->integer decode-char)))
	      (let ((translated-int (logxor encrypt-int decode-int)))
		(let ((translated-char (integer->char translated-int)))
		  (begin
		    (set! result-list (cons translated-char result-list))
		    ))
		)))
	  ))

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
;;; cipher.txt is a comma separated file
(define (get-ints-from-file filename)
  (let ((int-list (list)))
    (begin
      (if (file-exists? filename)
	  (begin
	    (with-input-from-file filename
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\n\r") (ice9-rdelim:read-delimited ",\n\r")))
		    ((eof-object? line))
		  (begin
		    (if (not (eof-object? line))
			(begin
			  (let ((this-num (string->number line)))
			    (if (not (equal? this-num #f))
				(set! int-list (cons this-num int-list))
				))))
		    ))
		))
	    (reverse int-list)
	    ))
      )))

;;;#############################################################
;;;#############################################################
;;; ascii code limits A - 65, Z - 90, a - 97, z - 122
(define (main-loop filename top-chars-list-list)
  (let ((max-score 0)
	(max-chars-list (list))
	(max-password-list (list))
	(encrypted-list (get-ints-from-file filename))
	(letters-htable (make-hash-table))
	(scores-htable (make-hash-table))
	(min-ascii 97)
	(max-ascii 122))
    (let ((total-num (length encrypted-list)))
      (begin
	(do ((ii min-ascii (+ ii 1)))
	    ((> ii max-ascii))
	  (begin
	    (let ((char-1 (integer->char ii)))
	      (begin
		(hash-clear! letters-htable)
		(populate-frequency-hash! letters-htable char-1 0 3 encrypted-list)
		(let ((this-score (score-results letters-htable total-num
						 top-chars-list-list)))
		  (begin
		    (hash-set! scores-htable (list char-1 0) this-score)
		    ))

		(hash-clear! letters-htable)
		(populate-frequency-hash! letters-htable char-1 1 3 encrypted-list)
		(let ((this-score (score-results letters-htable total-num
						 top-chars-list-list)))
		  (begin
		    (hash-set! scores-htable (list char-1 1) this-score)
		    ))

		(hash-clear! letters-htable)
		(populate-frequency-hash! letters-htable char-1 2 3 encrypted-list)
		(let ((this-score (score-results letters-htable total-num
						 top-chars-list-list)))
		  (begin
		    (hash-set! scores-htable (list char-1 2) this-score)
		    ))
		))
	    ))

	(let ((result-list (find-top-three-scores scores-htable)))
	  (let ((unencrypted-text-list (unencrypt-text encrypted-list result-list)))
	    (begin
	      (newline)
	      (display (format #f "best three-letter pass-phrase found = ~a~%"
			       (list->string result-list)))
	      (newline)
	      (display (format #f "unencrypted text:~%'~a'~%"
			       (list->string unencrypted-text-list)))

	      (newline)
	      (display (format #f "sum of ascii codes = ~a~%"
			       (srfi-1:fold + 0
					    (map char->integer
						 unencrypted-text-list))))
	      (force-output)
	      )))
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
    (display (format #f "Problem 059 - Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.~%"))
    (newline)
    (display (format #f "A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.~%"))
    (newline)
    (display (format #f "For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both 'halves', it is impossible to decrypt the message.~%"))
    (newline)
    (display (format #f "Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.~%"))
    (newline)
    (display (format #f "Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.~%"))
    (newline)
    (display (format #f "The solution method was found from the article at http://www.mathblog.dk/project-euler-59-xor-encryption/~%"))
    (newline)
    (display (format #f "It tries all 26 different letters on the encrypted text, and finds the top letters that produces the most commonly used letters.~%"))
    (newline)
    (display (format #f "see the website http://en.wikipedia.org/wiki/Letter_frequency for the most commonly used letters english.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-find-max-count-in-hash-1 counter)
	   (run-test test-top-three-letters-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((fname "cipher1.txt")
	  (top-four-letters (list (list #\e 0.127) (list #\t 0.091)
				  (list #\a 0.082) (list #\o 0.075)
				  (list #\i 0.069) (list #\n 0675))))
      (begin
	(time-code
	 (begin
	   (main-loop fname top-four-letters)
	   ))
	))

    (newline)
    ))
