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
;;; define card constants
(define two 2)
(define three 3)
(define four 4)
(define five 5)
(define six 6)
(define seven 7)
(define eight 8)
(define nine 9)
(define ten 10)
(define jack 11)
(define queen 12)
(define king 13)
(define ace 14)

(define spades "s")
(define clubs "c")
(define diamonds "d")
(define hearts "h")

(define royal-flush-const 110)
(define straight-flush-const 100)
(define four-of-a-kind-const 90)
(define full-house-const 80)
(define flush-const 70)
(define straight-const 60)
(define three-of-a-kind-const 50)
(define two-pairs-const 40)
(define one-pairs-const 30)
(define high-cards-const 20)

(begin
  (define hand-name-hash (make-hash-table 10))
  (hash-set! hand-name-hash royal-flush-const "royal flush")
  (hash-set! hand-name-hash straight-flush-const "straight flush")
  (hash-set! hand-name-hash four-of-a-kind-const "4-of-a-kind")
  (hash-set! hand-name-hash full-house-const "full-house")
  (hash-set! hand-name-hash flush-const "flush")
  (hash-set! hand-name-hash straight-const "straight")
  (hash-set! hand-name-hash three-of-a-kind-const "3-of-a-kind")
  (hash-set! hand-name-hash two-pairs-const "two-pairs")
  (hash-set! hand-name-hash one-pairs-const "one-pairs")
  (hash-set! hand-name-hash high-cards-const "high-cards"))

(begin
  (define num-name-hash (make-hash-table 20))
  (hash-set! num-name-hash two "2")
  (hash-set! num-name-hash three "3")
  (hash-set! num-name-hash four "4")
  (hash-set! num-name-hash five "5")
  (hash-set! num-name-hash six "6")
  (hash-set! num-name-hash seven "7")
  (hash-set! num-name-hash eight "8")
  (hash-set! num-name-hash nine "9")
  (hash-set! num-name-hash ten "t")
  (hash-set! num-name-hash jack "j")
  (hash-set! num-name-hash queen "q")
  (hash-set! num-name-hash king "k")
  (hash-set! num-name-hash ace "a")

  (define name-num-hash (make-hash-table 20))
  (hash-set! name-num-hash "2" two)
  (hash-set! name-num-hash "3" three)
  (hash-set! name-num-hash "4" four)
  (hash-set! name-num-hash "5" five)
  (hash-set! name-num-hash "6" six)
  (hash-set! name-num-hash "7" seven)
  (hash-set! name-num-hash "8" eight)
  (hash-set! name-num-hash "9" nine)
  (hash-set! name-num-hash "t" ten)
  (hash-set! name-num-hash "j" jack)
  (hash-set! name-num-hash "q" queen)
  (hash-set! name-num-hash "k" king)
  (hash-set! name-num-hash "a" ace))

(begin
  (define suit-name-hash (make-hash-table 10))
  (hash-set! suit-name-hash spades "spades")
  (hash-set! suit-name-hash clubs "clubs")
  (hash-set! suit-name-hash diamonds "diamonds")
  (hash-set! suit-name-hash hearts "hearts"))

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
(define (card-list-identical? c1 c2)
  (and (equal? (list-ref c1 0) (list-ref c2 0))
       (string-ci=? (list-ref c1 1) (list-ref  c2 1))))

;;;#############################################################
;;;#############################################################
(define (card-list-equal? c1 c2)
  (equal? (list-ref c1 0) (list-ref c2 0)))

;;;#############################################################
;;;#############################################################
(define (string-to-card this-card)
  (if (string? this-card)
      (let ((slist (string->list this-card)))
	(if (and (list? slist) (>= (length slist) 2))
	    (begin
	      (let ((value (make-string 1 (char-downcase (list-ref slist 0))))
		    (suit (make-string 1 (char-downcase (list-ref slist 1)))))
		(let ((vcheck (hash-ref name-num-hash value #f))
		      (scheck (hash-ref suit-name-hash suit #f)))
		  (begin
		    (if (or (equal? vcheck #f)
			    (equal? scheck #f))
			#f
			(list vcheck suit)
			)))))
	    #f))
      #f))

;;;#############################################################
;;;#############################################################
(define (test-string-to-card-1)
  (let ((sub-name "test-string-to-card-1")
	(test-list
	 (list
	  (list "ad" (list ace "d")) (list "kc" (list king "c"))
	  (list "qs" (list queen "s")) (list "jh" (list jack "h"))
	  (list "td" (list ten "d")) (list "9c" (list nine "c"))
	  (list "8s" (list eight "s")) (list "7h" (list seven "h"))
	  (list "6d" (list six "d")) (list "5c" (list five "c"))
	  (list "4s" (list four "s")) (list "3h" (list three "h"))
	  (list "2d" (list two "d"))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-string (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (string-to-card test-string)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : string = ~s, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-string
					shouldbe-list result-list))
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (update-value-frequency-hash! freq-htable card-list)
  (begin
    (hash-clear! freq-htable)

    (for-each
     (lambda (this-card)
       (let ((this-num (list-ref this-card 0)))
	 (let ((current-count (hash-ref freq-htable this-num 0)))
	   (hash-set! freq-htable this-num (+ current-count 1)))))
     card-list)
    ))

;;;#############################################################
;;;#############################################################
(define (test-update-value-frequency-hash-1)
  (let ((sub-name "test-update-value-frequency-hash-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list ace "d") (list ace "h") (list ace "s") (list king "h"))
		(list (list ace 4) (list king 1)))
	  (list (list (list ace "c") (list ace "d") (list ace "h") (list king "s") (list king "h"))
		(list (list ace 3) (list king 2)))
	  ))
	(freq-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! freq-htable)
	   (let ((card-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (begin
	       (update-value-frequency-hash! freq-htable card-list)

	       (for-each
		(lambda (this-pair)
		  (begin
		    (let ((shouldbe-value (list-ref this-pair 0))
			  (shouldbe-count (list-ref this-pair 1)))
		      (let ((result-count (hash-ref freq-htable shouldbe-value 0)))
			(if (not (equal? shouldbe-count result-count))
			    (begin
			      (display (format #f "~a : error (~a) : card-list = ~a, value = ~a, shouldbe = ~a, result = ~a~%"
					       sub-name test-label-index card-list
					       shouldbe-value shouldbe-count result-count))
			      (quit)
			      ))
			))
		    ))
		shouldbe-list)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (update-suit-frequency-hash! freq-htable card-list)
  (begin
    (hash-clear! freq-htable)

    (for-each
     (lambda (this-card)
       (let ((this-suit (list-ref this-card 1)))
	 (let ((current-count (hash-ref freq-htable this-suit 0)))
	   (hash-set! freq-htable this-suit (+ current-count 1)))))
     card-list)
    ))

;;;#############################################################
;;;#############################################################
(define (test-update-suit-frequency-hash-1)
  (let ((sub-name "test-update-suit-frequency-hash-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c") (list jack "c") (list ten "c"))
		(list (list "c" 5)))
	  (list (list (list ace "c") (list ace "d") (list ace "h") (list king "s") (list king "h"))
		(list (list "c" 1) (list "d" 1) (list "h" 2) (list "s" 1)))
	  ))
	(freq-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! freq-htable)
	   (let ((card-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (begin
	       (update-suit-frequency-hash! freq-htable card-list)

	       (for-each
		(lambda (this-pair)
		  (begin
		    (let ((shouldbe-value (list-ref this-pair 0))
			  (shouldbe-count (list-ref this-pair 1)))
		      (let ((result-count (hash-ref freq-htable shouldbe-value 0)))
			(begin
			  (if (not (equal? shouldbe-count result-count))
			      (begin
				(display (format #f "~a : error (~a) : card-list = ~a, value = ~a, shouldbe = ~a, result = ~a~%"
						 sub-name test-label-index card-list
						 shouldbe-value shouldbe-count result-count))
				(quit)
				))
			  )))
		    ))
		shouldbe-list)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (get-nth-of-a-kind-num freq-htable value-to-find)
  (let ((result #f))
    (begin
      (hash-for-each
       (lambda(key value)
	 (if (equal? value-to-find value)
	     (if (or (equal? result #f)
		     (> key result))
		 (set! result key))))
       freq-htable)
      result)
    ))

;;;#############################################################
;;;#############################################################
(define (test-get-nth-of-a-kind-num-1)
  (let ((sub-name "test-get-nth-of-a-kind-num-1")
	(test-list
	 (list
	  (list (list (list 4 4) (list 14 4)) 4 14)
	  (list (list (list 14 4) (list 4 4)) 4 14)
	  (list (list (list 14 3) (list 4 4)) 4 4)
	  (list (list (list 14 3) (list 4 4)) 3 14)
	  ))
	(freq-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! freq-htable)
	   (let ((test-list (list-ref alist 0))
		 (test-nn (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (begin
	       (for-each
		(lambda (tlist)
		  (let ((key (list-ref tlist 0))
			(value (list-ref tlist 1)))
		    (begin
		      (hash-set! freq-htable key value)
		      )))
		test-list)

	       (let ((result-num (get-nth-of-a-kind-num freq-htable test-nn)))
		 (begin
		   (if (not (equal? shouldbe-num result-num))
		       (begin
			 (display (format #f "~a : error (~a) : hash = ~a, number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-list test-nn
					  shouldbe-num result-num))
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
(define (get-second-nth-of-a-kind-num freq-htable value-to-find)
  (let ((counter 0)
	(vlist (list)))
    (begin
      (hash-for-each
       (lambda(key value)
	 (if (equal? value-to-find value)
	     (begin
	       (set! counter (1+ counter))
	       (set! vlist (cons key vlist))
	       )))
       freq-htable)

      (let ((vlen (length vlist)))
	(if (> vlen 1)
	    (let ((vsorted (sort vlist >)))
	      (let ((result (list-ref vsorted 1)))
		result
		))
	    (begin
	      #f)))
      )))

;;;#############################################################
;;;#############################################################
(define (test-get-second-nth-of-a-kind-num-1)
  (let ((sub-name "test-get-second-nth-of-a-kind-num-1")
	(test-list
	 (list
	  (list (list (list 4 4) (list 14 4)) 4 4)
	  (list (list (list 14 4) (list 4 4)) 4 4)
	  (list (list (list 14 3) (list 4 4) (list 13 3)) 3 13)
	  (list (list (list 14 3) (list 4 4)) 4 #f)
	  ))
	(freq-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (hash-clear! freq-htable)
	   (let ((test-list (list-ref alist 0))
		 (test-nn (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (begin
	       (for-each
		(lambda (tlist)
		  (begin
		    (let ((key (list-ref tlist 0))
			  (value (list-ref tlist 1)))
		      (begin
			(hash-set! freq-htable key value)
			))
		    ))
		test-list)

	       (let ((result-num (get-second-nth-of-a-kind-num freq-htable test-nn)))
		 (begin
		   (if (not (equal? shouldbe-num result-num))
		       (begin
			 (display (format #f "~a : error (~a) : hash = ~a, number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-list test-nn
					  shouldbe-num result-num))
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
;;; returns sorted list of cards in descending order
(define (extract-values-from-card-list card-list)
  (let ((this-seq-list
	 (sort
	  (map
	   (lambda(this-card)
	     (list-ref this-card 0))
	   card-list) >)))
    this-seq-list
    ))

;;;#############################################################
;;;#############################################################
(define (straight? card-list)
  (begin
    (let ((this-seq-list (extract-values-from-card-list card-list)))
      (begin
	(cond
	 ((equal? this-seq-list (list ace king queen jack ten)) #t)
	 ((equal? this-seq-list (list king queen jack ten nine)) #t)
	 ((equal? this-seq-list (list queen jack ten nine eight)) #t)
	 ((equal? this-seq-list (list jack ten nine eight seven)) #t)
	 ((equal? this-seq-list (list ten nine eight seven six)) #t)
	 ((equal? this-seq-list (list nine eight seven six five)) #t)
	 ((equal? this-seq-list (list eight seven six five four)) #t)
	 ((equal? this-seq-list (list seven six five four three)) #t)
	 ((equal? this-seq-list (list six five four three two)) #t)
	 ((equal? this-seq-list (list ace five four three two)) #t)
	 (else
	  #f
	  ))))))

;;;#############################################################
;;;#############################################################
(define (test-straight-1)
  (let ((sub-name "test-straight-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c")) #t)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "d")) #t)
	  (list (list (list queen "c") (list jack "c") (list ten "c")
		      (list nine "d") (list eight "h")) #t)
	  (list (list (list jack "c") (list ten "c") (list nine "d")
		      (list eight "h") (list seven "d")) #t)
	  (list (list (list ten "c") (list nine "d") (list eight "h")
		      (list seven "d") (list six "s")) #t)
	  (list (list (list nine "d") (list eight "h") (list seven "d")
		      (list six "s") (list five "c")) #t)
	  (list (list (list eight "h") (list seven "d") (list six "s")
		      (list five "c") (list four "h")) #t)
	  (list (list (list seven "d") (list six "s") (list five "c")
		      (list four "h") (list three "s")) #t)
	  (list (list (list six "s") (list five "c") (list four "h")
		      (list three "s") (list two "c")) #t)
	  (list (list (list five "c") (list four "h") (list three "s")
		      (list two "c") (list ace "s")) #t)
	  (list (list (list six "c") (list four "h") (list three "s")
		      (list two "c") (list ace "s")) #f)
	  (list (list (list king "c") (list queen "h") (list jack "s")
		      (list two "c") (list ace "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (straight? card-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : card list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index card-list
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
;;; returns high card value where there is just 1 card in the hand
;;; assumes vlist sorted in descending order
(define (get-high-card-value card-list freq-value-htable)
  (let ((vlist (extract-values-from-card-list card-list))
	(found-flag #f)
	(result-value #f))
    (let ((vlen (length vlist)))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((or (>= ii vlen)
		 (equal? found-flag #t)))
	  (begin
	    (let ((this-value (list-ref vlist ii)))
	      (let ((ncount (hash-ref freq-value-htable this-value 0)))
		(begin
		  (if (= ncount 1)
		      (begin
			(set! result-value this-value)
			(set! found-flag #t)
			)))))
	    ))

	(if (and (straight? card-list)
		 (equal? result-value ace)
		 (equal? (list-ref vlist 1) five))
	    (begin
	      ;;; then we have an ace to five straight
	      five)
	    (begin
	      result-value
	      ))
	))))

;;;#############################################################
;;;#############################################################
(define (test-get-high-card-value-1)
  (let ((sub-name "test-get-high-card-value-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c")) ace)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "d")) king)
	  (list (list (list queen "c") (list jack "c") (list ten "c")
		      (list nine "d") (list eight "h")) queen)
	  (list (list (list jack "c") (list ten "c") (list nine "d")
		      (list eight "h") (list seven "d")) jack)
	  (list (list (list ten "c") (list nine "d") (list eight "h")
		      (list seven "d") (list six "s")) ten)
	  (list (list (list nine "d") (list eight "h") (list seven "d")
		      (list six "s") (list five "c")) nine)
	  (list (list (list eight "h") (list seven "d") (list six "s")
		      (list five "c") (list four "h")) eight)
	  (list (list (list seven "d") (list six "s") (list five "c")
		      (list four "h") (list three "s")) seven)
	  (list (list (list six "s") (list five "c") (list four "h")
		      (list three "s") (list two "c")) six)
	  (list (list (list five "c") (list four "h") (list three "s")
		      (list two "c") (list ace "s")) five)
	  (list (list (list six "c") (list four "h") (list three "s")
		      (list two "c") (list ace "s")) ace)
	  (list (list (list king "c") (list queen "h") (list jack "s")
		      (list two "c") (list three "s")) king)
	  (list (list (list king "c") (list queen "h") (list queen "s")
		      (list two "c") (list king "s")) two)
	  ))
	(freq-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (begin
	       (update-value-frequency-hash! freq-htable card-list)
	       (let ((result (get-high-card-value card-list freq-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : card list = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index card-list
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
(define (calc-hand-value card-list)
  (let ((freq-value-htable (make-hash-table 10))
	(freq-suit-htable (make-hash-table 10))
	(flush-suit #f))
    (begin
      (update-value-frequency-hash! freq-value-htable card-list)
      (update-suit-frequency-hash! freq-suit-htable card-list)

      (let ((fsuit (get-nth-of-a-kind-num freq-suit-htable 5)))
	(set! flush-suit fsuit))

      (cond
       ((not (equal? flush-suit #f))  ;;; check for straight flush
	(begin
	  (let ((high-card (get-high-card-value card-list freq-value-htable)))
	    (if (equal? (straight? card-list) #t)
		(begin
		  (if (equal? high-card ace)
		      royal-flush-const
		      straight-flush-const
		      ))
		(begin
		  flush-const
		  ))
	    )))
       ((straight? card-list) straight-const)
       (else
	(begin
	;;; look for pairs, two-pairs, triples, quads, and high-card hands
	  (let ((four-value (get-nth-of-a-kind-num freq-value-htable 4))
		(three-value (get-nth-of-a-kind-num freq-value-htable 3))
		(two-value (get-nth-of-a-kind-num freq-value-htable 2))
		(one-value (get-high-card-value card-list freq-value-htable)))
	    (begin
	      (cond
	       ((not (equal? four-value #f)) four-of-a-kind-const)
	       ((not (equal? three-value #f))
		(begin
		  (if (equal? two-value #f)
		      three-of-a-kind-const
		      full-house-const
		      )))
	       ((not (equal? two-value #f))
		(begin
		  (let ((second-two-value (get-second-nth-of-a-kind-num freq-value-htable 2)))
		    (if (equal? second-two-value #f)
			one-pairs-const
			two-pairs-const
			))))
	       (else
	      ;;; high card
		high-cards-const
		))
	      ))
	  )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-hand-value-1)
  (let ((sub-name "test-calc-hand-value-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c")) royal-flush-const)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "c")) straight-flush-const)
	  (list (list (list queen "c") (list queen "d") (list ten "c")
		      (list queen "h") (list queen "s")) four-of-a-kind-const)
	  (list (list (list jack "c") (list jack "d") (list seven "h")
		      (list jack "h") (list seven "d")) full-house-const)
	  (list (list (list ten "c") (list nine "c") (list eight "c")
		      (list seven "c") (list four "c")) flush-const)
	  (list (list (list nine "d") (list eight "h") (list nine "c")
		      (list six "s") (list nine "s")) three-of-a-kind-const)
	  (list (list (list eight "h") (list seven "d") (list six "s")
		      (list six "c") (list eight "d")) two-pairs-const)
	  (list (list (list seven "d") (list six "s") (list five "c")
		      (list seven "h") (list three "s")) one-pairs-const)
	  (list (list (list six "s") (list eight "c") (list four "h")
		      (list three "s") (list two "c")) high-cards-const)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (begin
	       (let ((result (calc-hand-value card-list)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : card list = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index card-list
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
(define (generic-test-hand1-greater-hand2?
	 sub-name card-list-1 card-list-2
	 shouldbe-hvalue1 shouldbe-hvalue2
	 test-greater-function? test-label-index shouldbe)
  (begin
    (let ((result-flag #t))
      (begin
	(let ((hand-value-1 (calc-hand-value card-list-1))
	      (hand-value-2 (calc-hand-value card-list-2)))
	  (begin
            ;;; first make sure that both hands are what they shouldbe
	    (if (or
		 (not (equal? hand-value-1 shouldbe-hvalue1))
		 (not (equal? hand-value-2 shouldbe-hvalue2)))
		(begin
		  (display (format #f "~a : error (~a) : card list 1 = ~a, card list 2 = ~a, hand 1 value = ~a, shouldbe = ~a, hand value 2 = ~a, shouldbe = ~a~%"
				   sub-name test-label-index card-list-1 card-list-2
				   hand-value-1 shouldbe-hvalue1
				   hand-value-2 shouldbe-hvalue2))
		  (quit)
		  ))
            ;;; if we pass the tests above, then the two hands are straight flushes
	    (let ((result
		   (test-greater-function? card-list-1 card-list-2)))
	      (begin
		(if (not (equal? shouldbe result))
		    (begin
		      (display (format #f "~a : error (~a) : card list 1 = ~a, card-list-2 = ~a, shouldbe = ~a, result = ~a~%"
				       sub-name test-label-index card-list-1 card-list-2 shouldbe result))
		      (quit)
		      ))))
	    ))
	))))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two straight hands
(define (straight1-greater-straight2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((hc1 (get-high-card-value card-list-1 freq-value-1-htable))
	    (hc2 (get-high-card-value card-list-2 freq-value-2-htable)))
	(begin
	  (> hc1 hc2)
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-straight1-greater-straight2-1)
  (let ((sub-name "test-straight1-greater-straight2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const #f)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "c"))
		straight-flush-const
		(list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const #f)
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list queen "d") (list jack "d") (list ten "d")
		      (list nine "d") (list eight "d"))
		straight-flush-const #t)
	  (list (list (list king "c") (list queen "c") (list nine "c")
		      (list jack "c") (list ten "c"))
		straight-flush-const
		(list (list queen "d") (list jack "d") (list ten "d")
		      (list nine "d") (list eight "d"))
		straight-flush-const #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (hvalue-1 (list-ref alist 1))
		 (card-list-2 (list-ref alist 2))
		 (hvalue-2 (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		hvalue-1 hvalue-2
		straight1-greater-straight2? test-label-index shouldbe)

	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two four-of-a-kind hands
(define (quads1-greater-quads2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((qc1 (get-nth-of-a-kind-num freq-value-1-htable 4))
	    (qc2 (get-nth-of-a-kind-num freq-value-2-htable 4)))
	(begin
	  (cond
	   ((> qc1 qc2) #t)
	   ((< qc1 qc2) #f)
	   (else
	    (begin
	      (let ((hc1 (get-high-card-value card-list-1 freq-value-1-htable))
		    (hc2 (get-high-card-value card-list-2 freq-value-2-htable)))
		(> hc1 hc2))))
	   ))))
    ))

;;;#############################################################
;;;#############################################################
(define (test-quads1-greater-quads2-1)
  (let ((sub-name "test-quads1-greater-quads2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list ace "d")
		      (list ace "h") (list ace "s"))
		(list (list ace "c") (list queen "c") (list ace "d")
		      (list ace "h") (list ace "s")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list king "s") (list king "h"))
		(list (list queen "c") (list king "c") (list queen "d")
		      (list queen "h") (list queen "s")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list king "s") (list king "h"))
		(list (list king "c") (list queen "c") (list king "d")
		      (list king "s") (list king "h")) #f)
	  (list (list (list king "c") (list jack "c") (list king "d")
		      (list king "s") (list king "h"))
		(list (list king "c") (list queen "c") (list king "d")
		      (list king "s") (list king "h")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		four-of-a-kind-const four-of-a-kind-const
		quads1-greater-quads2? test-label-index shouldbe)

	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two full-house hands
(define (fulls1-greater-fulls2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((tc1 (get-nth-of-a-kind-num freq-value-1-htable 3))
	    (tc2 (get-nth-of-a-kind-num freq-value-2-htable 3)))
	(begin
	  (cond
	   ((> tc1 tc2) #t)
	   ((< tc1 tc2) #f)
	   (else
	    (begin
	      (let ((pc1 (get-nth-of-a-kind-num freq-value-1-htable 2))
		    (pc2 (get-nth-of-a-kind-num freq-value-2-htable 2)))
		(begin
		  (if (> pc1 pc2)
		      (begin
			#t)
		      (begin
			(let ((hc1 (get-high-card-value card-list-1 freq-value-1-htable))
			      (hc2 (get-high-card-value card-list-2 freq-value-2-htable)))
			  (> hc1 hc2))
			))
		  ))
	      ))
	   ))))
    ))

;;;#############################################################
;;;#############################################################
(define (test-fulls1-greater-fulls2-1)
  (let ((sub-name "test-fulls1-greater-fulls2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list ace "d")
		      (list ace "h") (list king "s"))
		(list (list ace "c") (list queen "c") (list ace "d")
		      (list ace "h") (list queen "s")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list king "s") (list queen "h"))
		(list (list queen "c") (list four "c") (list queen "d")
		      (list queen "h") (list four "s")) #t)
	  (list (list (list king "c") (list three "c") (list king "d")
		      (list king "s") (list three "h"))
		(list (list queen "d") (list queen "c") (list king "d")
		      (list queen "s") (list king "h")) #t)
	  (list (list (list king "c") (list jack "c") (list king "d")
		      (list jack "s") (list jack "h"))
		(list (list king "c") (list two "c") (list king "d")
		      (list king "s") (list two "h")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		full-house-const full-house-const
		fulls1-greater-fulls2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two flushes
(define (flushs1-greater-flushs2? card-list-1 card-list-2)
  (let ((clist1 (extract-values-from-card-list card-list-1))
	(clist2 (extract-values-from-card-list card-list-2)))
    (let ((clen1 (length clist1))
	  (clen2 (length clist2))
	  (break-flag #f)
	  (result-flag #f))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((or (> ii clen1)
		 (equal? break-flag #t)))
	  (begin
	    (let ((tc1 (list-ref clist1 ii))
		  (tc2 (list-ref clist2 ii)))
	      (begin
		(cond
		 ((> tc1 tc2)
		  (begin
		    (set! result-flag #t)
		    (set! break-flag #t)))
		 ((< tc1 tc2)
		  (begin
		    (set! result-flag #f)
		    (set! break-flag #t)))
		 )))
	    ))
	result-flag
	))))

;;;#############################################################
;;;#############################################################
(define (test-flushs1-greater-flushs2-1)
  (let ((sub-name "test-flushs1-greater-flushs2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list jack "c")
		      (list five "c") (list two "c"))
		(list (list king "c") (list queen "c") (list jack "c")
		      (list five "c") (list two "c")) #t)
	  (list (list (list king "c") (list queen "c") (list ten "c")
		      (list three "c") (list two "c"))
		(list (list king "c") (list jack "c") (list ten "c")
		      (list three "c") (list two "c")) #t)
	  (list (list (list king "d") (list three "d") (list five "d")
		      (list two "d") (list nine "d"))
		(list (list queen "s") (list jack "s") (list king "s")
		      (list two "s") (list seven "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		flush-const flush-const
		flushs1-greater-flushs2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two three-of-a-kind hands
(define (triples1-greater-triples2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((tc1 (get-nth-of-a-kind-num freq-value-1-htable 3))
	    (tc2 (get-nth-of-a-kind-num freq-value-2-htable 3)))
	(begin
	  (cond
	   ((> tc1 tc2) #t)
	   ((< tc1 tc2) #f)
	   (else
	    (begin
	      (let ((elist1 (extract-values-from-card-list card-list-1))
		    (elist2 (extract-values-from-card-list card-list-2)))
		(let ((rest1 (sort (srfi-1:delete tc1 elist1) >))
		      (rest2 (sort (srfi-1:delete tc1 elist2) >)))
		  (let ((len1 (length rest1))
			(len2 (length rest2))
			(break-flag #f)
			(result-flag #f))
		    (begin
		      (do ((ii 0 (+ ii 1)))
			  ((or (>= ii len1)
			       (equal? break-flag #t)))
			(begin
			  (let ((hc1 (list-ref rest1 ii))
				(hc2 (list-ref rest2 ii)))
			    (begin
			      (cond
			       ((> hc1 hc2)
				(begin
				  (set! result-flag #t)
				  (set! break-flag #t)))
			       ((< hc1 hc2)
				(begin
				  (set! result-flag #f)
				  (set! break-flag #t)))
			       )))
			  ))
		      result-flag
		      ))
		  ))
	      ))
	   ))))
    ))

;;;#############################################################
;;;#############################################################
(define (test-triples1-greater-triples2-1)
  (let ((sub-name "test-triples1-greater-triples2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list ace "d")
		      (list five "c") (list ace "s"))
		(list (list ace "c") (list queen "c") (list queen "d")
		      (list queen "h") (list two "c")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list king "h") (list two "c"))
		(list (list king "c") (list jack "c") (list king "d")
		      (list king "h") (list two "c")) #t)
	  (list (list (list king "c") (list jack "c") (list king "d")
		      (list king "h") (list three "c"))
		(list (list king "c") (list jack "c") (list king "d")
		      (list king "h") (list two "c")) #t)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list king "s") (list nine "d"))
		(list (list king "s") (list three "s") (list king "h")
		      (list king "c") (list nine "s")) #f)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list king "s") (list nine "d"))
		(list (list king "s") (list seven "s") (list king "h")
		      (list king "c") (list nine "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		three-of-a-kind-const three-of-a-kind-const
		triples1-greater-triples2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two two-pairs hands
(define (twopairs1-greater-twopairs2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((pc1 (get-nth-of-a-kind-num freq-value-1-htable 2))
	    (pc2 (get-nth-of-a-kind-num freq-value-2-htable 2)))
	(begin
	  (cond
	   ((> pc1 pc2) #t)
	   ((< pc1 pc2) #f)
	   (else
	    (begin
	      (let ((spc1 (get-second-nth-of-a-kind-num freq-value-1-htable 2))
		    (spc2 (get-second-nth-of-a-kind-num freq-value-2-htable 2)))
		(begin
		  (cond
		   ((> spc1 spc2) #t)
		   ((< spc1 spc2) #f)
		   (else
		    (begin
		      (let ((hc1 (get-high-card-value card-list-1 freq-value-1-htable))
			    (hc2 (get-high-card-value card-list-2 freq-value-2-htable)))
			(begin
			  (> hc1 hc2)
			  ))
		      ))
		   ))
		))))
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-twopairs1-greater-twopairs2-1)
  (let ((sub-name "test-twopairs1-greater-twopairs2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list ace "d")
		      (list king "h") (list five "s"))
		(list (list king "c") (list queen "c") (list queen "d")
		      (list king "h") (list two "c")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list queen "h") (list two "c"))
		(list (list king "c") (list jack "c") (list king "d")
		      (list jack "h") (list seven "c")) #t)
	  (list (list (list king "c") (list jack "c") (list king "d")
		      (list jack "h") (list three "c"))
		(list (list king "c") (list jack "c") (list king "d")
		      (list jack "h") (list two "c")) #t)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list nine "s") (list nine "d"))
		(list (list king "s") (list three "s") (list king "h")
		      (list nine "c") (list nine "h")) #f)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list three "s") (list nine "d"))
		(list (list king "s") (list seven "s") (list king "h")
		      (list seven "c") (list nine "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		two-pairs-const two-pairs-const
		twopairs1-greater-twopairs2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two one-pairs hands
(define (onepairs1-greater-onepairs2? card-list-1 card-list-2)
  (let ((freq-value-1-htable (make-hash-table 10))
	(freq-value-2-htable (make-hash-table 10)))
    (begin
      (update-value-frequency-hash! freq-value-1-htable card-list-1)
      (update-value-frequency-hash! freq-value-2-htable card-list-2)

      (let ((pc1 (get-nth-of-a-kind-num freq-value-1-htable 2))
	    (pc2 (get-nth-of-a-kind-num freq-value-2-htable 2)))
	(begin
	  (cond
	   ((> pc1 pc2) #t)
	   ((< pc1 pc2) #f)
	   (else
	    (begin
	      (let ((elist1 (extract-values-from-card-list card-list-1))
		    (elist2 (extract-values-from-card-list card-list-2)))
		(let ((rest1 (sort (srfi-1:delete pc1 elist1) >))
		      (rest2 (sort (srfi-1:delete pc1 elist2) >)))
		  (let ((len1 (length rest1))
			(len2 (length rest2))
			(break-flag #f)
			(result-flag #f))
		    (begin
		      (do ((ii 0 (+ ii 1)))
			  ((or (>= ii len1)
			       (equal? break-flag #t)))
			(begin
			  (let ((hc1 (list-ref rest1 ii))
				(hc2 (list-ref rest2 ii)))
			    (begin
			      (cond
			       ((> hc1 hc2)
				(begin
				  (set! result-flag #t)
				  (set! break-flag #t)))
			       ((< hc1 hc2)
				(begin
				  (set! result-flag #f)
				  (set! break-flag #t)))
			       )))
			  ))
		      result-flag
		      ))
		  ))
	      ))
	   ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-onepairs1-greater-onepairs2-1)
  (let ((sub-name "test-onepairs1-greater-onepairs2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list ace "d")
		      (list queen "h") (list five "s"))
		(list (list king "c") (list queen "c") (list ace "d")
		      (list king "h") (list two "c")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list ten "h") (list two "c"))
		(list (list king "c") (list jack "c") (list king "d")
		      (list ten "h") (list seven "c")) #t)
	  (list (list (list king "c") (list queen "c") (list king "d")
		      (list jack "h") (list three "c"))
		(list (list king "c") (list ten "c") (list king "d")
		      (list jack "h") (list three "c")) #t)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list nine "s") (list eight "d"))
		(list (list king "s") (list two "s") (list king "h")
		      (list nine "c") (list eight "h")) #t)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list ten "s") (list nine "d"))
		(list (list king "s") (list three "s") (list king "h")
		      (list ten "c") (list nine "s")) #f)
	  (list (list (list king "d") (list three "d") (list king "h")
		      (list ten "s") (list nine "d"))
		(list (list king "s") (list four "s") (list king "h")
		      (list ten "c") (list nine "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		one-pairs-const one-pairs-const
		onepairs1-greater-onepairs2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; assumes that we have two high card hands
(define (highcards1-greater-highcards2? card-list-1 card-list-2)
  (let ((elist1 (extract-values-from-card-list card-list-1))
	(elist2 (extract-values-from-card-list card-list-2)))
    (let ((len1 (length elist1))
	  (len2 (length elist2))
	  (break-flag #f)
	  (result-flag #f))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((or (>= ii len1)
		 (equal? break-flag #t)))
	  (begin
	    (let ((hc1 (list-ref elist1 ii))
		  (hc2 (list-ref elist2 ii)))
	      (begin
		(cond
		 ((> hc1 hc2)
		  (begin
		    (set! result-flag #t)
		    (set! break-flag #t)))
		 ((< hc1 hc2)
		  (begin
		    (set! result-flag #f)
		    (set! break-flag #t)))
		 )))
	    ))
	result-flag
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-highcards1-greater-highcards2-1)
  (let ((sub-name "test-highcards1-greater-highcards2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list jack "d")
		      (list nine "h") (list seven "s"))
		(list (list king "c") (list jack "c") (list nine "d")
		      (list seven "h") (list six "c")) #t)
	  (list (list (list king "c") (list queen "c") (list nine "d")
		      (list ten "h") (list three "c"))
		(list (list king "c") (list jack "c") (list nine "d")
		      (list ten "h") (list seven "c")) #t)
	  (list (list (list king "c") (list queen "c") (list nine "d")
		      (list jack "h") (list seven "c"))
		(list (list king "c") (list queen "c") (list nine "d")
		      (list ten "h") (list eight "c")) #t)
	  (list (list (list king "d") (list queen "d") (list nine "h")
		      (list jack "s") (list eight "d"))
		(list (list king "s") (list queen "s") (list eight "h")
		      (list jack "c") (list seven "h")) #t)
	  (list (list (list king "d") (list queen "d") (list nine "h")
		      (list ten "s") (list seven "d"))
		(list (list king "s") (list queen "s") (list nine "h")
		      (list ten "c") (list six "s")) #t)
	  (list (list (list king "d") (list queen "d") (list nine "h")
		      (list ten "s") (list seven "d"))
		(list (list king "s") (list queen "s") (list nine "h")
		      (list ten "c") (list seven "s")) #f)
	  (list (list (list king "d") (list queen "d") (list nine "h")
		      (list ten "s") (list seven "d"))
		(list (list king "s") (list queen "s") (list nine "h")
		      (list ten "c") (list eight "s")) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (card-list-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		high-cards-const high-cards-const
		highcards1-greater-highcards2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################


;;;#############################################################
;;;#############################################################
(define (hand1-greater-hand2? card-list-1 card-list-2)
  (let ((hvalue1 (calc-hand-value card-list-1))
	(hvalue2 (calc-hand-value card-list-2)))
    (begin
      (cond
       ((> hvalue1 hvalue2) #t)
       ((< hvalue1 hvalue2) #f)
       (else
	(begin
          ;;; same hand type
	  (cond
	   ((equal? hvalue1 royal-flush-const) #f)
	   ((equal? hvalue1 straight-flush-const)
	    (straight1-greater-straight2? card-list-1 card-list-2))
	   ((equal? hvalue1 four-of-a-kind-const)
	    (quads1-greater-quads2? card-list-1 card-list-2))
	   ((equal? hvalue1 full-house-const)
	    (fulls1-greater-fulls2? card-list-1 card-list-2))
	   ((equal? hvalue1 flush-const)
	    (flush1-greater-flush2? card-list-1 card-list-2))
	   ((equal? hvalue1 straight-const)
	    (straight1-greater-straight2? card-list-1 card-list-2))
	   ((equal? hvalue1 three-of-a-kind-const)
	    (triples1-greater-triples2? card-list-1 card-list-2))
	   ((equal? hvalue1 two-pairs-const)
	    (twopairs1-greater-twopairs2? card-list-1 card-list-2))
	   ((equal? hvalue1 one-pairs-const)
	    (onepairs1-greater-onepairs2? card-list-1 card-list-2))
	   (else
	    (begin
	      (highcards1-greater-highcards2? card-list-1 card-list-2)))
	   ))))
      )))

;;;#############################################################
;;;#############################################################
(define (test-hand1-greater-hand2-1)
  (let ((sub-name "test-hand1-greater-hand2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const #f)
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list queen "d") (list jack "d") (list ten "d")
		      (list nine "d") (list eight "d"))
		straight-flush-const #t)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "c"))
		straight-flush-const
		(list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const #f)
	  (list (list (list nine "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		straight-flush-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "d"))
		four-of-a-kind-const #t)
	  (list (list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "d"))
		four-of-a-kind-const
		(list (list nine "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		straight-flush-const #f)
	  (list (list (list ace "c") (list ace "d") (list ace "h")
		      (list ace "s") (list ten "c"))
		four-of-a-kind-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list eight "s") (list eight "d"))
		full-house-const #t)
	  (list (list (list queen "d") (list queen "c") (list queen "h")
		      (list eight "s") (list eight "d"))
		full-house-const
		(list (list ace "c") (list ace "d") (list ace "h")
		      (list ace "c") (list ten "c"))
		four-of-a-kind-const #f)
	  (list (list (list queen "d") (list queen "c") (list queen "h")
		      (list eight "s") (list eight "d"))
		full-house-const
		(list (list five "c") (list king "c") (list seven "c")
		      (list jack "c") (list ten "c"))
		flush-const #t)
	  (list (list (list five "c") (list king "c") (list seven "c")
		      (list jack "c") (list ten "c"))
		flush-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list eight "s") (list eight "d"))
		full-house-const #f)
	  (list (list (list five "c") (list king "c") (list seven "c")
		      (list jack "c") (list ten "c"))
		flush-const
		(list (list queen "d") (list jack "c") (list eight "h")
		      (list nine "s") (list ten "d"))
		straight-const #t)
	  (list (list (list queen "d") (list jack "c") (list eight "h")
		      (list nine "s") (list ten "d"))
		straight-const
		(list (list five "c") (list king "c") (list seven "c")
		      (list jack "c") (list ten "c"))
		flush-const #f)
	  (list (list (list queen "h") (list eight "s") (list ten "d")
		      (list jack "c") (list nine "d"))
		straight-const
		(list (list king "c") (list queen "c") (list nine "c")
		      (list king "h") (list king "d"))
		three-of-a-kind-const #t)
	  (list (list (list king "c") (list queen "c") (list nine "c")
		      (list king "h") (list king "d"))
		three-of-a-kind-const
		(list (list queen "h") (list eight "s") (list ten "d")
		      (list jack "c") (list nine "d"))
		straight-const #f)
	  (list (list (list king "c") (list queen "c") (list nine "c")
		      (list king "h") (list king "d"))
		three-of-a-kind-const
		(list (list queen "h") (list ten "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		two-pairs-const #t)
	  (list (list (list queen "h") (list ten "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		two-pairs-const
		(list (list king "c") (list queen "c") (list nine "c")
		      (list king "h") (list king "d"))
		three-of-a-kind-const #f)
	  (list (list (list queen "h") (list ten "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		two-pairs-const
		(list (list king "c") (list queen "c") (list nine "c")
		      (list five "h") (list king "d"))
		one-pairs-const #t)
	  (list (list (list queen "h") (list two "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		one-pairs-const
		(list (list five "c") (list queen "c") (list nine "c")
		      (list five "h") (list nine "d"))
		two-pairs-const #f)
	  (list (list (list queen "h") (list two "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		one-pairs-const
		(list (list five "c") (list queen "c") (list jack "c")
		      (list four "h") (list nine "d"))
		high-cards-const #t)
	  (list (list (list five "c") (list queen "c") (list jack "c")
		      (list four "h") (list nine "d"))
		high-cards-const
		(list (list queen "h") (list two "s") (list ten "d")
		      (list jack "c") (list queen "d"))
		one-pairs-const #f)
	  (list (list (list five "c") (list queen "c") (list jack "c")
		      (list four "h") (list nine "d"))
		high-cards-const
		(list (list five "h") (list jack "s") (list four "d")
		      (list nine "c") (list queen "d"))
		high-cards-const #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (hvalue-1 (list-ref alist 1))
		 (card-list-2 (list-ref alist 2))
		 (hvalue-2 (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		hvalue-1 hvalue-2
		hand1-greater-hand2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (hand1-equal-hand2? card-list-1 card-list-2)
  (let ((freq-suit-1-htable (make-hash-table 10))
	(freq-suit-2-htable (make-hash-table 10))
	(flush-suit-1 #f)
	(flush-suit-2 #f))
    (begin
      (update-suit-frequency-hash! freq-suit-1-htable card-list-1)
      (update-suit-frequency-hash! freq-suit-2-htable card-list-2)
      (let ((fsuit (get-nth-of-a-kind-num freq-suit-1-htable 5)))
	(set! flush-suit-1 fsuit))

      (let ((fsuit (get-nth-of-a-kind-num freq-suit-2-htable 5)))
	(set! flush-suit-2 fsuit))

      (cond
       ((or (and (equal? flush-suit-1 #f) (equal? flush-suit-2 #f))
	    (and (not (equal? flush-suit-1 #f)) (not (equal? flush-suit-2 #f))))
	(begin
	  (let ((elist1
		 (sort (extract-values-from-card-list card-list-1) >))
		(elist2
		 (sort (extract-values-from-card-list card-list-2) >)))
	    (equal? elist1 elist2)
	    )))
       (else
	(begin
	  #f
	  ))
       ))))

;;;#############################################################
;;;#############################################################
(define (test-hand1-equal-hand2-1)
  (let ((sub-name "test-hand1-equal-hand2-1")
	(test-list
	 (list
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list ten "d") (list king "d") (list queen "d")
		      (list jack "d") (list ace "d"))
		royal-flush-const #t)
	  (list (list (list ace "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		royal-flush-const
		(list (list queen "d") (list jack "d") (list ten "d")
		      (list nine "d") (list eight "d"))
		straight-flush-const #f)
	  (list (list (list king "c") (list queen "c") (list jack "c")
		      (list ten "c") (list nine "c"))
		straight-flush-const
		(list (list king "h") (list queen "h") (list jack "h")
		      (list ten "h") (list ace "h"))
		royal-flush-const #f)
	  (list (list (list nine "c") (list king "c") (list queen "c")
		      (list jack "c") (list ten "c"))
		straight-flush-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "d"))
		four-of-a-kind-const #f)
	  (list (list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "d"))
		four-of-a-kind-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "c"))
		four-of-a-kind-const #t)
	  (list (list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list seven "d"))
		four-of-a-kind-const
		(list (list queen "d") (list queen "c") (list queen "h")
		      (list queen "s") (list eight "c"))
		four-of-a-kind-const #f)
	  (list (list (list queen "d") (list ten "d") (list nine "d")
		      (list jack "d") (list eight "d"))
		straight-flush-const
		(list (list queen "d") (list ten "c") (list nine "h")
		      (list jack "s") (list eight "c"))
		straight-const #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((card-list-1 (list-ref alist 0))
		 (hvalue-1 (list-ref alist 1))
		 (card-list-2 (list-ref alist 2))
		 (hvalue-2 (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (begin
	       (generic-test-hand1-greater-hand2?
		sub-name card-list-1 card-list-2
		hvalue-1 hvalue-2
		hand1-equal-hand2? test-label-index shouldbe)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (problem-hand-comparison-test-1)
  (let ((sub-name "problem-hand-comparison-test-1")
	(test-list
	 (list
	  (list "5H 5C 6S 7S KD" "2C 3S 8S 8D TD" 2)
	  (list "5D 8C 9S JS AC" "2C 5C 7D 8S QH" 1)
	  (list "2D 9C AS AH AC" "3D 6D 7D TD QD" 2)
	  (list "4D 6S 9H QH QC" "3D 6D 7H QD QS" 1)
	  (list "2H 2D 4C 4D 4S" "3C 3D 3S 9S 9D" 1)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((cstring-1 (list-ref alist 0))
		 (cstring-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((card-list-1
		    (map
		     string-to-card
		     (string-split cstring-1 #\space)))
		   (card-list-2
		    (map
		     string-to-card
		     (string-split cstring-2 #\space))))
	       (let ((hvalue1 (calc-hand-value card-list-1))
		     (hvalue2 (calc-hand-value card-list-2)))
		 (let ((hand-1-string (hash-ref hand-name-hash hvalue1 #f))
		       (hand-2-string (hash-ref hand-name-hash hvalue2 #f))
		       (winner-string #f))
		   (begin
		     (if (hand1-greater-hand2? card-list-1 card-list-2)
			 (set! winner-string "    player 1 won"))
		     (if (hand1-greater-hand2? card-list-2 card-list-1)
			 (set! winner-string "    player 2 won"))
		     (if (hand1-equal-hand2? card-list-1 card-list-2)
			 (set! winner-string "    tie"))

		     (display (format #f "hand = ~a : player 1) ~a : ~a~a~%"
				      test-label-index cstring-1 hand-1-string
				      (if (not (equal?
						(string-index winner-string #\1) #f))
					  winner-string "")))
		     (display (format #f "         : player 2) ~a : ~a~a~%"
				      cstring-2 hand-2-string
				      (if (equal?
					   (string-index winner-string #\1) #f)
					  winner-string "")))
		     (newline)
		     (force-output)
		     )))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop filename)
  (let ((counter 0)
	(player1-wins 0)
	(player2-wins 0)
	(ties 0))
    (begin
      (if (file-exists? filename)
	  (begin
	    (with-input-from-file filename
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\n")
			   (ice9-rdelim:read-delimited ",\n")))
		    ((eof-object? line))
		  (begin
		    (cond
		     ((not (eof-object? line))
		      (let ((this-string (string-downcase line)))
			(let ((slist (string-split this-string #\space)))
			  (if (and
			       (list? slist) (> (length slist) 9))
			      (begin
				(let ((slength (length slist)))
				  (let ((card-list-1
					 (map string-to-card (list-head slist 5)))
					(card-list-2
					 (map string-to-card (list-tail slist 5))))
				    (begin
				      (set! counter (1+ counter))
				      (if (hand1-greater-hand2? card-list-1 card-list-2)
					  (begin
					    (set! player1-wins (1+ player1-wins)))
					  (begin
					    (if (hand1-greater-hand2? card-list-2 card-list-1)
						(begin
						  (set! player2-wins (1+ player2-wins)))
						(begin
						  (set! ties (1+ ties))
						  )))
					  ))
				    ))))))))
		    ))))
	    (if (> counter 0)
		(begin
		  (let ((p1-pcnt
			 (* 0.010
			    (truncate
			     (* 10000.0 (exact->inexact
					 (/ player1-wins counter))))))
			(p2-pcnt
			 (* 0.010
			    (truncate
			     (* 10000.0 (exact->inexact
					 (/ player2-wins counter))))))
			(ties-pcnt
			 (* 0.010
			    (truncate
			     (* 10000.0 (exact->inexact
					 (/ ties counter)))))))
		    (begin
		      (display (ice9-format:format #f "player 1 wins ~:d out of ~:d (~f%)~%"
						   player1-wins counter p1-pcnt))
		      (display (ice9-format:format #f "player 2 wins ~:d out of ~:d (~f%)~%"
						   player2-wins counter p2-pcnt))
		      (display (ice9-format:format #f "number of ties ~:d out of ~:d (~f%)~%"
						   ties counter ties-pcnt))
		      (display (ice9-format:format #f "total = ~:d out of ~:d (~f%)~%"
						   (+ player1-wins player2-wins ties)
						   counter (+ p1-pcnt p2-pcnt ties-pcnt)))
		      (newline)
		      (force-output)
		      )))
		(begin
		  (display (format #f "no valid lines found in file ~a~%" filename))
		  ))
	    )))))

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
    (display (format #f "Problem 054 - In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:~%"))
    (display (format #f "  High Card: Highest value card.~%"))
    (display (format #f "  One Pair: Two cards of the same value.~%"))
    (display (format #f "  Two Pairs: Two different pairs.~%"))
    (display (format #f "  Three of a Kind: Three cards of the same value.~%"))
    (display (format #f "  Straight: All cards are consecutive values.~%"))
    (display (format #f "  Flush: All cards of the same suit.~%"))
    (display (format #f "  Full House: Three of a kind and a pair.~%"))
    (display (format #f "  Four of a Kind: Four cards of the same value.~%"))
    (display (format #f "  Straight Flush: All cards are consecutive values of same suit.~%"))
    (display (format #f "  Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.~%"))
    (newline)
    (display (format #f "The cards are valued in the order:~%"))
    (display (format #f "2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.~%"))
    (newline)
    (display (format #f "If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.~%"))
    (newline)
    (display (format #f "Consider the following five hands dealt to two players:~%"))
    (newline)
    (display (format #f "Hand    Player 1    Player 2    Winner~%"))
    (display (format #f "1    5H 5C 6S 7S KD Pair of Fives    2C 3S 8S 8D TD Pair of Eights    Player 2~%"))
    (display (format #f "2    5D 8C 9S JS AC Highest card Ace   2C 5C 7D 8S QH Highest card Queen    Player 1~%"))
    (display (format #f "3    2D 9C AS AH AC Three Aces     3D 6D 7D TD QD Flush with Diamonds    Player 2~%"))
    (display (format #f "4    4D 6S 9H QH QC Pair of Queens Highest card Nine    3D 6D 7H QD QS Pair of Queens Highest card Seven   Player 1~%"))
    (display (format #f "5    2H 2D 4C 4D 4S Full House With Three Fours    3C 3D 3S 9S 9D Full House with Three Threes    Player 1~%"))
    (newline)
    (display (format #f "The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.~%"))
    (newline)
    (display (format #f "How many hands does Player 1 win?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-string-to-card-1 counter)
	   (run-test test-update-value-frequency-hash-1 counter)
	   (run-test test-update-suit-frequency-hash-1 counter)
	   (run-test test-get-nth-of-a-kind-num-1 counter)
	   (run-test test-get-second-nth-of-a-kind-num-1 counter)
	   (run-test test-straight-1 counter)
	   (run-test test-get-high-card-value-1 counter)
	   (run-test test-calc-hand-value-1 counter)
	   (run-test test-straight1-greater-straight2-1 counter)
	   (run-test test-quads1-greater-quads2-1 counter)
	   (run-test test-fulls1-greater-fulls2-1 counter)
	   (run-test test-flushs1-greater-flushs2-1 counter)
	   (run-test test-triples1-greater-triples2-1 counter)
	   (run-test test-twopairs1-greater-twopairs2-1 counter)
	   (run-test test-onepairs1-greater-onepairs2-1 counter)
	   (run-test test-highcards1-greater-highcards2-1 counter)
	   (run-test test-hand1-greater-hand2-1 counter)
	   (run-test test-hand1-equal-hand2-1 counter)


	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (problem-hand-comparison-test-1)

    (force-output)
    (let ((filename "poker.txt"))
      (begin
	(time-code
	 (begin
	   (main-loop filename)
	   ))
	))

    (newline)
    ))

