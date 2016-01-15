;;
;; Boyer-Moore Search algorithim library
;; Copyright (C) 2016 William Ziener-Dignazio
;;
#!r6rs

(use-modules (srfi srfi-69))

;; Returns a sorted list of the positions of occurrences of a value in a list
(define (occurrences2 c xs n)
  (if (null? xs)
      '() (if (eq? c (car xs))
	      (cons n (occurrences2 c (cdr xs) (+ n 1)))
	      (occurrences2 c (cdr xs) (+ n 1)))))

(define (occurrences c xs)
  (occurrences2 c xs 0))

;; From left -> right, find the first mismatch between xs and ys
;; If there is a mismatch, a pair containing (in order) the position at which there was a mismatch,
;; and the character of ys that xs mismatched. In other words, at a mismatch the character in ys will be returned.
;; If there are no mismatched between xs and ys (for the length of xs), then simply the length the
;; routine traveled will be returned.
;; eg.  (first-mismatch '(a b c) '(a b c d)) => (3)
;;	(first-mismatch '(a b z) '(a b c d)) => (3 . c)
(define (first-mismatch xs ys)
  (if (or (null? xs) (null? ys))
      '(0)
      (if (eq? (car xs) (car ys))
	  (let ([result (first-mismatch (cdr xs) (cdr ys))])
	    (cons (+ 1 (car result)) (cdr result)))
	  (cons '0 (car ys)))))

(define (list-get xs n)
  (if (null? xs)
      '() (if (> n 0)
	      (list-get (cdr xs) (- n 1))
	      (car xs))))

(define (sublist2 xs p n m )
  (if (null? xs)
      '() (if (< p n)
	      (sublist2 (cdr xs) (+ p 1) n m)
	      (if (< p m)
		  (cons (car xs) (sublist2 (cdr xs) (+ p 1) n m))
		  '()))))

(define (sublist xs n m)
  (sublist2 xs 0 n m))

(define (sublist-from xs n)
  (if (null? xs)
      '() (if (> n 0)
	      (sublist-from (cdr xs) (- n 1))
	      xs)))

(define (fst p) (car p))
(define (snd p) (cdr p))

(define (bm-search-full pattern pattern-reversed patlen position text delta1-table)
  (if (null? text)
      '() (let ([char	  (list-get text (- patlen 1))]
		[mismatch (first-mismatch pattern-reversed (reverse (sublist text 0 patlen)))])
	    (if (= (car mismatch) patlen) ; Match found
		(cons position (bm-search-full pattern pattern-reversed patlen
					       (+ position 1) (cdr text) delta1-table))
		;; Now we have a problem, we found some matches but not the whole string did.
		;; So now, if there is a match of the new char where we mismatched at, work from there.
		(let ([char-occurrence (hash-table-ref/default delta1-table (snd mismatch) 0)])
		  (if (> char-occurrence (- (- patlen (fst mismatch)) 1))
		      ;; The rightmost occurrence is to the right of our mismatch, this would
		      ;; be a useless shift 'backwards', so we safely move forward 1
		      (bm-search-full pattern pattern-reversed patlen (+ position 1) (cdr text) delta1-table)
		      ;; Otherwise, we can safely shift up to that next character occurrence
		      (let ([shift (- (- patlen char-occurrence) 1)])
			(bm-search-full pattern pattern-reversed patlen	(+ position shift) (sublist-from text shift) delta1-table))))))))

(define (bm-search pattern-str text-str)
  (letrec* ([pattern		(string->list pattern-str)]
	    [text		(string->list text-str)]
	    [pattern-reversed	(reverse pattern)]
	    [patlen		(length pattern)]
	    [delta1-table 		(make-hash-table)])
    (map (lambda (ch) (hash-table-set! delta1-table ch (car (reverse (occurrences ch pattern))))) pattern)
    (bm-search-full pattern pattern-reversed patlen 0 text delta1-table)))
