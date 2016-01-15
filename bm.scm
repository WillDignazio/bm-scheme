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

(define (first-mismatch xs ys)
  (if (null? xs)
      0 (if (null? ys)
	    0 (if (eq? (car xs) (car ys))
		  (+ 1 (first-mismatch (cdr xs) (cdr ys))) 0))))

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

(define (bm-search-full pattern pattern-reversed patlen position text)
  (if (null? text)
      '() (let ([char			(list-get text (- patlen 1))]
		[mismatch-position	(first-mismatch pattern-reversed (reverse (sublist text 0 patlen)))])
	    (if (= mismatch-position patlen) ; Match found
		(cons position (bm-search-full pattern pattern-reversed patlen
					       (+ position 1) (cdr text))) ; Move forward one
		;; Now we have a problem, we found some matches but not the whole string did.
		;; So now, if there is a match of the new char where we mismatched at, work from there.
		(let ([char-occurrences	(reverse (occurrences char pattern))])
		  (if (null? char-occurrences)
		      (bm-search-full pattern pattern-reversed patlen
				      (+ position (- patlen mismatch-position))
				      (sublist-from text (- patlen mismatch-position)))
		      (if (> (car char-occurrences) (- (- patlen mismatch-position) 1))
			  ;; The rightmost occurrence is to the right of our mismatch, this would
			  ;; be a useless shift 'backwards', so we safely move forward 1
			  (bm-search-full pattern pattern-reversed patlen (+ position 1) (cdr text))
			  ;; Otherwise, we can safely shift up to that next character occurrence
			  (let ([shift (- (- patlen (car char-occurrences)) 1)])
			    (bm-search-full pattern pattern-reversed patlen
					    (+ position shift)
					    (sublist-from text shift))))))))))

(define (bm-search pattern-str text-str)
  (letrec* ((pattern		(string->list pattern-str))
	    (text		(string->list text-str))
	    (pattern-reversed	(reverse pattern))
	    (patlen		(length pattern)))
    (bm-search-full pattern pattern-reversed patlen 0 text)))
