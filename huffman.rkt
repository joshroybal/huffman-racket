#lang racket/base

;;; Uses SICP exercises as basic for a file compression utility.

;;; definitions

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ;symbol
			       (cadr pair))  ;frequency
		    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
	 (if (eq? symbol (symbol-leaf tree))
	     '()
	     (error "missing symbol: ENCODE-SYMBOL" symbol)))
	(else
	 (if (memq symbol (symbols (left-branch tree)))
	     (cons 0 (encode-symbol symbol (left-branch tree)))
	     (cons 1(encode-symbol symbol (right-branch tree)))))))
  
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (let ((lightest-tree (car trees)) (heavier-trees (cdr trees)))
    (if (null? heavier-trees)
	lightest-tree
	(successive-merge
	 (adjoin-set
	  (make-code-tree lightest-tree (car heavier-trees))
	  (cdr heavier-trees))))))

(define (alphabet n)
  (if (= n 0)
      '()
      (cons (list n (expt 2 (- n 1))) (alphabet (- n 1)))))

;;; text-file->character-list
(define (read-file filename)
  (let ((infile (open-input-file filename)))
    (define (iter result)
      (let ((c (read-char infile)))
        (if (eof-object? c)
            (reverse result)
            (iter (cons c result)))))
    (iter '())))

(define (unique-symbols text)
  (define (iter rest result)
    (cond ((null? rest) result)
	  ((member (car rest) result) (iter (cdr rest) result))
	  (else (iter (cdr rest) (cons (car rest) result)))))
  (iter text '()))

(define (count-symbol symbol text)
  (define (iter rest result)
    (cond ((null? rest) result)
	  ((equal? symbol (car rest)) (iter (cdr rest) (+ 1 result)))
	  (else (iter (cdr rest) result))))
  (iter text 0))

(define (symbol-counts symbols text)
  (define (iter rest result)
    (if (null? rest)
	result
	(iter (cdr rest)
	      (cons (list (car rest) (count-symbol (car rest) text)) result))))
  (iter symbols '()))

(define (bit-list->byte-list bits)
  (define (iter k rest octet bytes)
    (cond ((null? rest) (cons octet bytes))
          ((= (length octet) 8)
           (iter (+ k 1)
                 (cdr rest)
                 (list (car rest))
                 (cons octet bytes)))
          (else (iter (+ k 1)
                      (cdr rest)
                      (cons (car rest) octet)
                      bytes))))
  (map octet->byte (iter 0 bits '() '())))

(define (octet->byte octet)
  (string->number (apply string-append (map number->string octet)) 2))

(define (byte-list->bit-list bytes)
  (define (pad octet)
    (if (= 8 (length octet))
        octet
        (pad (cons 0 octet))))
  (define (byte->octet byte)
    (map (lambda (x) (- (char->integer x) 48))
         (string->list (number->string byte 2))))
  (define (iter rest bits)
    (if (null? rest)
        bits
        (iter (cdr rest) (cons (reverse (pad (byte->octet (car rest)))) bits))))
  (apply append (iter bytes '())))

(define (write-binary-file byte-list filename)
  (let ((outfile (open-output-file filename #:exists 'replace)))
    (define (iter rest)
      (cond ((null? rest)
             (close-output-port outfile)
             'done)
            (else
             (write-byte (car rest) outfile)
             (iter (cdr rest)))))
    (iter byte-list)))
           
;;; text-file->byte-list
(define (read-binary-file filename)
  (let ((infile (open-input-file filename)))
    (define (iter result)
      (let ((c (read-byte infile)))
        (if (eof-object? c)
            (reverse result)
            (iter (cons c result)))))
    (iter '())))

;;; processing
(define args (current-command-line-arguments))
(cond ((< (vector-length args) 2)
       (display "Usage: huffman infile outfile")
       (newline)
       (exit)))
(define infile (vector-ref args 0))
(define outfile (vector-ref args 1))
(define text (read-file infile))
(define character-list (unique-symbols text))
(define character-counts (symbol-counts character-list text))
(define message-tree (generate-huffman-tree character-counts))
(define bit-list (encode text message-tree))
(define byte-list (bit-list->byte-list bit-list))
(write-binary-file byte-list outfile)
(display "Huffman encoded compressed archive written to file: ")
(display outfile)
(newline)