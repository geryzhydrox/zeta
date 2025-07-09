(define-module (zeta-lib prompts)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 match)
  #:export (yn-prompt numbered-prompt))

(define (yn-prompt prompt)
  (let ((input (readline (string-append prompt " [y/n] "))))
    (match input
      ("y" #t)
      ("Y" #t)
      ("yes" #t)
      ("Yes" #t)
      ("n" #f)
      ("N" #f)
      ("no" #f)
      ("No" #f)
      (else (yn-prompt prompt)))))

(define (numbered-prompt prompt lst)
  (for-each (lambda (elem index)
	      (format #t "[~a] ~a\n" index elem))
	    lst
	    (map 1+ (iota (length lst))))

  (let choice ((input (string->number (readline (string-append prompt " ")))))
    (if (and
	 (integer? input)
	 (>= input 1)
	 (<= input (length lst)))
	(list-ref lst (- input 1))
	(choice (string->number (readline (string-append prompt " "))))
    )))
