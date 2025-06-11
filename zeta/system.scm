(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(define-module (zeta system)
  #:use-module (ice-9 match)
  #:use-module (zeta term)
  #:export (%zeta-root
	    relative->absolute
	    mkdir-p
	    touch
	    make-file-at-path
	    make-scm-file-at-path
	    write-file
	    read-pkgs))


(define %zeta-root (or (getenv "ZETA_ROOT")
		       (string-append (getenv "HOME") "/.zeta")
		       (error-with-msg "Something went seriously wrong. $HOME environment variable cannot be read.")))

(define (relative->absolute rel-path)
  (string-append %zeta-root
		 "/" rel-path
		 ".scm"))

(define (mkdir-p path)
  (system* "mkdir" "-p" path))

(define (touch filename)
  (system* "touch" "-f" filename))

(define (make-file-at-path path filename)
  (mkdir-p path)
  (touch (string-append path "/" filename)))

(define (make-scm-file-at-path path filename)
  (make-file-at-path path (string-append filename ".scm")))

(define (write-file filename str)
  (call-with-output-file filename
    (lambda (output-port)
      (display str output-port))))

(define (read-pkgs manifest)
  (call-with-input-file manifest
    (lambda (input-port)
      (let ((result (read input-port)))
	(match result
	  (('specifications->manifest
	    ('quote
	     (pkgs ...))) pkgs)
	  (_ '()))))))
