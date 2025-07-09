(define-module (zeta-lib system)
  #:use-module (ice-9 match)
  #:use-module (zeta-lib term)
  #:export (%zeta-root
	    %root-manifest
	    %rebuild?
	    apply-root-manifest
	    relative->absolute
	    mkdir-p
	    touch
	    make-file-at-path
	    make-scm-file-at-path
	    read-file
	    write-file
	    read-pkgs
	    read-manifests
	    manifest-with-pkgs
	    root-with-manifests
	    define-recursive))


(define %zeta-root (or (getenv "ZETA_ROOT")
		       (string-append (getenv "HOME") "/.zeta")
		       (error-with-msg "Something went seriously wrong. $HOME environment variable cannot be read.")))

(define %root-manifest
  (string-append %zeta-root "/root.scm"))

(define %rebuild? #f)

(define* (apply-root-manifest #:optional (root-file %root-manifest))
  (unless (eq? (system* "guix" "package" "-m" root-file) 0)
    (error-with-msg "Guix package command failed. Make sure `guix` is properly installed."
		    )))

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

(define (read-file filepath)
  (call-with-input-file filepath
    (lambda (input-port)
      (read input-port))))

(define (read-pkgs manifest)
  (match (read-file manifest)
    (('specifications->manifest
      ('quote
       (pkgs ...))) pkgs)
    (_ '())))

(define (read-manifests root-file)
  (match (read-file root-file)
    (('concatenate-manifests
      ('map ('lambda ('filepath)
	      ('primitive-eval
	       ('call-with-input-file 'filepath
		 ('lambda ('input-port)
		   ('read 'input-port)))))
	    ('quote (manifests ...)))) manifests)
    (_ '())))

(define (manifest-with-pkgs pkgs)
  (format #f "(specifications->manifest\n '(~a))"
	  (string-join
	   (map (lambda (pkg)
		  (format #f "\"~a\"" pkg))
		pkgs)
	   "\n   ")))

(define (root-with-manifests manifests)
  (format #f
"(concatenate-manifests
  (map (lambda (filepath)
     (primitive-eval
       (call-with-input-file filepath
         (lambda (input-port)
           (read input-port)))))
  '(~a)))"
	  (string-join
	   (map (lambda (manifest)
		  (format #f "\"~a\"" manifest))
		manifests)
	   "\n    ")))


(define-syntax define-recursive
  ;; Macro for simplifying the definition of procedures that act on lists recursively.
  ;; First argument MUST be a list.
  (lambda (x)
    (syntax-case x (single recurse finish)
      ((define-recursive (proc-name list-arg ...)
	 (single identifier)
	 exp ...
	 ;; Recurse with arguments recurse-arg ... 
	 (recurse recurse-arg ...)
	 ;; If finished, evaluate expressions finish-exp ...
	 (finish finish-exp ...))
       ;; (with-syntax ((item (datum->syntax x 'item)))
       #'(define (proc-name list-arg ...)
	   (define recurse? (not (nil? (cdar (list list-arg ...)))))
	   ;; `item` is introduced as a binding for a "single element" of the list
	   (let ((identifier (caar (list list-arg ...))))
	     exp ...
	     (if recurse?
		 (proc-name recurse-arg ...)
		 (begin finish-exp ...))))
       ))))
