(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(define-module (zeta system)
  #:use-module (ice-9 match)
  #:use-module (zeta term)
  #:export (%zeta-root
	    %root-manifest
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
	    root-with-manifests))


(define %zeta-root (or (getenv "ZETA_ROOT")
		       (string-append (getenv "HOME") "/.zeta")
		       (error-with-msg "Something went seriously wrong. $HOME environment variable cannot be read.")))

(define %root-manifest
  (string-append %zeta-root "/root.scm"))

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
