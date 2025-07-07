(define-module (lib cmds)
  #:use-module (lib prompts)
  #:use-module (lib system)
  #:use-module (lib term)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 readline)
  #:export (zeta-add
	    zeta-del
	    zeta-install
	    zeta-remove
	    zeta-init
	    zeta-list))

(define-recursive (zeta-add manifest-paths)
  (single manifest-path)
  (let* ((slash-index (string-rindex manifest-path #\/))
	 (path (if slash-index
		   (string-append %zeta-root "/"
				  (string-take manifest-path (1+ slash-index)))
		   %zeta-root))
	 (filepath (relative->absolute manifest-path)))
    ;; Abuse short-circuiting behaviour of `or` and `and` 
    (when (or (and (file-exists? filepath)
		   (yn-prompt (format #f "Manifest ~a already exists. Overwrite?" filepath))
		   (delete-file filepath)
		   (set! %rebuild? #t))
	      (not (file-exists? filepath)))
      (info-with-msg (format #f "Adding manifest ~a" filepath))
      (mkdir-p path)
      (touch filepath)
      (write-file filepath (manifest-with-pkgs '()))
      (let* ((manifests (read-manifests %root-manifest))
	     (new-manifests (if (member filepath manifests)
				(begin
				  (info-with-msg "Manifest already contained in root. Skipping...")
				  manifests)
				(append manifests (list filepath))))
	     (new-root (root-with-manifests new-manifests)))
	(write-file %root-manifest new-root))))
    (recurse
     (cdr manifest-paths) %rebuild?)
    (finish
     (when %rebuild? (apply-root-manifest))))

(define-recursive (zeta-del manifest-paths)
  (single manifest-path)
  (let ((filepath (relative->absolute manifest-path)))
    (unless (file-exists? filepath)
      (error-with-msg (format #f "Specified manifest ~a does not exist" filepath)))
    (info-with-msg (format #f "Deleting manifest ~a" filepath))
    (delete-file filepath)
    (let* ((manifests (read-manifests %root-manifest))
	   (new-manifests (if (member filepath manifests)
			      (delete filepath manifests)
			      (begin
				(info-with-msg "Manifest not contained in root. Skipping..."))))
	   (new-root (root-with-manifests new-manifests)))
      (write-file %root-manifest new-root)))
    (recurse
     (cdr manifest-paths))
    (finish
     (apply-root-manifest)))

(define-recursive (zeta-install pkgs manifest-path)
  (single pkg)
  (define creating-new-manifest? #f)
  (unless manifest-path
    (info-with-msg "No manifest specified")
    (let* ((answer (numbered-prompt "Install at:"  
				    (append (read-manifests %root-manifest) (list "Create new manifest"))))
	   (new-manifest-path (if (string= answer "Create new manifest") 
				  (begin (set! creating-new-manifest? #t) (readline "Create new manifest at: "))
				  (string-drop-right 
				   (string-drop answer (1+ (string-length %zeta-root))) 4))))
      (set! manifest-path new-manifest-path)))
  (let ((filepath (relative->absolute manifest-path)))
    (when creating-new-manifest? (zeta-add (list manifest-path)))
    (unless (file-exists? filepath)
      (info-with-msg (format #f "Specified manifest ~a does not exist" filepath))
      (when (yn-prompt "Create manifest?") (zeta-add (list manifest-path))))
    (info-with-msg (format #f "Installing package ~a at manifest ~a" pkg filepath))
    (let* ((manifest-pkgs (read-pkgs filepath))
	   (new-pkgs (if (member pkg manifest-pkgs)
			 (begin
			   (info-with-msg "Package already installed. Skipping...")
			   manifest-pkgs)
			 (append manifest-pkgs (list pkg))))
	   (new-file (manifest-with-pkgs new-pkgs)))
      (write-file filepath new-file)))
  (recurse
   (cdr pkgs) manifest-path)
  (finish
   (apply-root-manifest)))

(define-recursive (zeta-remove pkgs manifest-path)
  (single pkg)
  (define available-manifests '())
  (define manifest-provided? manifest-path)
  (unless manifest-path
    (info-with-msg "No manifest specified")
    (ftw %zeta-root
	 (lambda (filename statinfo flag)
	   (when (and
		  (eq? flag 'regular)
		  (not (string= filename %root-manifest))
		  (member pkg (read-pkgs filename)))
	     (set! available-manifests (append available-manifests (list filename))))
	   #t
	   ))
    (let ((answer (cond ((nil? available-manifests) #f)
			((equal? (length available-manifests) 1) (car available-manifests))
			(#t (numbered-prompt (format #f "Choose manifest to remove `~a` from:" pkg) available-manifests)))))
      (set! manifest-path
	    (if answer
		(string-drop-right 
		 (string-drop answer (1+ (string-length %zeta-root))) 4) 
		(error-with-msg "Specified package is not installed."))
	    )))
  (let ((filepath (relative->absolute manifest-path)))
    (unless (file-exists? filepath)
	(error-with-msg (format #f "Specified manifest ~a does not exist" filepath)))
    (info-with-msg (format #f "Deleting package ~a from manifest ~a" pkg filepath))
    (let* ((manifest-pkgs (read-pkgs filepath))
	   (new-pkgs (if (member pkg manifest-pkgs)
			 (delete pkg manifest-pkgs)
			 (begin
			   (info-with-msg "Package not installed. Skipping...")
			   manifest-pkgs)))
	   (new-file (manifest-with-pkgs new-pkgs)))
      (write-file filepath new-file)
      ))
  (recurse
   (cdr pkgs)
   (if manifest-provided?
       manifest-path
       #f))
  (finish
   (apply-root-manifest)))

(define* (zeta-init #:optional (manual #t))
  (when (or
	 (not manual)   
	 (and
	  manual
	  (file-exists? %zeta-root)
	  (yn-prompt "Root manifest already exists. Overwrite?")))
    (make-file-at-path %zeta-root "root.scm")
    (set! %root-manifest (string-append %zeta-root "/" "root.scm"))
    ))

(define (zeta-list)
  (define pkg+locations '())
  (ftw %zeta-root
	 (lambda (filename statinfo flag)
	   (when (and
		  (eq? flag 'regular)
		  (not (string= filename %root-manifest)))
	     (for-each (lambda (pkg+location)
			 (when (not
				(member pkg+location pkg+locations))
			   (set! pkg+locations
				 (append pkg+locations (list (list pkg+location filename))))))
		       (read-pkgs filename)))
	   #t))
  (for-each (lambda (pkg+location)
	      (format #t "~15a ~a\n" (car pkg+location)
		      (cadr pkg+location))) pkg+locations))
