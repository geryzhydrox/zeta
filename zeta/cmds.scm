(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(define-module (zeta cmds)
  #:use-module (zeta prompts)
  #:use-module (zeta system)
  #:use-module (zeta term)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 readline)
  #:export (zeta-add
	    zeta-del
	    zeta-install
	    zeta-remove))

(define* (zeta-add manifest-path #:optional (rebuild? #t))
  (if (list? manifest-path)
      (for-each zeta-add manifest-path)
      (let* ((slash-index (string-rindex manifest-path #\/))
	     (path (if slash-index
		       (string-append %zeta-root "/"
				      (string-take manifest-path (1+ slash-index)))
		       %zeta-root))
	     (filepath (relative->absolute manifest-path)))
	;; Abuse short-circuiting behaviour of `or` and `and` 
	(when (or (and (file-exists? filepath)
		       (yn-prompt (format #f "Manifest ~a already exists. Overwrite?" filepath))
		       (delete-file filepath))
		  (not (file-exists? filepath)))
	  (info-with-msg (format #f "Adding manifest ~a" filepath))
	  (mkdir-p path)
	  (touch filepath)
	  (let* ((manifests (read-manifests %root-manifest))
		 (new-manifests (if (member filepath manifests)
				    (begin
				      (info-with-msg "Manifest already contained in root. Skipping...")
				      manifests)
				    (append manifests (list filepath))))
		 (new-root (root-with-manifests new-manifests)))
	    (write-file %root-manifest new-root)
	    (when rebuild? (apply-root-manifest))
	  )))))

(define (zeta-del manifest-path)
  (if (list? manifest-path)
      (for-each zeta-del manifest-path)
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
	  (write-file %root-manifest new-root)
	  (apply-root-manifest))
        )))

(define (zeta-install manifest-path pkgs)
  (define creating-new-manifest? #f)
  (define recurse? #t)
  (when (eq? (length pkgs) 1) (set! recurse? #f))
  (define pkg (car pkgs))
  (begin
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
      (when creating-new-manifest? (zeta-add manifest-path #f))
      (unless (file-exists? filepath)
	(info-with-msg (format #f "Specified manifest ~a does not exist" filepath))
	(when (yn-prompt "Create manifest?") (zeta-add manifest-path #f)))
      (info-with-msg (format #f "Installing package ~a at manifest ~a" pkg filepath))
      (let* ((manifest-pkgs (read-pkgs filepath))
	     (new-pkgs (if (member pkg manifest-pkgs)
			   (begin
			     (info-with-msg "Package already installed. Skipping...")
			     manifest-pkgs)
			   (append manifest-pkgs (list pkg))))
	     (new-file (manifest-with-pkgs new-pkgs)))
	(write-file filepath new-file)
	;; (apply-root-manifest)
	)))
  (if recurse?
      (zeta-install manifest-path (cdr pkgs))
      (apply-root-manifest))
  )

(define (zeta-remove manifest-path pkgs)
  (define available-manifests '())
  (define manifest-provided? manifest-path)
  (define recurse? #t)
  (when (eq? (length pkgs) 1) (set! recurse? #f))
  (define pkg (car pkgs))
  (begin
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
      (let ((answer (and (not (nil? available-manifests))
			 (numbered-prompt (format #f "Choose manifest to remove `~a` from:" pkg) available-manifests))))
	(set! manifest-path
	      (if answer
		  (string-drop-right 
		   (string-drop answer (1+ (string-length %zeta-root))) 4) 
		  (error-with-msg "Specified package not installed."))
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
	)))
    (if recurse?
	(if manifest-provided? 
	    (zeta-remove manifest-path (cdr pkgs))
	    (zeta-remove #f (cdr pkgs)))
	(apply-root-manifest)))
