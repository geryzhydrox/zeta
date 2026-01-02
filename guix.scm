(use-modules
 (guix build-system guile)
 (gnu packages guile)
 (guix packages)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix licenses))

(package
 (name "gideon")
 (version "0.1")
 (source
  (origin
   (method git-fetch)
   (uri (git-reference
	 (url "https://github.com/geryzhydrox/gideon")
	 (commit "3b213e055c51a33a883f533a039948d90fec87f4")))
   (sha256
    (base32 "19mn3clcwn9m72my6y7v03z81cmp9k2sv7f4d13hw6q4zkz570bm"))))
 (build-system guile-build-system)
 (arguments
  '(#:source-directory "src"
    #:phases (modify-phases %standard-phases
			    (add-after 'build 'install
				       (lambda* (#:key outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(bin (string-append out "/bin")))
					   (install-file "src/gideon" bin)
					   ))))))
 (native-inputs (list
		 guile-3.0
		 guile-readline))
 (propagated-inputs (list
		     guile-3.0
		     guile-readline))
 (synopsis "Imperative `guix` wrapper.")
 (description "")
 (home-page "https://github.com/geryzhydrox/gideon")
 (license gpl3))
