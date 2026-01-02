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
	 (commit "dac3a79f5f6760d6e6d323f0f53f4742b22a14d8")))
   (sha256
    (base32 "133wsy4mbpkibnp9bar0whpnkzp2i2rfg4m57116l6k6a7zplnbq"))))
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
