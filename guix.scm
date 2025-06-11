(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "zeta")
  (version "0.1")
  (source "./zeta-0.1.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0
	     "guile-lib" ,guile-lib)))
  (propagated-inputs `())
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

