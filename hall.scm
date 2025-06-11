(hall-description
  (name "zeta")
  (prefix "")
  (version "0.1")
  (author "")
  (copyright (2025))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory
              "zeta"
              ((scheme-file "term") (scheme-file "prompts")))
            (scheme-file "zeta")))
         (tests ((directory "tests" ())))
         (programs
           ((directory "scripts" ((scheme-file "main")))))
         (documentation
           ((directory "doc" ((texi-file "zeta")))
            (text-file "COPYING")
            (text-file "HACKING")
            (symlink "README" "README.org")
            (org-file "README")))
         (infrastructure
           ((scheme-file "hall")
            (text-file ".gitignore")
            (scheme-file "guix")))))
