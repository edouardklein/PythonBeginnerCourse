(define-module (osef)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system asdf)
  #:use-module (guix profiles)
  #:use-module (guix git-download)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages readline)
  #:use-module ((guix licenses) #:prefix license:)
  )


(define-public sbcl-linear-programming
  (package
   (name "sbcl-linear-programming")
   (version "2.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/neil-lindquist/linear-programming.git")
           (commit "417f764e65be1a92f7f9fc0f1ed8092700e916c6")))
     (sha256
      (base32
       "0waqf2aam3ibp97472x7kbk1sk0f5ckwpdd7j9cfrlzkgk982vyn"))
     (file-name (git-file-name name version))))
   (build-system asdf-build-system/sbcl)
   (native-inputs
    (list sbcl-iterate sbcl-alexandria sbcl-fiveam))
   (synopsis "osef")
   (description
    "osef")
   (home-page "https://github.com/neil-lindquist/linear-programming")
   (license license:expat)))

(define-public sbcl-exam
  (package
    (name "sbcl-exam")
    (version "0.1.0")
    (source #f)
    (inputs
     (list sbcl sbcl-numcl rlwrap sbcl-clack sbcl-lack sbcl-caveman sbcl-ningle
           sbcl-envy sbcl-cl-ppcre sbcl-verbose sbcl-uuid sbcl-access sbcl-cl-csv
           sbcl-arrows sbcl-alexandria sbcl-djula sbcl-datafly sbcl-sxql cl-asdf cl-str
           sbcl-slime-swank sbcl-linear-programming))
    (build-system trivial-build-system)
    (synopsis "dummy package")
    (description
     "BOUZA")
    (home-page "")
    (license #f)))

sbcl-exam
