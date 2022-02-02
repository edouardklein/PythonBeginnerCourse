(define-module (osef)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix profiles)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages readline)
  )

(define-public sbcl-exam
  (package
    (name "sbcl-exam")
    (version "0.1.0")
    (source #f)
    (inputs
     (list sbcl sbcl-numcl rlwrap sbcl-clack sbcl-lack sbcl-caveman sbcl-ningle
           sbcl-envy sbcl-cl-ppcre sbcl-verbose sbcl-uuid sbcl-access sbcl-cl-csv
           sbcl-arrows sbcl-alexandria sbcl-djula sbcl-datafly sbcl-sxql cl-asdf cl-str
           sbcl-slime-swank))
    (build-system trivial-build-system)
    (synopsis "dummy package")
    (description
     "BOUZA")
    (home-page "")
    (license #f)))

sbcl-exam
