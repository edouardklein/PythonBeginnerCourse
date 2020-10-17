(define-module (beaverlabs packages courses)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages tls)
  )

(define-public python-beginner-course
  (package
    (name "python-beginner-course")
    (version "0.0.2")
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (let ((out (assoc-ref %outputs "out")))
                             (mkdir out)
                             (const #t))))
    (synopsis "FIXME")
    (description
     "FIXME")
    (propagated-inputs
     `(("python" ,python)
       ("sbcl" ,sbcl)
       ("openssl" ,openssl)))
    (native-search-paths
     (list (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib/")))))
    (home-page "FIXME")
    (license gpl3+)))

(packages->manifest
 (list python-beginner-course))
