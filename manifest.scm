(define-module (beaverlabs packages courses)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix profiles)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages tls)
  )

(define-public python-seaborn-11
  (package
   (inherit python-seaborn)
   (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seaborn" version))
       (sha256
        (base32 "1b81p9s6li3b606ivgbynrh102fclwlw74m8qxzsii9pv6p8xrs4"))))
        (arguments
         `(#:phases
           (modify-phases %standard-phases
                          (delete 'check))))
))

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
       ;("jupyter" ,jupyter)
       ;("python-seaborn" ,python-seaborn-11)
       ("python-pandas" ,python-pandas)
                                        ;("openssl" ,openssl)
       ))
    (native-search-paths
     (list (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib/")))))
    (home-page "FIXME")
    (license gpl3+)))


(define-public python-cwcwidth
  (package
    (name "python-cwcwidth")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cwcwidth" version))
        (sha256
          (base32
            "1azrphpkcyggg38xvkfb9dpc4xmmm90p02kf8dkqd4d6j5w96aj8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cython" ,python-cython)
       ))
    (home-page
      "https://github.com/sebastinas/cwcwidth")
    (synopsis "Python bindings for wc(s)width")
    (description "Python bindings for wc(s)width")
    (license #f)))

(define-public python-curtsies-0.3.5
  (package (inherit python-curtsies)
           (version "0.3.5")
           (source
            (origin
             (method url-fetch)
             (uri (pypi-uri "curtsies" version))
             (sha256
              (base32
               "1g8dwafx4vx06isjkn28r3cwb0hw1bv67lgygaz34yk66lrzz1x5"))))
           (propagated-inputs
            `(("python-blessings" ,python-blessings)
              ("python-wcwidth" ,python-wcwidth)))
               (arguments
                `(#:phases
                  (modify-phases %standard-phases
                                 (delete 'check))))

           ))

(define-public python-bpython
  (package
    (name "python-bpython")
    (version "0.21")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bpython" version))
        (sha256
          (base32
            "11wlf12nm6ggn9512y4yqs26s8gdgpx0h9ls94k7fsjgjy4rpal8"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-curtsies" ,python-curtsies-0.3.5)
        ("python-cwcwidth" ,python-cwcwidth)
        ("python-greenlet" ,python-greenlet)
        ("python-pygments" ,python-pygments)
        ("python-pyxdg" ,python-pyxdg)
        ("python-requests" ,python-requests)))
    (home-page
      "https://www.bpython-interpreter.org/")
    (synopsis
      "Fancy Interface to the Python Interpreter")
    (description
      "Fancy Interface to the Python Interpreter")
    (license #f)))

(packages->manifest
 (list python-beginner-course python-bpython))
