(use-modules
 (beaver system))

(->
 (minimal-ovh "osef")
 (users "edouard")
 (http-static-content #:from-host "osef"
                      #:to-dir "/home/edouard/src/PythonBeginnerCourse/LandingPage"
                      #:nomkdir #t))
