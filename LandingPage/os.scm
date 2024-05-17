(use-modules
 (guix gexp)
 (beaver system)
 (beaver functional-services)
 (gnu services shepherd)
 (gnu packages python)
 (gnu packages bash)
 (gnu packages python-web))

(->
 (minimal-ovh "osef")
 (groups "handsonpython")
 (nobody-like-user "handsonpython")
 (http-static-content #:from-host "osef"
                      #:to-dir "/var/lib/handsonpython"
                      #:nomkdir #t
                      #:raw-content '("location /forms/ {"
                                      "proxy_pass http://127.0.0.1:5000;"
                                      "}"))
 ;; os/mkdir-p /var/lib/handsonpython
 (packages python python-flask)
 (nobody-like-user "handsonpython")
 (extend-service
  shepherd-root
  (list (shepherd-service
         (documentation (string-append "Run the handsonpython website forms"))
         (requirement '(user-homes networking))
         (provision (list 'handsonpython))
         (start #~(make-forkexec-constructor
                   (list #$(file-append
                            bash
                            "/bin/bash") "-c"
                            "FLASK_ENV=development FLASK_DEBUG=1 FLASK_APP=backend.py  flask run --reload --debugger --host=0.0.0.0 --port=5000")
                    #:user "handsonpython"
                    #:group "handsonpython"
                    #:directory "/var/lib/handsonpython"
                    #:log-file "/var/log/handsonpython.log"

                   ))
            (stop #~(make-kill-destructor)))))
 )
