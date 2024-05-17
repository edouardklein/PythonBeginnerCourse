(use-modules
 (beaver system)
 (gnu packages python)
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
 (os/shepherd-service
  "FLASK_ENV=development FLASK_DEBUG=1 FLASK_APP=/var/lib/handsonpython/backend.py  flask run --reload --debugger --host=0.0.0.0 --port=5000"
  "/var/lib/handsonpython")
 )
