(use-modules
 (beaver system)
 (gnu packages python)
 (gnu packages python-web))

(->
 (minimal-ovh "osef")
 (users "edouard")
 (groups "edouard")
 (http-static-content #:from-host "osef"
                      #:to-dir "/home/edouard/src/PythonBeginnerCourse/LandingPage"
                      #:nomkdir #t
                      #:raw-content '("location /forms/ {"
                                      "proxy_pass http://127.0.0.1:5000;"
                                      "}"))
 (os/mkdir-p "/var/lib/handsonpython"
             #:mode #o755
             #:owner "edouard"
             #:group "edouard")
 (packages python python-flask)
 (os/shepherd-service
  ;;  ;;"for i in $(seq 10); do echo $i; sleep 10 ; done"
  "FLASK_ENV=development FLASK_DEBUG=1 FLASK_APP=/home/edouard/src/PythonBeginnerCourse/LandingPage/backend.py  flask run --reload --debugger --host=0.0.0.0 --port=5000"
  "/home/edouard/src/PythonBeginnerCourse/LandingPage/")
  ;;(os/git)
 )
