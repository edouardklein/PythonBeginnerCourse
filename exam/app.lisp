(defpackage exam.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :exam.web
                :*web*)
  (:import-from :exam.config
                :config
                :productionp
                :*static-directory*))
(in-package :exam.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     ;;FIXME guix :accesslog
     nil)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 ;; FIXME guix :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
