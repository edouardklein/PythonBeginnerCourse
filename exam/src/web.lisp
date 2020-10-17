(in-package :cl-user)
(defpackage exam.web
  (:use :cl
        :caveman2
        :exam.config
        :exam.view
        :exam.db
        :datafly
        :sxql
        :uuid
        :bordeaux-threads
        :access
   )
  (:export :*web*))
(in-package :exam.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; The exam state machine
(defstruct exam
  "The exam state machine."
  state ;; The state machine's state name
  id ;; The student's id
  token  ;; A random token to prevent multiple logins
  name  ;; The student's name
  grade ;; The student's grade
  )

;;
;; Out global data structure
(defvar *lock* (bordeaux-threads:make-lock)
  "A lock to avoid concurrent access to the *students* list")
(defvar *students*
  (bordeaux-threads:with-lock-held (*lock*)
    (with-open-file (f #P"Students.sexp")
      (read f)))
  "The global list of exam state machines

Each student is a different instance of the same machine

Use the helper functions to access the data,
so that it will sync to disk on edit.

Use the lock to avoid concurrent access.")

(defun by-id (id)
  "Return the exam state in *students* whose id matches id"
  (find-if (lambda (s) (string= (exam-id s) id)) *students*))

(defun save-states ()
  "Commit the state to disk"
  (with-open-file (f #P"Students.sexp"
                     :direction :output
                     :if-exists :overwrite)
    (write *students* :stream f)))

(v:define-pipe ()
  (v:file-faucet :file #p"exam.log"))


(defun page (state)
  "Return the HTML page for the given state"
  (case (exam-state state)
    (:loggedout (render #P"login.html" `(:token ,(exam-token state))))
    (:collatz   (render #P"collatz.html" `(:token ,(exam-token state)
                                           :grade ,(exam-grade state))))
    (otherwise
     (v:log :error :render "Invalid state ~A" (exam-state state))
     (throw-code 500))))

(defun login (token name id)
  "Check credentials and register the user as having logged in"
  (with-lock-held (*lock*)
    (let ((student (by-id id)))
      (cond
        ((not student) ;; There is no registered student with this ID
         (let ((message
                 (format
                  nil
                  "Invalid login (bad id) :token ~A :name ~A :id ~A"
                  token name id)))
           (v:log :warn :login message)
           (error message)))
        ((exam-token student) ;; The student has already logged in
         (let ((message
                 (format
                  nil
                  "Invalid login (already logged in) :existing-token ~A :token ~A :name ~A :id ~A"
                  (exam-token student) token name id)))
           (v:log :warn :login message)
           (error message)))
        ;; At this point, student is non nil and (exam-token student) is nil
        ;; This means that the login attempt is valid and should cause an update of *students*
        (t
         (setf (exam-token student) token)
         (setf (exam-name student) name)
         (setf (exam-grade student) :E)
         (setf (exam-state student) :collatz)
         (save-states)
         (v:log :info :login "Successful login for :token ~A :name ~A :id ~A"
                token name id)
         student)))))
;;
;; Routing rules

(defroute ("/" :method :GET) ()
  "Getting the main page lands us in the start state"
  (let ((state (make-exam :state :loggedout :token (make-v4-uuid))))
    (page state)))

(defroute ("/" :method :POST) (&key |token| |name| |id|)
  "Try to log in"
  (handler-case
        (page (login |token| |name| |id|))
    (t (c)
      (v:log :error :routes "Error when POST to / ~A" c)
      (throw-code 403))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
