(in-package :cl-user)
(defpackage exam.web
  (:use :cl
        :alexandria
        :caveman2
        :exam.config
        :exam.view
        :exam.challenges
        :exam.db
        :datafly
        :sxql
        :uuid
        :bordeaux-threads
        :access
   )
  (:export :*web* :reload-students))
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
  challenge ;; The current challenge
  answer ;; The current expected answer
  message ;; A special message to display (e.g. commentary on a previous wrong answer)
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

(defun reload-students ()
  "Re-read students from disk"
  (bordeaux-threads:with-lock-held (*lock*)
    (setf *students*
          (with-open-file (f #P"Students.sexp")
            (read f)))))

(defun by-id (id)
  "Return the exam state in *students* whose id matches id"
  (find-if (lambda (s) (string= (exam-id s) id)) *students*))

(defun by-token (token)
  "Return the exam state in *students* whose token matches token"
  (find-if (lambda (s) (string= (exam-token s) token)) *students*))

(defun save-states ()
  "Commit the state to disk"
  (with-open-file (f #P"Students.sexp"
                     :direction :output
                     :if-exists :rename-and-delete)
    (write *students* :stream f)))

(defvar *bullshit*
  (v:define-pipe ()
    (v:file-faucet :file #p"exam.log"))
  "I don't care about the contents of this variable,
I just don't want the code to be executed again on app reload")

(defun page (state)
  "Return the HTML page for the given state"
  (case (exam-state state)
    (:loggedout (render #P"login.html" `(:token ,(exam-token state))))
    (:simonsays (render #P"simonsays.html" `(:token ,(exam-token state)
                                            :grade ,(exam-grade state)
                                            :message ,(exam-message state)
                                            :challenge ,(exam-challenge state))))
    (:collatz   (render #P"collatz.html" `(:token ,(exam-token state)
                                           :grade ,(exam-grade state)
                                           :message ,(exam-message state)
                                           :challenge ,(exam-challenge state)
                                           :answer ,(exam-answer state))))
    (:data (render #P"data.html" `(:token ,(exam-token state)
                                   :grade ,(exam-grade state)
                                   :message ,(exam-message state)
                                   :challenge ,(exam-challenge state)
                                   :answer ,(exam-answer state))))
    (:training-bandit
     (render
                       #P"training-bandit.html"
                       `(:token ,(exam-token state)
                         :grade ,(exam-grade state)
                         :message ,(exam-message state)
                         :answer ,(exam-answer state)
                         :lead-nb ,(- 31 (bandit-hleft (exam-challenge state)))
                         :csv ,(bandit->csv (exam-challenge state))
                         )))
    (:bandit (render
              #P"bandit.html"
              `(:token ,(exam-token state)
                :grade ,(exam-grade state)
                :message ,(exam-message state)
                :answer ,(exam-answer state)
                :lead-nb ,(- 51 (bandit-hleft (car (exam-challenge state))))
                :csv ,(bandit->csv (car (exam-challenge state)))
                :benchmark ,(bandit-benchmark (car (exam-challenge state))))))
    (:sat (render
           #P"sat.html"
           `(:token ,(exam-token state)
             :grade ,(exam-grade state)
             :message ,(exam-message state)
             :quality-0 ,(elt (exam-challenge state) 0)
             :quality-1 ,(elt (exam-challenge state) 1)
             :quality-2 ,(elt (exam-challenge state) 2)
             :quality-3 ,(elt (exam-challenge state) 3))))
    (:gas (render
           #P"gas.html"
           `(:token ,(exam-token state)
             :grade ,(exam-grade state)
             :message ,(exam-message state))))
    (:end (render #P"end.html"
                  `(:grade ,(exam-grade state))))
    (otherwise
     (v:log :error :render "Invalid state ~A" (exam-state state))
     (throw-code 500))))

;;
;; State transitions

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
         (setf (exam-grade student) 0)
         (setf (exam-state student) :simonsays)
         (multiple-value-bind (challenge answer) (simonsays 1000)
           (setf (exam-challenge student) challenge)
           (setf (exam-answer student) answer))
         (setf (exam-message student) "Congratulations ! You successfully logged in.
DO NOT CLOSE THE TAB OR WINDOW AND DO NOT FIDDLE WITH THE BACK AND FORWARD BUTTON OF YOUR BROWSER OR YOU WON'T BE ABLE TO LOG BACK IN !")
         (save-states)
         (v:log :info :login "Successful login for :token ~A :name ~A :id ~A"
                token name id)
         student)))))

(defun validate-simonsays (answer token)
  "Check the answer and move to next challenge"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (answer (parse-integer answer :junk-allowed t)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :simonsays message)
           (error message)))
        ((string/= (exam-state student) :simonsays)  ;;Why the fuck would they post on /simonsays, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :simonsays message)
           (error message)))
        ((not answer) ;; Not even wrong
         (v:log :info :simonsays "Invalid answer ~A for :token ~A" answer token)
         (setf (exam-message student)
               (format nil
                       "Your given answer (~A) was not even wrong. The correct answer was ~A."
                       answer
                       (exam-answer student))))
        ((= (exam-answer student) answer) ;; Good answer !
         (setf (exam-message student)
               (format nil
                       "Congratulations ! Your given answer (~A) was right."
                       answer))
         (setf (exam-grade student) (+ 1 (exam-grade student)))
         (v:log :info :simonsays "Right answer for :token ~A" token))
        ((/= (exam-answer student) answer) ;; Wrong answer !
         (v:log :info :simonsays "Wrong answer for :token ~A" token)
         (setf (exam-message student)
               (format nil
                       "You given answer (~A) was wrong. The correct answer was ~A."
                       answer
                       (exam-answer student)))))
      ;; Generate a new problem
      (multiple-value-bind (challenge answer) (collatz)
        (setf (exam-challenge student) challenge)
        (setf (exam-answer student) answer))
      (setf (exam-state student) :collatz)
      (save-states)
      student)))


(defun validate-collatz (answer token)
  "Check the answer and either move to next challenge or back to another collatz"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (answer (parse-integer answer :junk-allowed t)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :collatz message)
           (error message)))
        ((string/= (exam-state student) :collatz)  ;;Why the fuck would they post on /collatz, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :collatz message)
           (error message)))
        ((not answer) ;; Not even wrong
         (setf (exam-message student)
               (format nil
                       "You given answer (~A) was not even wrong. The correct answer was ~A."
                       answer
                       (exam-answer student)))
         (v:log :info :collatz "Invalid answer ~A for :token ~A" answer token))
        ((= (exam-answer student) answer) ;; Good answer !
         (v:log :info :collatz "Right answer for :token ~A" token)
         (setf (exam-grade student) (+ 1 (exam-grade student)))
         (setf (exam-message student) "Congratulations ! Your answer was right."))
        ((/= (exam-answer student) answer) ;; Wrong answer !
         (v:log :info :collatz "Wrong answer for :token ~A" token)
         (setf (exam-message student)
               (format nil
                       "You given answer (~A) was wrong. The correct answer was ~A."
                       answer
                       (exam-answer student)))))
      ;; Generate a new problem
      (multiple-value-bind (challenge answer) (icecream 1000)
        (setf (exam-challenge student) challenge)
        (setf (exam-answer student) answer))
      (setf (exam-state student) :data)
      (save-states)
      student)))

(defun atof (s)
  "Parse a float from a string. Return nil if junk is given."
  (let ((*read-eval* nil))
    (with-input-from-string (stream s)
      (handler-case
          (float (read stream nil nil))
        (t (c)
          nil)))))

(defun within-ten-percent-of (target value)
  "Return t if value is withinf 10% of target"
  (cond
    ((>= value 0)
     (and
      (< value (* target 1.1))
      (> value (* target 0.9))))
    ((< value 0)
     (and
      (> value (* target 1.1))
      (< value (* target 0.9))))))

(defun validate-data (answer token)
  "Validate the answer to the data analysis challenge before moving on"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (answer (atof answer)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :data message)
           (error message)))
        ((string/= (exam-state student) :data)  ;;Why the fuck would they post on /data, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :data message)
           (error message)))
        ((not answer) ;; Not even wrong
         (v:log :info :data "Invalid answer ~A for :token ~A" answer token)
         (setf (exam-message student)
               (format nil
                       "You given answer (~A) was not even wrong. The correct answer was ~A. Try again"
                       answer
                       (exam-answer student))))
        ((within-ten-percent-of (exam-answer student) answer) ;; Good answer !
         (setf (exam-grade student) (+ 1 (exam-grade student)))
         (setf (exam-message student) "Congratulations ! Your answer was correct")
         (v:log :info :data "Right answer for :token ~A" token))
        (t ;; Wrong answer !
         (v:log :info :data "Wrong answer for :token ~A" token)
         (setf (exam-message student)
               (format nil
                       "You given answer (~A) was wrong. The correct answer was ~A."
                       answer
                       (exam-answer student)))))
      (multiple-value-bind (challenge answer) (sat)
        (setf (exam-state student) :sat)
        (setf (exam-challenge student) challenge)
        (setf (exam-answer student) answer))
      (save-states)
      student)))

(defun training-pull (arm token)
  "Pull the specified arm"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (arm (parse-integer arm :junk-allowed t)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :training-bandit message)
           (error message)))
        ((string/= (exam-state student) :training-bandit)  ;;Why the fuck would they post on /training-bandit, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :training-bandit message)
           (error message)))
        ((not arm) ;; arm was not an int, therefore it was "Go", and we move on to the actual bandit
         (setf (exam-state student) :bandit)
         (setf (exam-challenge student) `(,(exam-bandit 50) 0))
         (setf (exam-message student) "Good luck !")
         (save-states)
         (v:log :info :training-bandit ":token ~A moved on to the real bandit" token)
         student)
        (t ;; The answer is now an int, we just have to pull the arm
         (multiple-value-bind (challenge last-result) (pull-arm (exam-challenge student) arm)
           (setf (exam-challenge student) challenge)
           (setf (exam-message student)
                 (if (= last-result 1)
                     (format nil "The sales lead <strong>became</strong> a customer :)" )
                     (format nil "The sales lead <strong>refused</strong> to become a customer :'(")))
           (v:log :info :training-bandit ":token ~A pulled arm ~A with result ~A" token arm last-result))
         (when (= 0 (bandit-hleft (exam-challenge student)))
           ;; Exhausted all sales lead, give feedback and create a new training challenge
           (progn
            (setf (exam-message student)
                  (format nil "You turned ~A sales lead into customers.
To get any credit, you should aim for an average of ~A or more."
                          (bandit-reward (exam-challenge student))
                          (bandit-benchmark (exam-challenge student))))
            (setf (exam-challenge student) (exam-training-bandit 30))))
         (save-states)
         student)))))

(defun bandit-grade (reward benchmark)
  "Return some credit depending on the student performance"
  (cond
    ((> reward benchmark) (/ reward benchmark))
    (t 0)))

(defun actual-pull (arm token)
  "Pull the specified arm"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (arm (parse-integer arm :junk-allowed t)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :bandit message)
           (error message)))
        ((string/= (exam-state student) :bandit)  ;;Why the fuck would they post on /bandit, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :bandit message)
           (error message)))
        ((not arm)
         (let ((message (format nil "Arm was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :bandit message)
           (error message)))
        (t ;; The answer is now an int, we just have to pull the arm
         (multiple-value-bind (challenge last-result) (pull-arm (car (exam-challenge student)) arm)
           (setf (car (exam-challenge student)) challenge)
           (setf (exam-message student)
                 (if (= last-result 1)
                     (format nil "The sales lead <strong>became</strong> a customer :)" )
                     (format nil "The sales lead <strong>refused</strong> to become a customer :'(")))
           (v:log :info :bandit ":token ~A pulled arm ~A with result ~A" token arm last-result))
         (when (= 0 (bandit-hleft (car (exam-challenge student))))
           ;; Exhausted all sales lead, give feedback and create a new training challenge
           ;; if there are any trials left
           (progn
            (setf (exam-message student)
                  (format nil "You turned ~A sales lead into customers.
The goal is ~A or more."
                          (bandit-reward (car (exam-challenge student)))
                          (bandit-benchmark (car (exam-challenge student)))))
            (setf (exam-grade student) (+ (exam-grade student) (bandit-grade
                                                                (bandit-reward (car (exam-challenge student)))
                                                                (bandit-benchmark (car (exam-challenge student))))))
            (v:log :info :bandit ":token ~A finished a trial" token)
            (let ((trials-left (cadr (exam-challenge student))))
              (when (= 0 trials-left)
                (v:log :info :bandit ":token ~A finished all their trials" token)
                (setf (exam-state student) :end)))))))
      (save-states)
      student)))

(defun aget (l k)
  "Return the value associated with l in alist l whose keys are strings"
  (car (alexandria:assoc-value l k :test 'equal)))

(defun validate-sat (happy rich fun smart token)
  "Validate the answer to the sat challenge before moving on"
  (with-lock-held (*lock*)
    (let ((student (by-token token)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :sat message)
           (error message)))
        ((string/= (exam-state student) :sat)  ;;Why the fuck would they post on /sat, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :sat message)
           (error message)))
        ((and
          (equal happy (aget (exam-answer student) "happy"))
          (equal rich (aget (exam-answer student) "rich"))
          (equal fun (aget (exam-answer student) "fun"))
          (equal smart (aget (exam-answer student) "smart"))) ;; Good answer !
         (setf (exam-grade student) (+ 3 (exam-grade student)))
         (setf (exam-message student) "Congratulations ! Your answer was correct")
         (v:log :info :sat "Right answer for :token ~A" token))
        (t ;; Wrong answer !
         (v:log :info :sat "Received value for happy ~A" happy)
         (v:log :info :sat "Received value for rich ~A" rich)
         (v:log :info :sat "Received value for fun ~A" fun)
         (v:log :info :sat "Received value for smart ~A" smart)
         (v:log :info :sat "Expected value for happy ~A" (aget (exam-answer student) "happy"))
         (v:log :info :sat "Expected value for rich ~A" (aget (exam-answer student) "rich"))
         (v:log :info :sat "Expected value for fun ~A" (aget (exam-answer student) "fun"))
         (v:log :info :sat "Expected value for smart ~A" (aget (exam-answer student) "smart"))
         (v:log :info :sat "Wrong answer for :token ~A" token)
         (setf (exam-message student)
               "You given answer was wrong.")))
      (setf (exam-state student) :gas)
      (save-states)
      student)))

(defun validate-gas (salt gas token)
  "Validate the answer to the lp challenge before moving on"
  (with-lock-held (*lock*)
    (let ((student (by-token token))
          (gas (parse-integer gas :junk-allowed t))
          (salt (parse-integer salt :junk-allowed t)))
      (cond
        ((not student) ;; There is no registered student with this token
         (let ((message
                 (format
                  nil
                  "Invalid creds (bad token) :token ~A"
                  token)))
           (v:log :warn :gas message)
           (error message)))
        ((string/= (exam-state student) :gas)  ;;Why the fuck would they post on /sat, then ?
         (let ((message (format nil "State was ~A for token ~A" (exam-state student) token)))
           (v:log :warn :gas message)
           (error message)))
        ((and
          (= salt 30)
          (= gas 20)) ;; Good answer !
         (setf (exam-grade student) (+ 1 (exam-grade student)))
         (setf (exam-message student) "Congratulations ! Your answer was correct")
         (v:log :info :gas "Right answer for :token ~A" token))
        (t ;; Wrong answer !
         (v:log :info :gas "Wrong answer for :token ~A" token)
         (setf (exam-message student)
               "You given answer was wrong.")))
      (setf (exam-state student) :training-bandit)
      (setf (exam-challenge student) (exam-training-bandit 30))
      (save-states)
      student)))

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

(defroute ("/simonsays" :method :POST) (&key |answer| |token|)
  "Check the answer to the simonsays challenge"
  (handler-case
      (page (validate-simonsays |answer| |token|))
    (t (c)
      (v:log :error :routes "Error when POST to /simonsays ~A" c)
      (throw-code 403))))

(defroute ("/collatz" :method :POST) (&key |answer| |token|)
  "Check the answer to the collatz challenge"
  (handler-case
      (page (validate-collatz |answer| |token|))
    (t (c)
      (v:log :error :routes "Error when POST to /collatz ~A" c)
      (throw-code 403))))

(defroute ("/data" :method :POST) (&key |answer| |token|)
  "Check the answer to the data analysis challenge"
  (handler-case
      (page (validate-data |answer| |token|))
    (t (c)
      (v:log :error :routes "Error when POST to /data ~A" c)
      (throw-code 403))))

(defroute ("/training-bandit" :method :POST) (&key |arm| |token|)
  "Pull the specified arm"
  (handler-case
      (page (training-pull |arm| |token|))
    (t (c)
      (v:log :error :routes "Error when POST to /training-bandit ~A" c)
      (throw-code 403))))

(defroute ("/bandit" :method :POST) (&key |arm| |token|)
  "Pull the specified arm"
  (handler-case
      (page (actual-pull |arm| |token|))
    (t (c)
      (v:log :error :routes "Error when POST to /bandit ~A" c)
      (throw-code 403))))

(defroute ("/sat" :method :POST) (&key |happy| |rich| |fun| |smart| |token|)
  "Check the given answer"
  (handler-case
      (page (validate-sat |happy| |rich| |fun| |smart| |token|))
    (t (c)
      (v:log :error :routes "ERROR when POST to /sat ~A" c)
      (throw-code 403))))

(defroute ("/gas" :method :POST) (&key |salt| |gas| |token|)
  "Check the given answer"
  (handler-case
      (page (validate-gas |salt| |gas| |token|))
    (t (c)
      (v:log :error :routes "ERROR when POST to /gas ~A" c)
      (throw-code 403))))

(defroute ("/test" :method :GET) ()
  "Heartbeat"
  (handler-case
      (progn
        (v:log :debug :debug "I'm alive 001")
        (print "Heartbeat")
        "OK")
    (t (c)
      (v:log :error :routes "Error when GET to /test ~A" c)
      (throw-code 403))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
