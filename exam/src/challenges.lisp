(in-package :cl-user)
(defpackage exam.challenges
  (:use :cl
        :cl-csv
        :access
        :str
        :arrows
        :linear-programming
   )
  (:export :collatz :icecream :exam-training-bandit :bandit :simonsays :gas
   :exam-bandit :pull-arm :cheat-arm :bandit-hleft :bandit->csv :copypaste
           :bandit-benchmark :bandit-reward :sat :drawer :parse-drawer-solution :format-drawer-solution :seteql
           :parse-huge-drawer-solution :huge-drawer-validp))
(in-package :exam.challenges)

(defun collatz-next (n)
  "Return the next element in the Collatz sequence"
  (if (= 0 (mod n 2))
      (/ n 2)
      (+ (* 3 n) 1)))

(defun collatz-sequence (n)
  "Return the collatz-sequence starting at n"
  (loop for i = n then (collatz-next i)
        collect i
        while (/= i 1)))

(defun collatz-max (n)
  "Return the maximum number reached in the Collatz sequence starting at n"
  (reduce #'max (collatz-sequence n)))

(defun collatz ()
  "Return a random number and the maximum number of its Collatz sequence

Make sure first that they are different so no student can naively copy paste the question as the answer"
  (loop for n = (+ 10000000000000000000 (random 10000000000000000000000000000000000000000000))
        do
           (let ((m (collatz-max n)))
             (when (/= n m)
               (return (values n m))))))

(defun copypaste ()
  "Return some semi-obfuscated python code and the integer it evaluates to."
  (let ((n (random 1000000)))
    (values
     (format nil "import base64
exec(base64.decodebytes(b'dG90byA9IDB4\\n').decode()+'~X')" n)
     n)))

(defun parse-huge-drawer-solution (s)
  (map 'list #'(lambda (x) (parse-integer x :junk-allowed t))
       (uiop:split-string s :separator ",")))

(defun huge-drawer-validp (answer)
  "Check the answer to the hardcoded huge drawer problem"
  (= (loop for drawer in answer
           sum drawer into height
           when (not (member drawer '(21 12 9 6)))
             do (return 0)
           finally (return height))
     9999))

(defun drawer ()
  "Return a knapsack problem and its solution"
  (let ((problem (drawer-problem)))
    (values problem (apply #'drawer-solution problem))))

(defun drawer-problem ()
  "Return an instance of a knapsack problem"
  (list (+ 25 (random 100))  ; Height
        (remove-duplicates
         (loop repeat 5 collect (+ 5 (random 20)))) ; Available drawers
        (random 20))) ; Leeway

(defun combinations (xs k)
  "https://rosettacode.org/wiki/Combinations_with_repetitions#Common_Lisp"
  (let ((x (car xs)))
    (cond
      ((null xs) nil)
      ((= k 1) (mapcar #'list xs))
      (t (append (mapcar (lambda (ys) (cons x ys))
                         (combinations xs (1- k)))
                 (combinations (cdr xs) k))))))

(defun drawer-solution (total available-lengths leeway)
  "Return the solution to the knapsack problem"
  (loop for k = 1 then (1+ k)
        while (<= (* k (apply #'min available-lengths))
                  total)
        append (loop for c in (combinations available-lengths k)
                     when (let ((length (reduce '+ c)))
                            (and
                             (<= length total)
                             (>= length (- total leeway))))
                     collect (sort c #'<)) into answer
        finally (return answer)))

(defun parse-drawer-solution (s)
  "Return a list of list of ints from s, or nil if the result is unparseable"
  (handler-case
      (loop for line in (uiop:split-string s :separator (list #\newline))
            unless (= 0 (length line))
            collect (map 'list #'parse-integer (uiop:split-string line :separator ",")))
    (error (c)
      (values nil c))))

(defun format-drawer-solution (sol)
  "Return the given solution, correctly formatted"
  (with-output-to-string (s)
    (loop for line in sol
          do (format s "~{~A~^, ~}~%" line))))

(defun seteql (s1 s2 &key (test #'eql))
  "Set equality, not efficient, bur correct"
  (and (subsetp s1 s2 :test test)
       (subsetp s2 s1 :test test)))

(defun random-choice (l)
  "Return a random element of l"
  (nth (random (length l)) l))

(defvar *icecream-neighbourhoods* (list "The Shire" "Mordor" "Rivendell" "Moria"))
(defvar *icecream-salespersons* (list "Frodo" "Bilbo" "Sméagol" "Sam" "Pippin"))
(defvar *icecream-truck-sizes* (list "Hobbit-sized" "Human-sized" "Ballrog-sized"))

(defun icecream-noise (v)
  "Return a noisy value within 5% of v"
  (if (= 0. v)
      0
      (+ v
         (*
          (random-choice '(1 -1))
          (random (* (abs v) 0.05))))))

(defvar *icecream-netincome-funcs*
  `(
    ;,(lambda (tp n s r p ts) (icecream-noise (+ tp r p)))
    ;,(lambda (tp n s r p ts) (icecream-noise (+ tp r)))
    ;,(lambda (tp n s r p ts) (icecream-noise (+ tp p)))
    ;,(lambda (tp n s r p ts) (icecream-noise (+ r p)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- tp r)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- tp p)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- r p)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- r tp)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- p tp)))
    ;,(lambda (tp n s r p ts) (icecream-noise (- p r)))
    ;; Functions of two variables are too hard for my poor little students
    ;; With the pivot, the net income already depends on two variables
    ;; Let them get away with a monodimensional regression
    ,(lambda (tp n s r p ts) (icecream-noise (* 2 p)))
    ,(lambda (tp n s r p ts) (icecream-noise (* 2 r)))
    ,(lambda (tp n s r p ts) (icecream-noise (* 2 tp)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -2 p)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -2 r)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -2 tp)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -5 p)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -5 r)))
    ,(lambda (tp n s r p ts) (icecream-noise (* -5 tp)))
    ,(lambda (tp n s r p ts) (icecream-noise (* 5 p)))
    ,(lambda (tp n s r p ts) (icecream-noise (* 5 r)))
    ,(lambda (tp n s r p ts) (icecream-noise (* 5 tp)))
    )
  "Multiple ways of simulating the net income from the real valued predictors")

(defun icecream-pivot (col)
  "Return a function of the predictors which compute the outcome
in randomly chosen ways depending on the value of the predictor column col"
  (let ((index-func (cond
                      ((string= col "Neighbourhood")
                       (lambda (tp n s r p ts)
                         (position n *icecream-neighbourhoods*
                                   :test #'string=)))
                      ((string= col "Salesperson")
                       (lambda (tp n s r p ts)
                         (position s *icecream-salespersons*
                                   :test #'string=)))
                      ((string= col "Truck size")
                       (lambda (tp n s r p ts)
                         (position ts *icecream-truck-sizes*
                                   :test #'string=)))
                      (t (error col))))
        (net-income-funcs
          (loop repeat (apply #'max
                              (mapcar #'length
                                      `(,*icecream-salespersons* ,*icecream-neighbourhoods*
                                                                 ,*icecream-truck-sizes*)))
                collect (random-choice *icecream-netincome-funcs*))))
    #'(lambda (tp n s r p ts)
        (let* ((i (funcall index-func tp n s r p ts))
               (net-income-func (nth i net-income-funcs)))
          (funcall net-income-func tp n s r p ts)))))


(defun icecream-configuration ()
  "Return a randomly chosen alist of headers to functions

Calling the given function will return a random value for the corresponding header.

All fonctions are niladic except for the 'Net income' header which needs to know the values for the other headers."
  `("Temperature" ,(lambda () (random 40))
    "Neighbourhood" ,(lambda () (random-choice *icecream-neighbourhoods*))
    "Salesperson" ,(lambda () (random-choice *icecream-salespersons*))
    "Route length" ,(lambda () (random 50))
    "Price index" ,(lambda () (random 100))
    "Truck size" ,(lambda () (random-choice *icecream-truck-sizes*))
    "Net income" ,(icecream-pivot (random-choice '("Neighbourhood" "Salesperson" "Truck size")))))

(defvar *icecream-predictors*
  '("Temperature" "Neighbourhood" "Salesperson" "Route length" "Price index" "Truck size")
  "The 'input' columns of the data")

(defun icecream-predictors-sample (conf)
  "Return, as a list, one set of predictors sampled according to conf"
  (loop
    for p in *icecream-predictors*
    collect (funcall (access conf p))))

(defun icecream-sample (conf)
  "Return, as a list, a random sample of an icecream route, predictors and outcome"
  (let ((preds (icecream-predictors-sample conf)))
    (concatenate 'list preds `(,(apply (access conf "Net income") preds)))))

(defun icecream ()
  "Return 1000 randomly generated lines, and the answer, with some sanity checks."
  (loop with challenge
        with answer = 0
        do (multiple-value-bind (c a) (icecream-lines 1000)
             (setf challenge c)
             (setf answer a))
        while (< (abs answer) 10)
        finally (return (values challenge answer))))

(defun icecream-lines (n)
   "Return randomly generated icecream truck stats and the missing value"
  (let* ((conf (icecream-configuration))
         (last-preds (icecream-predictors-sample conf)))
    (values
     (with-output-to-string (answer)
       (cl-csv:write-csv-row (concatenate 'list *icecream-predictors* '("Net income"))
                             :stream answer)
       (cl-csv:write-csv
        (loop repeat n
              collect (icecream-sample conf))
        :stream answer)
       (cl-csv:write-csv-row (concatenate 'list last-preds '("NaN"))
                             :stream answer))
     (apply (access conf "Net income") last-preds))))

(defstruct bandit
  "A multi armed-bandit state machine"
  probs ;; The list of probabilities of the arms
  data ;; A list of lists of past trials (one list per arm)
  hleft ;; Horizon: number of trials left
  benchmark ;; The cumulative reward a non stupid agent
  ;; would have gotten with the starting horizon
  reward ;; The cumulative reward so far
  )

(defun bandit->csv (b)
  "Return the stats of b as a csv string"
  (with-output-to-string (csv)
    (cl-csv:write-csv-row '("Sales Pitch" "Lead became client")
                          :stream csv)
    (loop for arm in (bandit-data b)
          for i = 0 then (+ 1 i)
          do (loop for reward in arm
                   do (cl-csv:write-csv-row `(,i ,reward)
                                            :stream csv)))
    csv))
;;    `(,s
;;      ,(length (bandit-probs b))
;;      ,i
;;      (bandit-hleft b)
;;      reward
;;      (+ reward (bandit-reward b)))
;;    :stream nextline)))

(defun random-bandit (n h)
  "Return an initialized random n-armed bandit with initial horizon h"
  (make-bandit
   :probs (loop repeat n collect (random 1.0))
   :data (loop repeat n collect '())
   :hleft h
   :benchmark nil
   :reward 0))

(defun exam-training-bandit (h)
  "Return a hand-crafted bandit that serves the purpose of the exam well"
  (benchmark
   (make-bandit
    :probs (random-choice '(
                            (.1 .2 .4)
                            (.1 .4 .2)
                            (.2 .1 .4)
                            (.2 .4 .1)
                            (.4 .1 .2)
                            (.4 .2 .1)))
    :data (loop repeat 3 collect '())
    :hleft h
    :benchmark 0
    :reward 0)))

(defun exam-bandit (h)
  "Return a hand-crafted bandit that serves the purpose of the exam well"
  (benchmark
   (make-bandit
    :probs (random-choice '(
                            (.1 .2 .3 .4 .6)
                            (.1 .2 .3 .6 .4)
                            (.1 .2 .6 .4 .3)
                            (.1 .6 .3 .4 .2)
                            (.6 .2 .3 .4 .1)
                            ))
    :data (loop repeat 5 collect '())
    :hleft h
    :benchmark 0
    :reward 0)))

(defun pull-arm (b i)
  "Return the bandit b after having pulled the i-th arm."
  (when (<= (bandit-hleft b) 0)
    (return-from pull-arm nil))
  (let ((reward (if (< (random 1.0)
                       (nth i (bandit-probs b)))
                    1 0)))
    (values
     (make-bandit
      :probs (bandit-probs b)
      :data  (let ((data (copy-tree (bandit-data b))))
               (setf (nth i data) (cons reward (nth i data)))
               data)
      :hleft (- (bandit-hleft b) 1)
      :benchmark (bandit-benchmark b)
      :reward (+ reward (bandit-reward b)))
     reward)))


(defun cheat-arm (b i)
  "Return the bandit b after having pulled the i-th arm.
But for the first pull of each arm,
don't be random, and draw a 1 on the best arm and a 0 on the others
in the hope of avoiding the worst case for simple strategies such as
epsilon greedy or k-then-greedy."
  (when (<= (bandit-hleft b) 0)
    (return-from cheat-arm nil))
  (if (null (nth i (bandit-data b)))
      (let ((reward (if (= .9 (nth i (bandit-probs b)))
                        1 0)))
        (values
         (make-bandit
          :probs (bandit-probs b)
          :data  (let ((data (copy-tree (bandit-data b))))
                   (setf (nth i data) (cons reward (nth i data)))
                   data)
          :hleft (- (bandit-hleft b) 1)
          :benchmark (bandit-benchmark b)
          :reward (+ reward (bandit-reward b)))
         reward))
      (pull-arm b i)))

(defun oracle (b)
  "Return the best action by 'cheating'"
  (let* ((best-prob (apply #'max (bandit-probs b))))
    (loop for p in (bandit-probs b)
          for i = 0 then (+ 1 i)
          when (= best-prob p)
            return i)))

(defun random-mab-strategy (b)
  "Return an arm number at random"
  (random (length (bandit-probs b))))

;; http://www.gigamonkeys.com/book/loop-for-black-belts.html

(defun mean (l)
  "Return the mean of l, or .5 if l is empty"
  (loop for x in l
        sum x into s
        count t into n
        finally (return (if (/= 0 n)
                            (/ s n)
                            .5))))

(defun greedy (b)
  "Choose from the most promising empirical mean,
break equality at random, assume a prior of 0.5"
  (let* ((means (mapcar #'mean (bandit-data b)))
         (best-mean (apply #'max means))
         (best-indices (loop for m in means
                             for i = 0 then (+ 1 i)
                             when (= m best-mean)
                             collect i)))
    (random-choice best-indices)))

(defun epsilon-greedy (epsilon)
  "Return the epsilon greedy strategy for the given epsilon"
  (lambda (b)
    "Choose among the most promising arm with prob 1-epsilon
and at random with prob epsilon"
    (if (< (random 1.0) epsilon)
        (random-mab-strategy b)
        (greedy b))))

(defun k-then-greedy (k)
  "Return the strategy that will play each arm k times,
then chose the best one"
  (lambda (b)
    "Play each arm k times then chose the best one"
    (loop for l in (bandit-data b)
          for i = 0 then (+ 1 i)
          when (< (length l) k)
            return i
          finally (return (greedy b)))))

(defun bayes-p (e p)
  "Return p(e|p), the probability to see the samples e assuming that
the bernoulli process behind them has probability p"
  (let ((nb-pos (loop for s in e
                      count (= s 1)))
        (nb-neg (loop for s in e
                      count (= s 0))))
    (* (expt p nb-pos) (expt (- 1 p) nb-neg))))

(defun optimistic-p (e &optional (alpha .05) (prec .01))
  "Return, within prec, the highest p for which p(e|p) is
bigger than alpha.
If alpha is too high, or prec is too high to find such a p,
the empirical mean of e is returned. This assumes that e is now
big enough to provide an accurate estimation."
  (loop for p from 1 downto 0 by prec
        when (>= (bayes-p e p) alpha)
          return p
        finally ;; Should not happen,
                ;; either alpha is too high, or prec is too high
                ;; either way return the empirical mean
                (return (mean e))))

(defun optimistic-greedy (b)
  "Choose the arm with the highest p so that p(e|p) > alpha=.05"
  (let* ((opt-p (mapcar #'optimistic-p (bandit-data b)))
         (best-p (apply #'max opt-p))
         (best-indices (loop for p in opt-p
                             for i = 0 then (+ 1 i)
                             when (= p best-p)
                               collect i)))
    (random-choice best-indices)))

(defun play-out (start strat &optional (pull-arm #'pull-arm))
  "Return the final state after playing strat from start"
  (loop for b = start then (funcall pull-arm b (funcall strat b))
        while b
        collect b into states
        finally (return (first (last states)))))

(defun benchmark (b)
  "Return the bandit with the benckmark field filled.
From our empirical testing with h=n*10, and n=3 or n=5,
we see that all the simple strategies can somewhat reliably reach
80% of the expected performance of the oracle."
  (make-bandit
   :probs (bandit-probs b)
   :data  (bandit-data b)
   :hleft (bandit-hleft b)
   :benchmark (* 0.8 (bandit-hleft b) (apply #'max (bandit-probs b)))
   :reward (bandit-reward b)))

;; (with-open-file
;;     (f #P"mab-exam-cheat.csv" :direction :output
;;                    :if-exists :supersede)
;;   (cl-csv:write-csv-row '("Strategy" "Reward" "oracle" "Horizon")
;;                         :stream f)
;;   (loop repeat 1
;;         do (let* ((start (exam-bandit 50))
;;                   (oracle (apply #'max (bandit-probs start))))
;;              (loop repeat 1000
;;                    do (loop for strat in `(
;;                                            ,#'oracle
;;                                            ,#'optimistic-greedy
;;                                            ,(k-then-greedy 1)
;;                                          ;  ,(k-then-greedy 2)
;;                                            ,(k-then-greedy 3)
;;                                           ; ,(k-then-greedy 4)
;;                                            ,(k-then-greedy 5)
;;                                            greedy
;;                                            ,(epsilon-greedy .05)
;;                                            ;,(epsilon-greedy .1)
;;                                            ,(epsilon-greedy .25)
;;                                            ;,(epsilon-greedy .5)
;;                                            random-mab-strategy
;;                                            )
;;                             for name in '(
;;                                           "oracle"
;;                                           "optgreedy"
;;                                           "1 then greedy"
;;                                          ; "2 then greedy"
;;                                           "3 then greedy"
;;                                          ; "4 then greedy"
;;                                           "5 then greedy"
;;                                           "greedy"
;;                                           ".05-greedy"
;;                                          ; ".1-greedy"
;;                                           ".25-greedy"
;;                                           ;".5-greedy"
;;                                           "random")
;;                             do (let ((end (play-out start strat #'cheat-arm)))
;;                                  (cl-csv:write-csv-row
;;                                   `(,name
;;                                     ,(bandit-reward end)
;;                                     ,oracle
;;                                     ,(bandit-hleft start))
;;                                   :stream f)))))))

(defun simonsays (n)
  (let ((c (simonsays-challenge n)))
    (values c (solve-simonsays c))))

(defun simonsays-challenge (n)
  "Return a 'Simon says' challenge"
  ;; FIXME: Prevent to high a number to avoid running into floating point precision problems
  ;; FIXME: Put the "rouding down" part in bold, or accept +-1 answers.
  ;; FIXME: Also, insist on n=1 at the beginning
  ;; FIXME: Make it clear that numbers can be negative or just remove them altogether
  (format nil "~{~A~%~}"
          (loop repeat n
                collect (concatenate 'string
                                     (random-choice
                                      '("Simon " "Mary " "Sue " "" "Yo momma "))
                                     (random-choice
                                      '("says " "" "shouts " "asks " "demands "))
                                     (random-choice '("+ " "/ " "* " "- "))
                                     (random-choice '("-" ""))
                                     (format nil "~A" (random 1000))))))

(defun solve-simonsays (s)
  "Return the solution to the given simonsays string"
  (let ((ANSWER 1))
    (-<> s
         (split-sequence:split-sequence #\newline <>)
         (loop for l in <>
               when (starts-with? "Simon says" l)
                 collect (read-from-string (format nil "(~A)" (subseq l 11))))
         (loop for e in <>
               collect `(lambda (x) (,(car e) x ,(cadr e))))
         (loop for e in <>
               do (setf ANSWER (funcall (eval e) ANSWER))))
    (floor ANSWER)))

(defun all-permutations (list)
  "https://stackoverflow.com/a/2087771"
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun sat ()
  "Return a permutation of qualities, and the corresponding map from them to t, f, or nil"
  (let ((qualities (random-choice (all-permutations
                                   '("happy" "rich" "fun" "smart")))))
    (values
     qualities
     `((,(elt qualities 0) "unknown")
       (,(elt qualities 1) "all")
       (,(elt qualities 2) "none")
       (,(elt qualities 3) "all")))))

(defun gas ()
  "Return the amount of available precursors, the price of gas and salt,
and how much of each should be produced.
We loop until the randomly chosen values for the inputs yield a non trivial
answer"
  (loop with gas-price
        with salt-price
        with cl
        with h
        with n
        with gas = 0
        with salt = 0
        do (let* ((_gas-price (random 20))
                  (_salt-price (random 20))
                  (_cl (random 80))
                  (_h (random 360))
                  (_n (random 50))
                  (problem (parse-linear-problem
                            `(max (= pognon (+ (* ,_gas-price gas) (* ,_salt-price salt))))
                            `((<= gas n-gas)
                              (<= (* 3 gas) h-gas)
                              (<= salt n-salt)
                              (<= (* 4 salt) h-salt)
                              (<= salt cl-salt)
                              (<= cl-salt ,_cl)
                              (<= (+ h-salt h-gas) ,_h)
                              (<= (+ n-salt n-gas) ,_n))))
                  (solution (solve-problem problem)))
             (setf gas-price _gas-price)
             (setf salt-price _salt-price)
             (setf cl _cl)
             (setf h _h)
             (setf n _n)
             (setf gas (solution-variable solution 'gas))
             (setf salt (solution-variable solution 'salt)))
        while (or (zerop gas) (zerop salt))
        finally (return (values (list :gas-price gas-price
                                      :salt-price salt-price
                                      :cl cl
                                      :n n
                                      :h h)
                                (list :gas gas
                                      :salt salt)))))
