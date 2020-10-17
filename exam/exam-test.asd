(defsystem "exam-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("exam"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "exam"))))
  :description "Test system for exam"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
