(defsystem "trivial-sbcl-pathname"
  :version "0.1.0"
  :description "Utilities for SBCL's glob behaviors"
  :author "t-sin <shinichi.tanaka45@gmail.com>"
  :license "MIT"
  :components ((:file "trivial-sbcl-pathname"))
  :in-order-to ((test-op (test-op "trivial-sbcl-pathname/test"))))

(defsystem "trivial-sbcl-pathname/test"
  :author "t-sin <shinichi.tanaka45@gmail.com>"
  :license "MIT"
  :depends-on ("trivial-sbcl-pathname"
               "rove")
  :components ((:module "test"
                :components ((:file "tests"))))
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
