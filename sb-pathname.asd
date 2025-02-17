(defsystem "sb-pathname"
  :version "0.1.0"
  :description "Pathname Utilities for SBCL"
  :author "t-sin <shinichi.tanaka45@gmail.com>"
  :license "MIT"
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "sb-pathname/test"))))

(defsystem "sb-pathname/test"
  :author "t-sin <shinichi.tanaka45@gmail.com>"
  :license "MIT"
  :depends-on ("sb-pathname"
               "rove")
  :components ((:module "test"
                :components ((:file "basic")
                             (:file "filesystem"))))
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
