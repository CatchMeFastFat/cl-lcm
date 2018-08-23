#|
  This file is a part of lcm project.
  Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)
|#

(defpackage :lcm/test/system
  (:use :cl :asdf))

(in-package :lcm/test/system)

(defsystem "lcm-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Islam Omar"
  :license "MIT"
  :depends-on ("lcm"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "publish-test"))))
  :description "Test system for lcm"

  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove)
c)))
