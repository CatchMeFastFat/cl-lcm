#|
  This file is a part of cl-lcm project.
  Copyright (c) 2018 Islam Omar (catchmefastfat@gmail.com)
|#

#|
  Lightweight Communications and Marshalling

  Author: Islam Omar (io1131@fayoum.edu.eg)
|#

(defpackage :lcm/system
  (:use :cl :asdf))

(in-package :lcm/system)

(defsystem "lcm"
  :version "1.0"
  :author "Islam Omar"
  :license "MIT"
  :depends-on (:cffi :trivial-garbage :ieee-floats :swap-bytes)
  :components ((:file "package")
               (:module "src"
                        :components ((:file "lcm")
                                     (:file "utilities")
                                     (:file "lcm-types")
                                     (:file "core"))))
  :description "Lightweight Communications and Marshalling"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op "lcm-test"))))
