(defpackage lcm/test
  (:use :cl
        :lcm
        :prove
        :cffi))
(in-package :lcm/test)

(define-foreign-library liblcm
  (:darwin (:or "liblcm.3.dylib" "liblcm.dylib"))
  (:unix (:or "liblcm.so.3" "liblcm.so"))
  (t (:default "liblcm")))

(use-foreign-library liblcm)
(plan 1)

;; supported types
;; :boolean
;; :uint8
;; :sint8
;; :uint16
;; :sint16
;; :uint32
;; :sint32
;; :uint64
;; :sint64
;; :float32
;; :float64
;; :string
;; +++++++++++ :array (size)
(flush-table)
(deflcmstruct  example_t
    (timestamp :sint64)
  (position :float64 :array (3)) ;; '(size) or (size)
  (orientation :float64 :array '(4))
  (num_ranges :sint32)
  (ranges :float64 :array (num_ranges))
  (name :string)
  (enabled :boolean))

(defun generate-example-msg ()
  (make-example_t :timestamp 0
                  :position (make-array 3 :initial-contents #(1d0 2d0 3d0)
                                          :element-type 'double-float)
                  :orientation (make-array 4 :initial-contents (vector 1d0 0d0 0d0 0d0)
                                             :element-type 'double-float)
                  :num_ranges 5
                  :ranges (make-array 5 :initial-element 8.3d0
                                        :element-type 'double-float)
                  :name "example string"
                  :enabled t))

;; start here
(defun test ()
  (let ((my-lcm (create-lcm))      
        (my-msg (generate-example-msg)))
    (publish my-lcm "cl-test-channel" my-msg)))

(ok (test))

(close-foreign-library 'liblcm)

(finalize)

