;; (defpackage lcm/test
;;   (:use :cl
;;         :lcm
;;         :prove
;;         :cffi))
;; (in-package :lcm/test)
;; (define-foreign-library liblcm
;; 	(:linux "liblcm.so"))
;; (use-foreign-library liblcm)

;; ;; start here
;; (defvar my-lcm (lcm_create))

;; ;; supproted types
;; ;; :boolean
;; ;; :uint8
;; ;; :sint8
;; ;; :uint16
;; ;; :sint16
;; ;; :uint32
;; ;; :sint32
;; ;; :uint64
;; ;; :sint64
;; ;; :float32
;; ;; :float64
;; ;; :string
;; ;; +++++++++++ :array (size)
;; (lcm:deflcmstruct  example_t
;;   (timestamp :sint64)
;;   (position :float64 :array (3)) ;; '(size) or (size)
;;   (orientation :float64 :array '(4))
;;   (num-ranges :sint32)
;;   (ranges :float64 :array (num-ranges))
;;   (name :string)
;;   (enabled :boolean))

;; (defun generate-example-msg ()
;;   (make-example_t :timestamp 0
;;                   :position #(1d0 2d0 3d0)
;;                   :orientation #(1d0 0 0 0)
;;                   :num-ranges 5
;;                   :ranges (make-array 5 :initial-element 8.3d0)
;;                   :name "example string"
;;                   :enabled t))

;; (defvar my-msg (generate-example-msg))

;; (publish my-lcm "cl-test-channel" my-msg)


