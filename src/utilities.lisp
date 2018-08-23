;; macros
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities.lisp

;;;; Basic utility functions and macros, used throughout the code.

(in-package :lcm)

;; useful types
(deftype uint8	() '(UNSIGNED-BYTE 8))
(deftype sint8	() '(SIGNED-BYTE 8))    ;
(deftype uint16	() '(UNSIGNED-BYTE 16))
(deftype sint16	() '(SIGNED-BYTE 16))
(deftype uint32	() '(UNSIGNED-BYTE 32))
(deftype sint32	() '(SIGNED-BYTE 32))
(deftype uint64	() '(UNSIGNED-BYTE 64))
(deftype sint64	() '(SIGNED-BYTE 64))

(defun required (&optional (msg "A required argument is missing.") &rest args)
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (apply #'error msg args))

;; from DBUS-lisp
(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given
size."
  (declare (type integer value size))
  (ldb (byte size 0) value))

(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given
size."
  (declare (type integer value size))
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defun hton (x x-type)
  (declare (type real x))
  (declare (type keyword x-type))
  (ecase x-type
    (:uint8 (the uint8 x))
    (:sint8 (the uint8 (signed-to-unsigned x 8)))
    (:uint16 (the uint16 (htons x)))
    (:sint16 (the uint16 (htons (signed-to-unsigned x 16))))
    (:uint32 (the uint32 (htonl x)))
    (:sint32 (the uint32 (htonl (signed-to-unsigned x 32))))
    (:uint64 (the uint64 (htonq x)))
    (:sint64 (the uint64 (htonq (signed-to-unsigned x 64))))
    (:float32 (the uint32 (htonl (encode-float32 x))))
    (:float64 (the uint64 (htonq (encode-float64 x))))))

(defun ntoh (x x-type)
  (declare (type real x))
  (declare (type keyword x-type))
  (ecase x-type
    (:uint8  (the uint8 x))
    (:sint8  (the sint8 (unsigned-to-signed x 8)))
    (:uint16  (the uint16 (ntohs x)))
    (:sint16  (the sint16 (unsigned-to-signed (ntohs x) 16)))
    (:uint32  (the uint32 (ntohl x)))
    (:sint32  (the sint32 (unsigned-to-signed (ntohl x) 32)))
    (:uint64  (the uint64 (ntohq x)))
    (:sint64  (the sint64 (unsigned-to-signed (ntohq x) 64)))
    (:float32  (the single-float (decode-float32 (ntohl x))))
    (:float64  (the double-float (decode-float64 (ntohq x))))))

(defun write-byte-vector (x octet-vec x-type)
  (declare (type (array uint8 *) octet-vec))
  (declare (type (unsigned-byte *) x))
  (declare (type keyword x-type))
  (let ((size (ecase x-type
                (:uint8 8)
                (:uint16 16)
                (:uint32 32)
                (:uint64 64))))
    (loop for i from 0 below size by 8 do
      (vector-push-extend (ldb (byte 8 i) x) octet-vec))))

(defun read-byte-vector (rev-octet-vec x-type)
  " vector is reversed"
  (declare (type (array uint8 *) rev-octet-vec))
  (declare (type keyword x-type))
  (let ((size (ecase x-type
                (:uint8 8)
                (:uint16 16)
                (:uint32 32)
                (:uint64 64)))
        (value 0))
    (declare (type (unsigned-byte *) value size))
    (loop for i from 0 below size by 8 do
      (setf value (dpb (vector-pop rev-octet-vec) (byte 8 i) value)))
    value))

(defun get-uint64 (n)
  (declare (type rational n))
  (the uint64 (ldb (byte 64 0) n)))

;; (defun struct-members (st)
;;   (declare (type structure-object st))  
;;   (mapcar (lambda (x) (slot-value st x))
;;           (struct-slots (type-of st))))

;; struct name in h-t gives struct slots
(defun struct-slots (struct-type)
  (declare (type symbol struct-type))
  (gethash  struct-type *lcm-hash-table*))

;; struct type from hash-table
(defun struct-slot-type (struct-type slot)
  (declare (type symbol struct-type slot))
  (let ((key (concatenate 'string (symbol-name struct-type)
                          (symbol-name slot) "-type")))
    (gethash  key *lcm-hash-table*)))


(defun split-by-spaces (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (declare (type string string))
  (remove ""
          (loop for i = 0 then (1+ j)
                as j = (position #\Space string :start i)
                collect (subseq string i j)
                while j) :test #'equal))
