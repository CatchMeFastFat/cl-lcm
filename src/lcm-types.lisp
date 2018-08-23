
(in-package :lcm)

;; store structs as follow:
;; struct-type ----> list of slots in order
;; struct-type slot "-type" ----> cons of (type array-p)
;; #(struct1 slot-cons1 struct2 slot-cons2 ... )
(defvar *lcm-hash-table* (make-hash-table :test 'equal))

(defun flush-table ()
  (setf *lcm-hash-table* (make-hash-table :test 'equal)))

;; lcm's built-in types. Note that unsigned types are not present
;; because there is no safe java implementation. Really, you don't
;; want to add unsigned types.
(defun primitive-types () (list :sint8 :sint16 :sint32 :sint64 :float32 :float64 :string :boolean :uint8))

;; which types can be legally used as const values?
(defun network-types () (list :sint8 :sint16 :sint32 :sint64 :float32 :float64 :uint8))

(defun primitive-type-p (data-type)
  "returns t when primitive or array of primitive"
  (declare (type symbol data-type))
  (member data-type (primitive-types)))

(defun lcm-type-name (data-type)
  "returns C names used for hashing"
  (declare (type keyword data-type))
	(case data-type
    (:sint8 "int8_t")
    (:sint16 "int16_t")
    (:sint32 "int32_t")
    (:sint64 "int64_t")
    (:float32 "float")
    (:float64  "double")
    (:string "string")
    (:boolean "boolean")
    (:uint8 "byte")
	  (t (string data-type))))

;; Hash computation
;; Port of https://github.com/ZeroCM/zcm/blob/e9c7bfc401ea15aa64291507da37d1163e5506c0/gen/ZCMGen.cpp#L73-L94
;; NOTE: actual LCM implementation: https://github.com/lcm-proj/lcm/blob/992959adfbda78a13a858a514636e7929f27ed16/lcmgen/lcmgen.c#L114-L132
;; uses an int64_t for v and thus relies on undefined behavior; see https://stackoverflow.com/a/4009922.
(defun sign-extended-right-shift (val nshift)
	(declare (type uint64 val nshift))
	(get-uint64
	 (if (= 0 (ash val -63))
			 (ash val (- nshift))
			 (logior (ash val (- nshift))
							 (lognot (- (ash 1 (- (the uint64 64) nshift))
													(the uint64 1)))))))


(defun %hash-update (v c)
	(declare (type uint64 v c))
	(get-uint64 (+ (logxor (ash v 8)
                         (sign-extended-right-shift v (the uint64 55)))
		             c)))

(defun hash-update (v c)
	"Hashing method depends on the type of 'c'"
	(declare (type uint64 v))
	(etypecase c
		(integer
		 (%hash-update v (the uint64 c)))
		(character
		 (%hash-update v (the uint64 (char-code c))))
		(string
		 (setf v (%hash-update v (the uint64 (length c))))
		 (loop for char across c do
       (setf v (%hash-update v
                             (the uint64 (char-code char)))))
		 (the uint64 v))
		(symbol
		 (hash-update v (string-downcase c)))))


(defun dimensions (struct-type slot)
  (declare (type symbol struct-type slot))
  (gethash (concatenate 'string (symbol-name struct-type) (symbol-name slot) "-dims") *lcm-hash-table*))

;; port of https://github.com/lcm-proj/lcm/blob/992959adfbda78a13a858a514636e7929f27ed16/lcmgen/lcmgen.c#L248-L282
(defun base-hash (struct)
  (declare (type symbol struct))
	(let ((v (the uint64 #x12345678)))
    (declare (type uint64 v))
		(loop for k in (struct-slots struct) do
      (setf v (hash-update v k))
      (let ((dims (dimensions struct k))
            (data-type (first (struct-slot-type struct k))))
        (declare (type list dims))
        (declare (type symbol data-type))
        (if (primitive-type-p data-type)
            (setf v (hash-update v
                                 (lcm-type-name
                                  (first (struct-slot-type struct k)))))) 
        (setf v (hash-update v (the uint64 (length dims))))
        (if dims
            (loop for (mode . dim-string) in dims do
              (setf v (hash-update v mode))
              (setf v (hash-update v dim-string))))))
		(get-uint64 v)))

(defun compute-hash (struct parents)
  (declare (type symbol struct))
  (declare (type list parents))
  (if (primitive-type-p struct)
      (the uint64 0)
	    (if (member struct parents)
			    (the uint64 0)
	        (let ((hash (base-hash struct)))
            (declare (type uint64 hash))
		        (loop for k in (struct-slots struct)
                  do (let ((slot-type (first (struct-slot-type struct k))))
                       (declare (type symbol slot-type))
                       (setf hash (get-uint64 (+ hash
                                                 (compute-hash slot-type
                                                               (append (list struct)
                                                                       parents)))))))
		        (get-uint64 (+ (ash hash 1) (logand (ash hash -63) 1)))))))

(defun fingerprint (struct-type)
  ;; hash value
  (declare (type symbol struct-type))
  (gethash (concatenate 'string (symbol-name struct-type) "-fingerprint") *lcm-hash-table*))

;; TODO
;;TODO: decode
(defun check-fingerprint (fp struct-type)
  (declare (type uint64 fp))
  (declare (type symbol struct-type))
  (if (= (fingerprint struct-type) fp)
      t
      nil))

;; Check that the array sizes of `x` match their corresponding size fields.
(defun struct-array-dimensions(st struct-type slot)
  "retruns array dimensions in lcm-struct"
  (declare (type symbol struct-type slot))
  (declare (type structure-object st))
  (loop for (mode . dim-string) in (dimensions struct-type slot)
        collect
        (if (=  mode 0)
            ;; LCM_CONS
            (parse-integer dim-string)
            ;; LCM_VAR
            (slot-value st (read-from-string dim-string)))))

(defun check-valid (struct)
  (declare (type structure-object struct))
  (let ((struct-type (type-of struct)))
    (loop for slot in (struct-slots struct-type) do
      (let ((arr-p (second (struct-slot-type struct-type slot))))
        (if arr-p
            (if (equal
                 (struct-array-dimensions struct struct-type slot)
                 (array-dimensions (slot-value struct slot)))
                t
                (return nil)))))))

;;decode 
(defun decode! (st byte-vec &optional (check-fp nil))
  (declare (type (vector uint8 *) byte-vec))
  (declare (type structure-object st))
  (let ((fp (decode-field :uint64 byte-vec))
        (st-type (type-of st)))
    (declare (type uint64 fp))
    (declare (type symbol st-type))
    ;; check for fingerprint
    (if check-fp
        (if (not (check-fingerprint fp st-type))
            (error "fingerprint mismatch")))
    (loop for slot in (struct-slots st-type)
          for (slot-type arr-p)= (struct-slot-type st-type slot) do
            (if arr-p
                (let ((arr-dims (struct-array-dimensions st st-type slot)))
                  (setf (slot-value st slot) (decode-field :array byte-vec
                                                           slot-type arr-dims)))
                (setf (slot-value st slot) (decode-field slot-type byte-vec ))))))

;; function decode!(x::LCMType, io::IO)
;;     checkfingerprint(io, typeof(x))
;;     decodefield!(x, io)
;; end
(defun arr-dims-to-total-size (arr-dims)
  (loop for i in arr-dims
        for j = i then (+ i j) finally (return j)))

(defun decode-field (value-type byte-vec &optional (arr-element-type value-type) arr-dims) 
  (declare (type (array (unsigned-byte 8) *) byte-vec))
  (declare (type keyword value-type arr-element-type))
  (declare (type list arr-dims))
  (ecase value-type
    (:boolean (if (= 0 (vector-pop byte-vec)) nil t))
    (:uint8 (read-byte-vector byte-vec :uint8))
    (:sint8 (ntoh (read-byte-vector byte-vec :uint8) :sint8))
    (:uint16 (ntoh (read-byte-vector byte-vec :uint16) :uint16))
    (:sint16 (ntoh (read-byte-vector byte-vec :uint16) :sint16))
    (:uint32 (ntoh (read-byte-vector byte-vec :uint32) :uint32))
    (:sint32 (ntoh (read-byte-vector byte-vec :uint32) :sint32))
    (:uint64 (ntoh (read-byte-vector byte-vec :uint64) :uint64))
    (:sint64 (ntoh (read-byte-vector byte-vec :uint64) :sint64))
    (:float32 (ntoh (read-byte-vector byte-vec :uint32) :float32))
    (:float64 (ntoh (read-byte-vector byte-vec :uint64) :float64))
    (:string
     (let* ((len (1- (ntoh (read-byte-vector byte-vec :uint32) :uint32))) ;; length + null terminator
            (str (make-string len)))
       (loop for i from 0 below len do
         (setf (aref str i) (code-char (read-byte-vector byte-vec :uint8))))
       (vector-pop byte-vec) ;; throw null 
       str))
    (:array
     (let ((arr (make-array arr-dims))
           (total-size (arr-dims-to-total-size arr-dims)))
       (loop for i from 0 below total-size do
         (setf (row-major-aref arr i)
               (decode-field arr-element-type byte-vec)))
       arr))))

;; # Encoding
;; """
;;     encode(io::IO, x::LCMType)

;; Write an LCM byte representation of `x` to `io`.
;; """
(defun encode (x)
  (declare (type structure-object x))
  (let ((v (make-array 0
                       :element-type 'uint8
                       :adjustable t
                       :fill-pointer 0))
        (x-type (type-of x)))
    (encode-field (fingerprint x-type) :uint64 v)
    (encode-field x x-type v)
    v))

(defun encode-field (x x-type byte-vec &optional arr-element-type)
  (declare (type (array (unsigned-byte 8) *) byte-vec))
  (declare (type symbol x-type arr-element-type))
  (case x-type
    (:boolean (vector-push-extend (if x #x01 #x00) byte-vec))
    (:uint8 (write-byte-vector (hton x x-type) byte-vec :uint8))
		(:sint8 (write-byte-vector (hton x x-type) byte-vec :uint8))
		(:uint16 (write-byte-vector (hton x x-type) byte-vec :uint16))
    (:sint16 (write-byte-vector (hton x x-type) byte-vec :uint16))
		(:uint32 (write-byte-vector (hton x x-type) byte-vec :uint32))
    (:sint32 (write-byte-vector (hton x x-type) byte-vec :uint32))
		(:uint64 (write-byte-vector (hton x x-type) byte-vec :uint64))
		(:sint64 (write-byte-vector (hton x x-type) byte-vec :uint64))
		(:float32 (write-byte-vector (hton x x-type) byte-vec :uint32))
		(:float64 (write-byte-vector (hton x x-type) byte-vec :uint64))
    (:string (write-byte-vector (hton (1+ (length x)) :uint32) byte-vec :uint32) ;; string length + null terminator 
     (loop for i across x do
       (vector-push-extend (char-code i) byte-vec))
     (vector-push-extend #x00 byte-vec))
    (:array (loop for i from 0 below (array-total-size x) do
      (encode-field (row-major-aref x i) arr-element-type byte-vec)))
    (t (loop for slot in (struct-slots x-type)
             for (m-type arr-p) = (struct-slot-type x-type slot) do
               (if arr-p
                   (encode-field (slot-value x slot) :array byte-vec m-type)
                   (encode-field (slot-value x slot) m-type byte-vec))))))

;; (defun lisp-types (lcm-type &optional arr-p)
;;   (case lcm-type
;;     (:boolean '(nil :type boolean)
;;     (:uint8 '(0 :type uint8)
;;     (:sint8 '(0 :type sint8)
;;     (:sint16 '(0 :type sint16)
;;     (:sint32 '(0 :type sint32)
;; 		(:sint64 '(0 :type sint64)
;; 		(:float32 '(0d0 :type single-float)
;; 		(:float64 '(0s0 :type double-float)
;;     (:string '("" :type string))
;;               (:array '((make-array 0 :element-type ) :type (array  *)

(defun lisp-type (slot-type &optional (array-p nil) (arr-size '()))
  ;; replace variable size array
  (loop for i in arr-size
        for j from 0 by 1 do
          (if (not (numberp i))
              (progn (setf arr-size '*)
                     (return))))
  (if array-p
      `(,(make-array (if (eql arr-size '*) 0 arr-size)
                     :element-type (first (last (lisp-type slot-type)))
                     :initial-element (first (lisp-type slot-type)))
        :type (array ,(first (last (lisp-type slot-type))) ,arr-size))
      (ecase slot-type
        (:boolean '(nil :type boolean))
        (:uint8 '(0 :type uint8))
        (:sint8 '(0 :type sint8))
        (:uint16 '(0 :type uint16))
        (:sint16 '(0 :type sint16))
        (:uint32 '(0 :type uint32))
        (:sint32 '(0 :type sint32))
        (:uint64 '(0 :type uint64))
        (:sint64 '(0 :type sint64))
        (:float32 '(0s0 :type single-float))
        (:float64 '(0d0 :type double-float))
        (:string '("" :type string)))))


(defmacro deflcmstruct (name &rest slots)
  "Define a struct
    all names assumed to be small
    arrays written like (name :type :array '(dims))
                      (my-array :sint8 :array '(1 my-daq 4 29 my-lazer 9))"
  ;; store struct-slots and their types in hash-table:
  (let ((slots-list nil))
    (mapcar (lambda (slot)
              (destructuring-bind (slot-name slot-type &rest slot-args &key (array nil) &allow-other-keys) slot
                (declare (ignore slot-args))
                ;; account for :array '(1 2 3) OR (1 2 3)
                (push slot-name slots-list)
                (setf (gethash (concatenate 'string (symbol-name name)
                                            (symbol-name slot-name) "-type")
                               *lcm-hash-table*)
                      (list slot-type (if array t nil)))))
            slots)
    (setf (gethash name *lcm-hash-table*) (reverse slots-list)))
  
  ;; array dimensions
  (mapcar (lambda (slot)
            (destructuring-bind
                (slot-name slot-type &rest slot-args &key (array nil) &allow-other-keys) slot
              (declare (ignore slot-args slot-type))
              (if array
                  (let ((dims-list nil))
                    ;; allow :array '( 1 3 ) or :arrray ( 1 2 )
                    (loop for i in (if (eq 'quote (first array))
                                       (first (cdr array))
                                       array)
                          collect
                          (if (typep i 'integer)
                              ;; LCM_CONST
                              ;;   MODE      dim-name
                              (push (cons 0 (write-to-string i)) dims-list)                                   
                              (if (and (typep i 'symbol)
                                       (search (list i)
                                               (gethash name *lcm-hash-table*)))
                                  ;; LCM_VAR
                                  ;;   MODE      dim-name
                                  (push (cons 1  (string-downcase (write-to-string i))) dims-list)
                                  (error "unvalid array dimension!!"))))
                    (if dims-list
                        (setf (gethash (concatenate 'string (symbol-name name)
                                                    (symbol-name slot-name) "-dims")
                                       *lcm-hash-table*)
                              (reverse dims-list)))))))
          slots)
  (setf (gethash (concatenate 'string
                              (symbol-name name)
                              "-fingerprint")
                 *lcm-hash-table*)
        (compute-hash name nil))
  
  ;; structure
  `(defstruct ,name
     ,@(mapcar (lambda (slot)
                 (destructuring-bind (slot-name slot-type &rest slot-args &key (array nil) &allow-other-keys) slot
                   ;; (declare (ignore slot-type))
                   (setf slot-args (remove array slot-args))
                   (setf slot-args (remove :array slot-args))
                   `(,slot-name ,@(if slot-args
                                      slot-args
                                      (lisp-type slot-type array
                                                 (if (eq 'quote (first array))
                                                     (first (cdr array))
                                                     array))))))
        slots)))



