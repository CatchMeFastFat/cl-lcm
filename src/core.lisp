
(in-package :lcm)

(defstruct subscription
  (msg-type (required))
  (handler (required))
  (csubscription (required)))

;; lcm structure
(defstruct (lcm* (:constructor %create-lcm))
	(pointer (required))
	(provider (required))
	(file-descriptor (required))
	(subscriptions (make-array 0 :adjustable t
                               :fill-pointer 0) :type (array subscription *)))



(defun good-p (lcm)
	(not (null-pointer-p (lcm*-pointer lcm))))

(defun create-lcm (&optional (provider (null-pointer)))
	(let ((ptr (lcm_create provider)))
		(if (null-pointer-p ptr)
        (error "lcm creation error"))
		(flet ((finaliser () (if (not (null-pointer-p ptr))
                             (lcm_destroy ptr))))
			(let ((result (%create-lcm :pointer ptr :provider provider
																 :file-descriptor (lcm_get_fileno ptr))))
				(tg:finalize result #'finaliser)
				result))))

(defun publish (lcm channel msg)
  (declare (type lcm* lcm))
  (declare (type string channel))
  ;; (declare (type structure-object msg))
  (etypecase msg
    (structure-object (publish lcm channel (encode msg)))
    ((array uint8 *) (let* ((ptr (lcm*-pointer lcm))
                            (channel (convert-to-foreign channel :string))
                            (n (length msg))
                            (data-len (convert-to-foreign n :uint))
                            (data (convert-to-foreign msg `(:array :uint8 ,n))))
                       (= 0 (lcm_publish ptr channel data data-len))))))

(defun file-descriptor(lcm)
  (declare (type lcm* lcm))
  (lcm*-file-descriptor (lcm*-pointer lcm)))

;; pass hander and msg-type as strings to avoid GC mess
(defcallback on-response :void ((rbuf* (:pointer (:struct _lcm_recv_buf_t)))
                                (c-channel :string)
                                (opts :string))
  (let* ((msg (make-array 0 :element-type 'uint8
                            :adjustable t
                            :fill-pointer 0))
         (channel (convert-from-foreign c-channel :string))
         (opt (split-by-spaces (convert-from-foreign opts :string)))
         (handler (read-from-string (first opt)))
         (msg-type (second opt)))
    (with-foreign-slots ((data data_size recv_utime lcm)
                         rbuf* (:struct _lcm_recv_buf_t))
      (loop for i from 0 below data_size do
        (vector-push-extend (mem-aref data :uint8 (- (1- data_size) i)) msg)))

    (if msg-type
        ;; raw data
        (funcall handler channel msg)
        ;; complex data
        (let ((st (funcall (read-from-string (concatenate 'string "make-" msg-type)))))
          (decode! st msg t)
          (funcall handler channel st)))))

(defun subscribe (lcm channel handler &optional (msg-type nil))
  (declare (type lcm* lcm))
  (declare (type string channel))
  (declare (type symbol handler))
  (declare (type symbol msg-type))
  (let* ((ptr (lcm*-pointer lcm))
         (channel (convert-to-foreign channel :string))
         (opts (convert-to-foreign (concatenate 'string (symbol-name handler)
                                                " " (if msg-type
                                                        (symbol-name msg-type)
                                                        ""))
                                   :string))
         (sub (make-subscription :msg-type msg-type :handler handler
                                 :csubscription (lcm_subscribe ptr channel (callback on-response) opts))))
    
    (vector-push-extend sub (lcm*-subscriptions lcm))))

(defun csubscriptions (lcm)
  (loop for i across (lcm*-subscriptions lcm)
        collect (slot-value i 'csubscription)))

;;;;TODO: unsubscribe
(defun unsubscribe (lcm csubscription)
  (declare (type lcm* lcm))
  (= 0 (lcm_unsubscribe (lcm*-pointer lcm) csubscription)))

(defun set-queue-capacity (csubscription capacity)
  (declare (type (integer 0 *) capacity))
  (= 0 (lcm_subscription_set_queue_capacity csubscription (convert-to-foreign capacity :int))))

;; (defun set-queue-capacity (csubscription capacity)
;;   (declare (type (integer 0 *) capacity))
;;   (= 0 (lcm_subscription_set_queue_capacity csubscription (convert-to-foreign capacity :int))))

;; (defun get_queue_size (csubscription)
;;   "returns size"
;;    )

(defun blocking-handle (lcm &optional (timeout/ms 0 timeout/ms-supplied-p))
  (declare (type lcm* lcm))
  (declare (type (integer 0 *) timeout/ms))
  (if timeout/ms-supplied-p
      (let ((r (lcm_handle_timeout (lcm*-pointer lcm) timeout/ms)))
        (if (= 0 r)
            nil
            (if (> 0)
                t
                (error "handler error"))))
      (= 0 (lcm_handle (lcm*-pointer lcm)))))


