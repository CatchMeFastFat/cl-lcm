(cl:defpackage :lcm
  (:use :cl :cffi :ieee-floats :trivial-garbage :swap-bytes)
  ;; Basic types and constructors.
  (:export
   #:publish
   #:create-lcm
   #:subscribe
   #:required
   #:flush-table
   #:dimensions
   #:FINGERPRINT
   #:encode
   #:encode-field
   #:decode!
   #:decode-field
   #:csubscriptions
   #:unsubscribe
   #:set-queue-capacity
   #:blocking-handle
   #:deflcmstruct))
  
