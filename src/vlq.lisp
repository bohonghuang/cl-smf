(in-package #:binstruct)

(defbinstruct %vlq-base128-be ()
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (satisfies (unsigned-byte 8) (lambda (byte) (plusp (ldb (byte 1 7) byte)))) (*)))
  (byte 0 :type (unsigned-byte 8)))

(defun %read-vlq (vlq)
  (loop :with value :of-type (unsigned-byte 64) := (%vlq-base128-be-byte vlq)
        :with bytes := (%vlq-base128-be-bytes vlq)
        :for i :of-type (mod 8) :from 1 :to (length bytes)
        :do (setf (ldb (byte 7 (* i 7)) value) (ldb (byte 7 0) (aref bytes (- (length bytes) i))))
        :finally (return value)))

(defbinstruct (vlq-base128-be (:type (unsigned-byte 64)) (:constructor progn) (:conc-name nil)) ()
  (values 0 :type (map %vlq-base128-be #'%read-vlq #'identity)))
