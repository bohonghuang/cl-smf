(defpackage cl-smf
  (:use #:cl #:alexandria #:binstruct)
  (:nicknames #:smf)
  (:export
   #:read-smf
   #:write-smf))

(in-package #:smf)

(defbinstruct smf-event ())

(defmacro define-smf-event ((name status) &body fields)
  (destructuring-bind (name &rest options) (ensure-list name)
    (let ((name (alexandria:symbolicate '#:smf-event- name)))
      `(defbinstruct (,name (:endian :big) . ,options) ()
         (nil ,status :type (satisfies (unsigned-byte 8) (curry #'eql ,status)))
         . ,fields))))

;;;;;;;;;;;;;;;;;;;;
;; Channel events ;;
;;;;;;;;;;;;;;;;;;;;

(defbinstruct (smf-channel-event (:include smf-event)) ())

(declaim (inline smf-status-channel))
(defun smf-status-channel (status)
  (ldb (byte 4 0) status))

(defmacro define-smf-channel-event ((name status) &body fields)
  (let ((struct-name (symbolicate '#:smf-event- name))
        (status-channel 'smf-status-channel)
        (channel-status (symbolicate 'smf-channel-status '/ name)))
    `(progn
       (declaim (inline ,channel-status))
       (defun ,channel-status (channel)
         (dpb channel (byte 4 0) ,status))
       (defbinstruct (,struct-name (:include smf-channel-event) (:endian :big)) ()
         (channel ,status :type (map (or . ,(loop :for i :from #x00 :to #x0F
                                                  :collect `(satisfies (unsigned-byte 8) (curry #'eql ,(+ status i)))))
                                     #',status-channel #',channel-status))
         . ,fields))))

(define-smf-channel-event (note-off #x80)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-smf-channel-event (note-on #x90)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-smf-channel-event (poly-pressure #xA0)
  (note 0 :type (unsigned-byte 8))
  (pressure 0 :type (unsigned-byte 8)))

(define-smf-channel-event (controller #xB0)
  (controller 0 :type (satisfies (unsigned-byte 8) (lambda (byte) (<= #x00 byte #x77))))
  (value 0 :type (unsigned-byte 8)))

(define-smf-channel-event (program-change #xC0)
  (program 0 :type (unsigned-byte 8)))

(define-smf-channel-event (channel-pressure #xD0)
  (pressure 0 :type (unsigned-byte 8)))

(define-smf-channel-event (pitch-bend #xE0)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

;;;;;;;;;;;;;;;;;;;
;; Mode messages ;;
;;;;;;;;;;;;;;;;;;;

(define-smf-channel-event (mode-message #xB0))

(defmacro define-smf-mode-message (name status)
  `(define-smf-event ((,name (:include smf-event-mode-message)) ,status)
     (value 0 :type (unsigned-byte 8))))

(define-smf-mode-message reset-all-controllers #x79)

(define-smf-mode-message local-control #x7A)

(define-smf-mode-message all-notes-off #x7B)

(define-smf-mode-message omni-mode-off #x7C)

(define-smf-mode-message omni-mode-on #x7D)

(define-smf-mode-message mono-mode-on #x7E)

(define-smf-mode-message poly-mode-on #x7F)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System common messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-smf-event (timing-code #xF1)
  (code 0 :type (unsigned-byte 8)))

(define-smf-event (song-position-pointer #xF2)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(define-smf-event (song-select #xF3)
  (song 0 :type (unsigned-byte 8)))

(define-smf-event (tune-request #xF6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System real-time messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-smf-event (timing-clock #xF8))

(define-smf-event (start-sequence #xFA))

(define-smf-event (continue-sequence #xFB))

(define-smf-event (stop-sequence #xFC))

(define-smf-event (active-sensing #xFE))

;;;;;;;;;
;; VLQ ;;
;;;;;;;;;

(declaim (inline make-%vlq))
(defbinstruct %vlq ()
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (satisfies (unsigned-byte 8) (lambda (byte) (plusp (ldb (byte 1 7) byte)))) (*)))
  (byte 0 :type (unsigned-byte 8)))

(declaim (inline %vlq-integer))
(defun %vlq-integer (vlq)
  (declare (dynamic-extent vlq))
  (loop :with value :of-type (unsigned-byte 64) := (%vlq-byte vlq)
        :with bytes := (%vlq-bytes vlq)
        :for i :of-type (mod 8) :from 1 :to (length bytes)
        :do (setf (ldb (byte 7 (* i 7)) value) (ldb (byte 7 0) (aref bytes (- (length bytes) i))))
        :finally (return value)))

(defun integer-%vlq (value)
  (loop :for v :of-type (unsigned-byte 64) := (ash value -7) :then (ash v -7)
        :while (plusp v)
        :collect (logior #x80 (ldb (byte 7 0) v)) :into bytes
        :finally (return (make-%vlq :bytes (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))
                                    :byte (ldb (byte 7 0) value)))))

(defbinstruct (vlq (:type (unsigned-byte 64)) (:constructor progn) (:conc-name nil)) ()
  (values 0 :type (map %vlq #'%vlq-integer #'integer-%vlq)))

;;;;;;;;;;;;;;;;;;
;; SysEx events ;;
;;;;;;;;;;;;;;;;;;

(define-smf-event (sysex #xF0)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(define-smf-event (authorization-sysex #xF7)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;;;;;;;;;;;;;;;;
;; Meta events ;;
;;;;;;;;;;;;;;;;;

(defbinstruct (smf-meta-event (:include smf-event) (:endian :big)) ()
  (nil #xFF :type (satisfies (unsigned-byte 8) (curry #'eql #xFF))))

(defmacro define-smf-meta-event ((name status) &body fields)
  `(define-smf-event ((,name (:include smf-meta-event)) ,status)
     (len 0 :type vlq)
     ,@fields))

(define-smf-meta-event (sequence-number #x00)
  (ssss 0 :type (unsigned-byte 16)))

(define-smf-meta-event (text #x01)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (copyright #x02)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (sequence-track-name #x03)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (instrument-name #x04)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (lyric #x05)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (marker #x06)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (cue-point #x07)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (program-name #x08)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (device-name #x09)
  (text "" :type (simple-base-string len)))

(define-smf-meta-event (channel-prefix #x20)
  (cc 0 :type (unsigned-byte 8)))

(define-smf-meta-event (end-of-track #x2F))

(define-smf-meta-event (tempo #x51)
  (tttttt 0 :type (unsigned-byte 24)))

(define-smf-meta-event (smpte-offset #x54)
  (hr 0 :type (unsigned-byte 8))
  (mn 0 :type (unsigned-byte 8))
  (se 0 :type (unsigned-byte 8))
  (fr 0 :type (unsigned-byte 8))
  (ff 0 :type (unsigned-byte 8)))

(define-smf-meta-event (time-signature #x58)
  (nn 0 :type (unsigned-byte 8))
  (dd 0 :type (unsigned-byte 8))
  (cc 0 :type (unsigned-byte 8))
  (bb 0 :type (unsigned-byte 8)))

(define-smf-meta-event (key-signature #x59)
  (sf 0 :type (signed-byte 8))
  (mi 0 :type (unsigned-byte 8)))

(define-smf-meta-event (sequencer-specific #x7F)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun smf-event-classes (&optional (class (find-class 'smf-event)))
    (or (loop :for class :in (c2mop:class-direct-subclasses class)
              :nconc (smf-event-classes class))
        (list class))))

;;;;;;;;;;;;;;;;
;; Containers ;;
;;;;;;;;;;;;;;;;

(defbinstruct (smf-track-event (:endian :big)) (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq)
  (event nil :type (or . #.(mapcar #'class-name (smf-event-classes)))))

(defbinstruct (smf-track (:endian :big)) ()
  (nil #.(coerce "MTrk" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (len-events 0 :type (unsigned-byte 32))
  (track-end 0 :type (map position (curry #'+ len-events)))
  (events (make-array 0 :element-type 'smf-track-event) :type (simple-array (smf-track-event track-end) (*))))

(defbinstruct (smf-header (:endian :big)) ()
  (nil #.(coerce "MThd" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (nil 6 :type (unsigned-byte 32))
  (format 0 :type (unsigned-byte 16))
  (num-tracks 0 :type (unsigned-byte 16))
  (division 0 :type (signed-byte 16)))

(defbinstruct (smf (:endian :big)) ()
  (header (make-smf-header) :type smf-header)
  (tracks (make-array 0 :element-type 'smf-track) :type (simple-array smf-track ((smf-header-num-tracks header)))))

(defbinio smf stream)
