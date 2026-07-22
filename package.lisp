(defpackage cl-smf
  (:use #:cl #:alexandria #:binstruct)
  (:nicknames #:smf))

(in-package #:smf)

(defbinstruct midi-event ())

(defbinstruct (midi-channel-event (:include midi-event)) ())

(defmacro define-midi-channel-event ((name status) &body fields)
  `(defbinstruct (,name (:include midi-channel-event) (:endian :big)) ()
     (channel ,status :type (map (or . ,(loop :for i :from #x00 :to #x0F
                                              :collect `(satisfies (unsigned-byte 8) (curry #'eql ,(+ status i)))))
                                 (curry #'ldb (byte 4 0))
                                 (rcurry #'dpb (byte 4 0) ,status)))
     . ,fields))

(defmacro define-midi-event ((name status) &body fields)
  (let* ((name-list (ensure-list name))
         (has-include (some (lambda (opt) (and (consp opt) (find :include (rest opt)))) name-list)))
    `(defbinstruct (,@name-list ,@(unless has-include '((:include midi-event))) (:endian :big)) ()
       (nil ,status :type (satisfies (unsigned-byte 8) (curry #'eql ,status)))
       . ,fields)))

;;;;;;;;;;;;;;;;;;;;
;; Channel events ;;
;;;;;;;;;;;;;;;;;;;;

(define-midi-channel-event (midi-note-off-event #x80)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-note-on-event #x90)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-poly-pressure-event #xA0)
  (note 0 :type (unsigned-byte 8))
  (pressure 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-controller-event #xB0)
  (controller 0 :type (unsigned-byte 8))
  (value 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-program-change-event #xC0)
  (program 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-channel-pressure-event #xD0)
  (pressure 0 :type (unsigned-byte 8)))

(define-midi-channel-event (midi-pitch-bend-event #xE0)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

;;;;;;;;;;;;;;;;;;;
;; Mode messages ;;
;;;;;;;;;;;;;;;;;;;

(define-midi-channel-event (midi-mode-message #xB0))

(defmacro define-midi-mode-message (name status)
  `(define-midi-event ((,name (:include midi-mode-message)) ,status)
     (value 0 :type (unsigned-byte 8))))

(define-midi-mode-message midi-reset-all-controllers-event #x79)

(define-midi-mode-message midi-local-control-event #x7A)

(define-midi-mode-message midi-all-notes-off-event #x7B)

(define-midi-mode-message midi-omni-mode-off-event #x7C)

(define-midi-mode-message midi-omni-mode-on-event #x7D)

(define-midi-mode-message midi-mono-mode-on-event #x7E)

(define-midi-mode-message midi-poly-mode-on-event #x7F)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System common messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-midi-event (midi-timing-code-event #xF1)
  (code 0 :type (unsigned-byte 8)))

(define-midi-event (midi-song-position-pointer-event #xF2)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(define-midi-event (midi-song-select-event #xF3)
  (song 0 :type (unsigned-byte 8)))

(define-midi-event (midi-tune-request-event #xF6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System real-time messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-midi-event (midi-timing-clock-event #xF8))

(define-midi-event (midi-start-sequence-event #xFA))

(define-midi-event (midi-continue-sequence-event #xFB))

(define-midi-event (midi-stop-sequence-event #xFC))

(define-midi-event (midi-active-sensing-event #xFE))

;;;;;;;;;
;; VLQ ;;
;;;;;;;;;

(defbinstruct %vlq ()
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (satisfies (unsigned-byte 8) (lambda (byte) (plusp (ldb (byte 1 7) byte)))) (*)))
  (byte 0 :type (unsigned-byte 8)))

(defun %vlq-integer (vlq)
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

(define-midi-event (midi-sysex-event #xF0)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(define-midi-event (midi-authorization-sysex-event #xF7)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;;;;;;;;;;;;;;;;
;; Meta events ;;
;;;;;;;;;;;;;;;;;

(define-midi-event ((midi-meta-event (:include midi-event)) #xFF))

(defmacro define-midi-meta-event ((name status) &body fields)
  `(define-midi-event ((,name (:include midi-meta-event)) ,status)
     (len 0 :type vlq)
     ,@fields))

(define-midi-meta-event (midi-sequence-number-event #x00)
  (ssss 0 :type (unsigned-byte 16)))

(define-midi-meta-event (midi-text-event #x01)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-copyright-event #x02)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-sequence-track-name-event #x03)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-instrument-name-event #x04)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-lyric-event #x05)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-marker-event #x06)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-cue-point-event #x07)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-program-name-event #x08)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-device-name-event #x09)
  (text "" :type (simple-base-string len)))

(define-midi-meta-event (midi-channel-prefix-event #x20)
  (cc 0 :type (unsigned-byte 8)))

(define-midi-meta-event (midi-end-of-track-event #x2F))

(define-midi-meta-event (midi-tempo-event #x51)
  (tttttt 0 :type (unsigned-byte 24)))

(define-midi-meta-event (midi-smpte-offset-event #x54)
  (hr 0 :type (unsigned-byte 8))
  (mn 0 :type (unsigned-byte 8))
  (se 0 :type (unsigned-byte 8))
  (fr 0 :type (unsigned-byte 8))
  (ff 0 :type (unsigned-byte 8)))

(define-midi-meta-event (midi-time-signature-event #x58)
  (nn 0 :type (unsigned-byte 8))
  (dd 0 :type (unsigned-byte 8))
  (cc 0 :type (unsigned-byte 8))
  (bb 0 :type (unsigned-byte 8)))

(define-midi-meta-event (midi-key-signature-event #x59)
  (sf 0 :type (signed-byte 8))
  (mi 0 :type (unsigned-byte 8)))

(define-midi-meta-event (midi-sequencer-specific-event #x7F)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun midi-event-classes (&optional (class (find-class 'midi-event)))
    (or (loop :for class :in (c2mop:class-direct-subclasses class)
              :nconc (midi-event-classes class))
        (list class))))

(defbinstruct (smf-track-event (:endian :big)) (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq)
  (event nil :type (or . #.(mapcar #'class-name (midi-event-classes)))))

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
