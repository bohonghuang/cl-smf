(in-package #:binstruct)

(defbinstruct %vlq ()
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (simple-array (satisfies (unsigned-byte 8) (lambda (byte) (plusp (ldb (byte 1 7) byte)))) (*)))
  (byte 0 :type (unsigned-byte 8)))

(defun %read-vlq (vlq)
  (loop :with value :of-type (unsigned-byte 64) := (%vlq-byte vlq)
        :with bytes := (%vlq-bytes vlq)
        :for i :of-type (mod 8) :from 1 :to (length bytes)
        :do (setf (ldb (byte 7 (* i 7)) value) (ldb (byte 7 0) (aref bytes (- (length bytes) i))))
        :finally (return value)))

(defun %write-vlq (value)
  (loop :for v :of-type (unsigned-byte 64) := (ash value -7) :then (ash v -7)
        :while (plusp v)
        :collect (logior #x80 (ldb (byte 7 0) v)) :into bytes
        :finally (return (make-%vlq :bytes (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))
                                    :byte (ldb (byte 7 0) value)))))

(defbinstruct (vlq (:type (unsigned-byte 64)) (:constructor progn) (:conc-name nil)) ()
  (values 0 :type (map %vlq #'%read-vlq #'%write-vlq)))
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

;;; Channel event structs

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

;;; Mode message structs — share controller status nibble #xB

(define-midi-channel-event (midi-mode-message #xB0))

(define-midi-event ((midi-reset-all-controllers-event (:include midi-mode-message)) #x79)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-local-control-event (:include midi-mode-message)) #x7A)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-all-notes-off-event (:include midi-mode-message)) #x7B)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-omni-mode-off-event (:include midi-mode-message)) #x7C)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-omni-mode-on-event (:include midi-mode-message)) #x7D)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-mono-mode-on-event (:include midi-mode-message)) #x7E)
  (value 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-poly-mode-on-event (:include midi-mode-message)) #x7F)
  (value 0 :type (unsigned-byte 8)))

;;; System common message structs

(define-midi-event (midi-timing-code-event #xF1)
  (code 0 :type (unsigned-byte 8)))

(define-midi-event (midi-song-position-pointer-event #xF2)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(define-midi-event (midi-song-select-event #xF3)
  (song 0 :type (unsigned-byte 8)))

(define-midi-event (midi-tune-request-event #xF6))

;;; System real-time message structs

(define-midi-event (midi-timing-clock-event #xF8))

(define-midi-event (midi-start-sequence-event #xFA))

(define-midi-event (midi-continue-sequence-event #xFB))

(define-midi-event (midi-stop-sequence-event #xFC))

(define-midi-event (midi-active-sensing-event #xFE))

;;; SysEx event structs

(define-midi-event (midi-sysex-event #xF0)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(define-midi-event (midi-authorization-sysex-event #xF7)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; Meta event structs — fine-grained, :include from midi-meta-event parent

(define-midi-event ((midi-meta-event (:include midi-event)) #xFF))

(define-midi-event ((midi-sequence-number-event (:include midi-meta-event)) #x00)
  (len 0 :type vlq)
  (ssss 0 :type (unsigned-byte 16)))

(define-midi-event ((midi-text-event (:include midi-meta-event)) #x01)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-copyright-event (:include midi-meta-event)) #x02)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-sequence-track-name-event (:include midi-meta-event)) #x03)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-instrument-name-event (:include midi-meta-event)) #x04)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-lyric-event (:include midi-meta-event)) #x05)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-marker-event (:include midi-meta-event)) #x06)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-cue-point-event (:include midi-meta-event)) #x07)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-program-name-event (:include midi-meta-event)) #x08)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-device-name-event (:include midi-meta-event)) #x09)
  (len 0 :type vlq)
  (text "" :type (simple-base-string len)))

(define-midi-event ((midi-channel-prefix-event (:include midi-meta-event)) #x20)
  (len 0 :type vlq)
  (cc 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-end-of-track-event (:include midi-meta-event)) #x2F)
  (len 0 :type vlq))

(define-midi-event ((midi-tempo-event (:include midi-meta-event)) #x51)
  (len 0 :type vlq)
  (tttttt 0 :type (unsigned-byte 24)))

(define-midi-event ((midi-smpte-offset-event (:include midi-meta-event)) #x54)
  (len 0 :type vlq)
  (hr 0 :type (unsigned-byte 8))
  (mn 0 :type (unsigned-byte 8))
  (se 0 :type (unsigned-byte 8))
  (fr 0 :type (unsigned-byte 8))
  (ff 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-time-signature-event (:include midi-meta-event)) #x58)
  (len 0 :type vlq)
  (nn 0 :type (unsigned-byte 8))
  (dd 0 :type (unsigned-byte 8))
  (cc 0 :type (unsigned-byte 8))
  (bb 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-key-signature-event (:include midi-meta-event)) #x59)
  (len 0 :type vlq)
  (sf 0 :type (signed-byte 8))
  (mi 0 :type (unsigned-byte 8)))

(define-midi-event ((midi-sequencer-specific-event (:include midi-meta-event)) #x7F)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; Compute all leaf midi-event subclasses for the or union.
;;; Leaves are subclasses with no direct subclasses of their own.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun midi-event-classes (&optional (class (find-class 'midi-event)))
    (or (loop :for class :in (c2mop:class-direct-subclasses class)
              :nconc (midi-event-classes class))
        (list class))))

;;; Track event — parametric with position sentinel

(defbinstruct (midi-track-event (:endian :big)) (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq)
  (event nil :type (or . #.(mapcar #'class-name (midi-event-classes)))))

;;; Track struct — computes end boundary from len-events

(defbinstruct (midi-track (:endian :big)) ()
  (nil (coerce "MTrk" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (len-events 0 :type (unsigned-byte 32))
  (track-end 0 :type (map position (curry #'+ len-events)))
  (events (make-array 0 :element-type 'midi-track-event) :type (simple-array (midi-track-event track-end) (*))))

;;; Header struct

(defbinstruct (midi-header (:endian :big)) ()
  (nil (coerce "MThd" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (nil 6 :type (unsigned-byte 32))
  (format 0 :type (unsigned-byte 16))
  (num-tracks 0 :type (unsigned-byte 16))
  (division 0 :type (signed-byte 16)))

;;; Top-level file struct

(defbinstruct (midi-file (:endian :big)) ()
  (header (make-midi-header) :type midi-header)
  (tracks (make-array 0 :element-type 'midi-track) :type (simple-array midi-track ((midi-header-num-tracks header)))))

;;; Reader function

(defbinio midi-file stream)
