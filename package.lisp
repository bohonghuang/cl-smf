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
;; Running status ;;
;;;;;;;;;;;;;;;;;;;;

(declaim (type (unsigned-byte 8) *smf-running-status*))
(defvar *smf-running-status*)

(defbinstruct (smf-running-status (:type (unsigned-byte 8)) (:constructor progn) (:conc-name nil)) (expected)
  (values 0 :type (satisfies
                   (map (binstruct::peek (unsigned-byte 8))
                        (the (function (t) (unsigned-byte 8))
                             (lambda (byte) (if (< byte #x80) *smf-running-status* 0)))
                        (constantly 0))
                   (lambda (status) (= (ldb (byte 4 4) status) (ldb (byte 4 4) (the (unsigned-byte 8) expected)))))))

;;;;;;;;;;;;;;;;;;;;
;; Channel events ;;
;;;;;;;;;;;;;;;;;;;;

(defbinstruct (smf-channel-event (:include smf-event)) ())

(declaim (ftype (function ((unsigned-byte 8)) (values (unsigned-byte 4))) smf-status-channel))
(defun smf-status-channel (status)
  (assert (ldb (byte 4 4) status))
  (ldb (byte 4 0) (setf *smf-running-status* status)))

(defmacro define-smf-channel-event ((name status) &body fields)
  (let* ((struct (symbolicate '#:smf-event- name))
         (struct-status (symbolicate struct '#:/status))
         (status-channel 'smf-status-channel)
         (channel-status (symbolicate 'smf-channel-status '/ name)))
    `(progn
       (defun ,channel-status (channel)
         (dpb channel (byte 4 0) ,status))
       (defbinstruct (,struct-status (:type (unsigned-byte 8)) (:constructor progn) (:conc-name nil)) ()
         (values ,status :type ,(if fields
                                    `(or . ,(loop :for i :from #x00 :to #x0F
                                                  :collect `(satisfies (unsigned-byte 8) (curry #'eql ,(+ status i)))))
                                    `(satisfies (unsigned-byte 8) ,(with-gensyms (byte) `(lambda (,byte) (<= ,(+ status #x00) ,byte  ,(+ status #x0F))))))))
       (defbinstruct (,struct (:include smf-channel-event) (:endian :big)) ()
         (channel 0 :type (map (or (,struct-status) (smf-running-status ,status))
                               (the (function ((unsigned-byte 8)) (unsigned-byte 4)) #',status-channel)
                               (the (function ((unsigned-byte 4)) (unsigned-byte 8)) #',channel-status)))
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

(define-smf-channel-event (control-change #xB0))

(defmacro define-smf-control-event ((name status) &body fields)
  (let* ((struct (symbolicate '#:smf-control-event- name))
         (status-list (ensure-list status)))
    (destructuring-bind (&optional status-field (reader '#'identity) (writer '#'identity))
        (ensure-list (typecase (car status-list)
                       ((or cons symbol) (pop status-list))))
      (let ((type `(or . ,(loop :for status :in status-list
                                :collect `(satisfies (unsigned-byte 8) (curry #'eql ,status)))))
            (fields (or fields '((nil 0 :type (unsigned-byte 8))))))
        `(defbinstruct (,struct (:include smf-event-control-change) (:endian :big)) ()
           (,status-field 0 :type (map ,type (the (function ((unsigned-byte 8)) (unsigned-byte 8)) ,reader) ,writer))
           ,@fields)))))

(define-smf-control-event (bank-select #x00)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (modulation-wheel #x01)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (breath-controller #x02)
  (value 0 :type (unsigned-byte 8)))

;; #x03 undefined

(define-smf-control-event (foot-pedal #x04)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (portamento-time #x05)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (data-entry #x06)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (volume #x07)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (balance #x08)
  (value 0 :type (unsigned-byte 8)))

;; #x09 undefined

(define-smf-control-event (pan #x0A)
  (value 0 :type (signed-byte 7))
  (nil 0 :type (satisfies bit)))

(define-smf-control-event (expression #x0B)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event
    (effect-controller
     ((id (lambda (status)
            (ecase status
              (#x0C 1)
              (#x0D 2)
              (#x2C 3)
              (#x2D 4)))
          (lambda (id)
            (ecase id
              (1 #x0C)
              (2 #x0D)
              (3 #x2C)
              (4 #x2D))))
      #x0C #x0D #x2C #x2D))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event
    (general-purpose
     ((id (lambda (status)
            (ecase status
              (#x10 1)
              (#x11 2)
              (#x12 3)
              (#x13 4)))
          (lambda (id)
            (ecase id
              (1 #x10)
              (2 #x11)
              (3 #x12)
              (4 #x13))))
      #x10 #x11 #x12 #x13))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (bank-select/lsb #x20)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (modulation-wheel/lsb #x21)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (breath-controller/lsb #x22)
  (value 0 :type (unsigned-byte 8)))

;; #x23 undefined

(define-smf-control-event (foot-pedal/lsb #x24)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (portamento-time/lsb #x25)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (data-entry/lsb #x26)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (volume/lsb #x27)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (balance/lsb #x28)
  (value 0 :type (unsigned-byte 8)))

;; #x29 undefined

(define-smf-control-event (pan/lsb #x2A)
  (value 0 :type (signed-byte 7))
  (nil 0 :type (satisfies bit)))

(define-smf-control-event (expression/lsb #x2B)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event
    (effect-controller/lsb
     ((id (lambda (status)
            (ecase status
              (#x2C 1)
              (#x2D 2)
              (#x4C 3)
              (#x4D 4)))
          (lambda (id)
            (ecase id
              (1 #x2C)
              (2 #x2D)
              (3 #x4C)
              (4 #x4D))))
      #x2C #x2D #x4C #x4D))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event
    (general-purpose/lsb
     ((id (lambda (status)
            (ecase status
              (#x30 1)
              (#x31 2)
              (#x32 3)
              (#x33 4)))
          (lambda (id)
            (ecase id
              (1 #x30)
              (2 #x31)
              (3 #x32)
              (4 #x33))))
      #x30 #x31 #x32 #x33))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (sound-controller
                           ((id (lambda (status)
                                  (ecase status
                                    (#x46 1) (#x47 2) (#x48 3) (#x49 4) (#x4A 5)
                                    (#x4B 6) (#x4C 7) (#x4D 8) (#x4E 9) (#x4F 10)))
                                (lambda (id)
                                  (ecase id
                                    (1 #x46) (2 #x47) (3 #x48) (4 #x49) (5 #x4A)
                                    (6 #x4B) (7 #x4C) (8 #x4D) (9 #x4E) (10 #x4F))))
                            #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event
    (effect-depth
     ((id (lambda (status)
            (ecase status
              (#x5B 1)
              (#x5C 2)
              (#x5D 3)
              (#x5E 4)
              (#x5F 5)))
          (lambda (id)
            (ecase id
              (1 #x5B)
              (2 #x5C)
              (3 #x5D)
              (4 #x5E)
              (5 #x5F))))
      #x5B #x5C #x5D #x5E #x5F))
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (data-increment #x60)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (data-decrement #x61)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (nrpn-lsb #x62)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (nrpn-msb #x63)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (rpn-lsb #x64)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (rpn-msb #x65)
  (value 0 :type (unsigned-byte 8)))

;; #x66--#x77 undefined

(define-smf-control-event (all-sound-off #x78))

(define-smf-control-event (reset-all-controllers #x79))

(define-smf-control-event (local-on-off #x7A)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (all-notes-off #x7B))

(define-smf-control-event (omni-mode-off #x7C))

(define-smf-control-event (omni-mode-on #x7D))

(define-smf-control-event (mono-mode #x7E)
  (value 0 :type (unsigned-byte 8)))

(define-smf-control-event (poly-mode #x7F))

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
  (event (make-smf-control-event-all-notes-off) :type (or . #.(mapcar #'class-name (smf-event-classes)))))

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

(defbinio (smf &aux (*smf-running-status* 0)) stream)
