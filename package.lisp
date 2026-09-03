(defpackage cl-smf
  (:use #:cl #:alexandria #:binstruct)
  (:shadow #:read #:write #:copy-file #:switch)
  (:nicknames #:smf)
  (:export
   #:read
   #:write))

(in-package #:smf)

(defmacro defbinalias (name-and-options lambda-list parser)
  (destructuring-bind (name &rest options) (ensure-list name-and-options)
    (destructuring-bind (&key (type name typep)) (mappend #'identity options)
      (unless typep
        (let ((inferred-type (binstruct::lisp-type parser)))
          (unless (eq inferred-type t)
            (setf type inferred-type))))
      `(defbinstruct (,name (:type ,type) (:constructor progn) (:conc-name nil))
         ,lambda-list
         (values nil :type ,parser)))))

(defbinstruct event ())

(defmacro define-event ((name status) &body fields)
  (destructuring-bind (name &rest options) (ensure-list name)
    (let ((name (alexandria:symbolicate '#:event- name)))
      `(defbinstruct (,name (:endian :big) . ,options) ()
         (nil ,status :type (satisfies (unsigned-byte 8) (curry #'eql ,status)))
         . ,fields))))

;;;;;;;;;;;;;;;;;;;;
;; Running status ;;
;;;;;;;;;;;;;;;;;;;;

(declaim (type (unsigned-byte 8) *running-status*))
(defvar *running-status*)

(defbinalias (running-status (:type (unsigned-byte 8))) (expected)
  (satisfies
   (map (binstruct::peek (unsigned-byte 8))
        (the (function (t) (unsigned-byte 8))
             (lambda (byte) (if (< byte #x80) *running-status* 0)))
        (constantly 0))
   (lambda (status) (= (ldb (byte 4 4) status) (ldb (byte 4 4) (the (unsigned-byte 8) expected))))))

;;;;;;;;;;;;;;;;;;;;
;; Channel events ;;
;;;;;;;;;;;;;;;;;;;;

(defbinstruct (channel-event (:include event)) ())

(declaim (ftype (function ((unsigned-byte 8)) (values (unsigned-byte 4))) status-channel))
(defun status-channel (status)
  (assert (ldb (byte 4 4) status))
  (ldb (byte 4 0) (setf *running-status* status)))

(defmacro define-channel-event ((name status) &body fields)
  (let* ((struct (symbolicate '#:event- name))
         (struct-status (symbolicate struct '#:/status))
         (status-channel 'status-channel)
         (channel-status (symbolicate 'channel-status '/ name)))
    `(progn
       (defun ,channel-status (channel)
         (dpb channel (byte 4 0) ,status))
       (defbinstruct (,struct-status (:type (unsigned-byte 8)) (:constructor progn) (:conc-name nil)) ()
         (values ,status :type ,(if fields
                                    `(or . ,(loop :for i :from #x00 :to #x0F
                                                  :collect `(satisfies (unsigned-byte 8) (curry #'eql ,(+ status i)))))
                                    `(satisfies (unsigned-byte 8) ,(with-gensyms (byte) `(lambda (,byte) (<= ,(+ status #x00) ,byte  ,(+ status #x0F))))))))
       (defbinstruct (,struct (:include channel-event) (:endian :big)) ()
         (channel 0 :type (map (or (,struct-status) (running-status ,status))
                               (the (function ((unsigned-byte 8)) (unsigned-byte 4)) #',status-channel)
                               (the (function ((unsigned-byte 4)) (unsigned-byte 8)) #',channel-status)))
         . ,fields))))

(define-channel-event (note-off #x80)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-channel-event (note-on #x90)
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(define-channel-event (poly-pressure #xA0)
  (note 0 :type (unsigned-byte 8))
  (pressure 0 :type (unsigned-byte 8)))

(define-channel-event (control-change #xB0))

(define-channel-event (program-change #xC0)
  (program 0 :type (unsigned-byte 8)))

(define-channel-event (channel-pressure #xD0)
  (pressure 0 :type (unsigned-byte 8)))

(define-channel-event (pitch-bend #xE0)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(defmacro define-control-event ((name status) &body fields)
  (let* ((struct (if (stringp name) (intern name) (symbolicate '#:control-event- name)))
         (status-list (ensure-list status))
         (status-field (typecase (car status-list)
                         (symbol (pop status-list))
                         (t (when (> (length status-list) 1) 'id))))
         (status-mapping (loop :for (status id) :in (mapcar #'ensure-list status-list)
                               :for i :from 1
                               :collect `(,status ,(or id i))))
         (type `(or . ,(loop :for (status) :in status-mapping
                             :collect `(satisfies (unsigned-byte 8) (curry #'eql ,status)))))
         (fields (or fields '((nil 0 :type (unsigned-byte 8))))))
    `(defbinstruct (,struct (:include event-control-change) (:endian :big)) ()
       (,status-field 0 :type ,(if status-field
                                   (with-gensyms (arg)
                                     `(map ,type (the (function ((unsigned-byte 8)) (unsigned-byte 8))
                                                      (lambda (,arg) (ecase ,arg . ,status-mapping)))
                                           (lambda (,arg) (ecase ,arg . ,(reverse status-mapping)))))
                                   type))
       ,@fields)))

(defmacro define-mode-message ((name status) &body fields)
  `(define-control-event (,(format nil "~A~A" '#:mode-message- name) ,status) . ,fields))

(define-control-event (bank-select/msb #x00)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (modulation-wheel/msb #x01)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (breath-controller/msb #x02)
  (value 0 :type (unsigned-byte 8)))

;; #x03 undefined

(define-control-event (foot-pedal/msb #x04)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (portamento-time/msb #x05)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (data-entry/msb #x06)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (volume/msb #x07)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (balance/msb #x08)
  (value 0 :type (unsigned-byte 8)))

;; #x09 undefined

(define-control-event (pan/msb #x0A)
  (value 0 :type (signed-byte 7))
  (nil 0 :type (satisfies bit)))

(define-control-event (expression/msb #x0B)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (effect-controller/msb (#x0C #x0D #x2C #x2D))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (general-purpose/msb (#x10 #x11 #x12 #x13))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (bank-select/lsb #x20)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (modulation-wheel/lsb #x21)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (breath-controller/lsb #x22)
  (value 0 :type (unsigned-byte 8)))

;; #x23 undefined

(define-control-event (foot-pedal/lsb #x24)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (portamento-time/lsb #x25)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (data-entry/lsb #x26)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (volume/lsb #x27)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (balance/lsb #x28)
  (value 0 :type (unsigned-byte 8)))

;; #x29 undefined

(define-control-event (pan/lsb #x2A)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (expression/lsb #x2B)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (effect-controller/lsb (#x2C #x2D #x4C #x4D))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (general-purpose/lsb (#x30 #x31 #x32 #x33))
  (value 0 :type (unsigned-byte 8)))

(defbinalias (switch (:type boolean)) ()
  (map (unsigned-byte 8) (lambda (byte) (>= byte #x40)) (lambda (boolean) (if boolean #x7F #x00))))

(define-control-event (damper-pedal #x40)
  (value 0 :type switch))

(define-control-event (portamento-on-off #x41)
  (value 0 :type switch))

(define-control-event (sostenuto #x42)
  (value 0 :type switch))

(define-control-event (soft-pedal #x43)
  (value 0 :type switch))

(define-control-event (legato-footswitch #x44)
  (value 0 :type switch))

(define-control-event (hold-2 #x45)
  (value 0 :type switch))

(define-control-event (sound-controller (#x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (general-purpose-switch (#x50 #x51 #x52 #x53))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (portamento-control #x54)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (high-resolution-velocity-prefix #x58)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (effect-depth (#x5B #x5C #x5D #x5E #x5F))
  (value 0 :type (unsigned-byte 8)))

(define-control-event (data-increment #x60)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (data-decrement #x61)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (nrpn/lsb #x62)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (nrpn/msb #x63)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (rpn/lsb #x64)
  (value 0 :type (unsigned-byte 8)))

(define-control-event (rpn/msb #x65)
  (value 0 :type (unsigned-byte 8)))

;; #x66--#x77 undefined

(define-mode-message (all-sound-off #x78))

(define-mode-message (reset-all-controllers #x79))

(define-mode-message (local-on-off #x7A)
  (value 0 :type (boolean (unsigned-byte 8))))

(define-mode-message (all-notes-off #x7B))

(define-mode-message (omni-mode-off #x7C))

(define-mode-message (omni-mode-on #x7D))

(define-mode-message (mono-mode #x7E)
  (value 0 :type (unsigned-byte 8)))

(define-mode-message (poly-mode #x7F))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System common messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event (timing-code #xF1)
  (code 0 :type (unsigned-byte 8)))

(define-event (song-position-pointer #xF2)
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(define-event (song-select #xF3)
  (song 0 :type (unsigned-byte 8)))

(define-event (tune-request #xF6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System real-time messages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event (timing-clock #xF8))

(define-event (start-sequence #xFA))

(define-event (continue-sequence #xFB))

(define-event (stop-sequence #xFC))

(define-event (active-sensing #xFE))

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

(defbinalias (vlq (:type (unsigned-byte 64))) ()
  (map %vlq #'%vlq-integer #'integer-%vlq))

;;;;;;;;;;;;;;;;;;
;; SysEx events ;;
;;;;;;;;;;;;;;;;;;

(define-event (sysex #xF0)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(define-event (authorization-sysex #xF7)
  (len 0 :type vlq)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;;;;;;;;;;;;;;;;
;; Meta events ;;
;;;;;;;;;;;;;;;;;

(defbinstruct (meta-event (:include event) (:endian :big)) ()
  (nil #xFF :type (satisfies (unsigned-byte 8) (curry #'eql #xFF))))

(defmacro define-meta-event ((name status) &body fields)
  `(define-event ((,name (:include meta-event)) ,status)
     (len 0 :type vlq)
     ,@fields))

(define-meta-event (sequence-number #x00)
  (ssss 0 :type (unsigned-byte 16)))

(define-meta-event (text #x01)
  (text "" :type (simple-base-string len)))

(define-meta-event (copyright #x02)
  (text "" :type (simple-base-string len)))

(define-meta-event (sequence-track-name #x03)
  (text "" :type (simple-base-string len)))

(define-meta-event (instrument-name #x04)
  (text "" :type (simple-base-string len)))

(define-meta-event (lyric #x05)
  (text "" :type (simple-base-string len)))

(define-meta-event (marker #x06)
  (text "" :type (simple-base-string len)))

(define-meta-event (cue-point #x07)
  (text "" :type (simple-base-string len)))

(define-meta-event (program-name #x08)
  (text "" :type (simple-base-string len)))

(define-meta-event (device-name #x09)
  (text "" :type (simple-base-string len)))

(define-meta-event (channel-prefix #x20)
  (cc 0 :type (unsigned-byte 8)))

(define-meta-event (end-of-track #x2F))

(define-meta-event (tempo #x51)
  (tttttt 0 :type (unsigned-byte 24)))

(define-meta-event (smpte-offset #x54)
  (hr 0 :type (unsigned-byte 8))
  (mn 0 :type (unsigned-byte 8))
  (se 0 :type (unsigned-byte 8))
  (fr 0 :type (unsigned-byte 8))
  (ff 0 :type (unsigned-byte 8)))

(define-meta-event (time-signature #x58)
  (nn 0 :type (unsigned-byte 8))
  (dd 0 :type (unsigned-byte 8))
  (cc 0 :type (unsigned-byte 8))
  (bb 0 :type (unsigned-byte 8)))

(define-meta-event (key-signature #x59)
  (sf 0 :type (signed-byte 8))
  (mi 0 :type (unsigned-byte 8)))

(define-meta-event (sequencer-specific #x7F)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun event-classes (&optional (class (find-class 'event)))
    (or (loop :for class :in (c2mop:class-direct-subclasses class)
              :nconc (event-classes class))
        (list class))))

(defbinalias (event-union (:type event)) ()
  (or . #.(mapcar #'class-name (event-classes))))

;;;;;;;;;;;;;;;;
;; Containers ;;
;;;;;;;;;;;;;;;;

(defbinstruct (track-event (:endian :big)) (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq)
  (event (make-mode-message-all-notes-off) :type event-union))

(defbinstruct (track (:endian :big)) ()
  ($track-start 0 :type position)
  (nil #.(coerce "MTrk" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (%len-events 0 :type (values (pointer (unsigned-byte 32) (map (unsigned-byte 32)
                                                                (the (function ((unsigned-byte 32)) (unsigned-byte 32)) (constantly 4))
                                                                (constantly (max (- $track-end $track-start 4 4) 0)))
                                        $track-start)))
  (%track-end 0 :type (map position (curry #'+ %len-events)))
  (events (make-array 0 :element-type 'track-event) :type (simple-array (track-event %track-end) (*)))
  ($track-end 0 :type position))

(defbinstruct (header (:endian :big)) ()
  (nil #.(coerce "MThd" 'simple-base-string) :type (satisfies (simple-base-string 4)))
  (nil 6 :type (unsigned-byte 32))
  (format 0 :type (unsigned-byte 16))
  (num-tracks 0 :type (unsigned-byte 16))
  (division 0 :type (signed-byte 16)))

(defbinstruct (file (:endian :big)) ()
  (header (make-header) :type header)
  (tracks (make-array 0 :element-type 'track) :type (simple-array track ((header-num-tracks header)))))

(defbinio (file &aux (*running-status* 0)) stream)

(defgeneric read (input)
  (:method ((stream stream))
    (read-file stream))
  (:method ((vector vector))
    (read (flex:make-in-memory-input-stream vector)))
  (:method ((pathname pathname))
    (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
      (read stream))))

(defgeneric write (object output)
  (:method ((object file) (stream stream))
    (write-file stream object))
  (:method ((object file) (null null))
    (let ((stream (flex:make-in-memory-output-stream)))
      (write object stream)
      (flex:get-output-stream-sequence stream)))
  (:method ((object file) (pathname pathname))
    (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8))
      (write object stream))))
