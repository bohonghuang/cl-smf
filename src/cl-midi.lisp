;;;; cl-midi.lisp — MIDI file reader with CLOS message hierarchy.
;;;; Rivals the `midi` library by Strandh et al. with a binstruct-backed parser.

(defpackage #:midi
  (:use #:common-lisp)
  (:export #:read-midi-file
           #:midifile
           #:midifile-format #:midifile-tracks #:midifile-division
           #:message #:note-off-message #:note-on-message
           #:polyphonic-key-pressure-message #:control-change-message
           #:program-change-message #:channel-pressure-message
           #:pitch-bend-message
           #:meta-message #:tempo-message #:time-signature-message
           #:key-signature-message #:smpte-offset-message
           #:end-of-track-message #:sequence-number-message
           #:text-message #:sequence/track-name-message #:copyright-message
           #:instrument-name-message #:lyric-message #:marker-message
           #:cue-point-message #:channel-prefix-message
           #:system-exclusive-message
           #:message-time #:message-status #:message-channel
           #:message-key #:message-velocity #:message-program
           #:message-tempo #:message-numerator #:message-denominator
           #:message-sf #:message-mi #:message-value
           #:message-controller #:message-pressure
           #:message-data #:message-text
           #:unknown-event #:header))

(in-package #:midi)

;;; Conditions

(define-condition unknown-event ()
  ((status :initarg :status :reader status)
   (data-byte :initform nil :initarg :data-byte :reader data-byte))
  (:documentation "Condition when an unknown MIDI event is encountered."))

(define-condition header ()
  ((header-type :initarg :header :reader header-type))
  (:documentation "Condition when a MIDI chunk header is incorrect."))

;;; Midifile class

(defclass midifile ()
  ((format :initarg :format :reader midifile-format)
   (division :initarg :division :reader midifile-division)
   (tracks :initarg :tracks :reader midifile-tracks))
  (:documentation "Represents a Standard MIDI File in core."))

;;; Message protocol — generic functions

(defgeneric message-time (message))
(defgeneric (setf message-time) (time message))
(defgeneric message-status (message))
(defgeneric message-channel (message))
(defgeneric message-key (message))
(defgeneric message-velocity (message))
(defgeneric message-program (message))
(defgeneric message-tempo (message))
(defgeneric message-numerator (message))
(defgeneric message-denominator (message))
(defgeneric message-sf (message))
(defgeneric message-mi (message))
(defgeneric message-value (message))
(defgeneric message-controller (message))
(defgeneric message-pressure (message))
(defgeneric message-data (message))
(defgeneric message-text (message))

;;; Message class hierarchy

(defclass message ()
  ((time :initarg :time :accessor message-time)
   (status :initarg :status :reader message-status))
  (:documentation "Base class for all MIDI messages."))

(defclass channel-message (message)
  ((channel :initarg :channel :reader message-channel))
  (:documentation "Base class for channel-voice and channel-mode messages."))

(defclass voice-message (channel-message)
  ()
  (:documentation "Base class for channel voice messages."))

(defclass note-off-message (voice-message)
  ((key :initarg :key :reader message-key)
   (velocity :initarg :velocity :reader message-velocity)))

(defclass note-on-message (voice-message)
  ((key :initarg :key :reader message-key)
   (velocity :initarg :velocity :reader message-velocity)))

(defclass polyphonic-key-pressure-message (voice-message)
  ((key :initarg :key :reader message-key)
   (pressure :initarg :pressure :reader message-pressure)))

(defclass control-change-message (voice-message)
  ((controller :initarg :controller :reader message-controller)
   (value :initarg :value :reader message-value)))

(defclass program-change-message (voice-message)
  ((program :initarg :program :reader message-program)))

(defclass channel-pressure-message (voice-message)
  ((pressure :initarg :pressure :reader message-pressure)))

(defclass pitch-bend-message (voice-message)
  ((value :initarg :value :reader message-value)))

;;; System messages

(defclass system-message (message)
  ()
  (:documentation "Base class for system messages."))

(defclass system-exclusive-message (system-message)
  ((data :initarg :data :reader message-data)))

;;; Meta messages

(defclass meta-message (message)
  ()
  (:documentation "Base class for meta messages."))

(defclass sequence-number-message (meta-message)
  ((sequence :initarg :sequence :reader message-data)))

(defclass text-message (meta-message)
  ((text :initarg :text :reader message-text)))

(defclass copyright-message (text-message) ())
(defclass sequence/track-name-message (text-message) ())
(defclass instrument-name-message (text-message) ())
(defclass lyric-message (text-message) ())
(defclass marker-message (text-message) ())
(defclass cue-point-message (text-message) ())

(defclass channel-prefix-message (meta-message)
  ((channel :initarg :channel :reader message-channel)))

(defclass end-of-track-message (meta-message) ())

(defclass tempo-message (meta-message)
  ((tempo :initarg :tempo :reader message-tempo)))

(defclass smpte-offset-message (meta-message)
  ((hr :initarg :hr :reader message-data)
   (mn :initarg :mn)
   (se :initarg :se)
   (fr :initarg :fr)
   (ff :initarg :ff)))

(defclass time-signature-message (meta-message)
  ((numerator :initarg :numerator :reader message-numerator)
   (denominator :initarg :denominator :reader message-denominator)
   (cc :initarg :cc)
   (bb :initarg :bb)))

(defclass key-signature-message (meta-message)
  ((sf :initarg :sf :reader message-sf)
   (mi :initarg :mi :reader message-mi)))

;;; Byte stream reader — sequential byte access with position tracking

(defstruct midi-stream
  (data nil :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type (integer 0)))

(declaim (inline %read-byte %peek-byte))
(defun %read-byte (stream)
  (aref (midi-stream-data stream)
        (prog1 (midi-stream-pos stream)
          (incf (midi-stream-pos stream)))))

(defun %peek-byte (stream)
  (aref (midi-stream-data stream) (midi-stream-pos stream)))

(declaim (inline %stream-end-p))
(defun %stream-end-p (stream end)
  (>= (midi-stream-pos stream) end))

(defun %read-vlq (stream)
  (loop :with value := 0
        :for byte := (%read-byte stream)
        :do (setf value (logior (ash value 7) (logand byte #x7F)))
        :until (< byte #x80)
        :finally (return value)))

(defun %read-bytes (stream count)
  (let ((arr (make-array count :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below count
          :do (setf (aref arr i) (%read-byte stream)))
    arr))

(defun %read-string (stream count)
  (let ((str (make-string count)))
    (loop :for i :from 0 :below count
          :do (setf (aref str i) (code-char (%read-byte stream))))
    str))

;;; Event parsing with running-status support

(defun %parse-channel-event (stream status)
  (let ((channel (logand status #x0F))
        (status-hi (ash status -4)))
    (ecase status-hi
      (#x8 (make-instance 'note-off-message
                          :status status :channel channel
                          :key (%read-byte stream)
                          :velocity (%read-byte stream)))
      (#x9 (let ((key (%read-byte stream))
                 (vel (%read-byte stream)))
             (if (zerop vel)
                 (make-instance 'note-off-message
                                :status status :channel channel
                                :key key :velocity vel)
                 (make-instance 'note-on-message
                                :status status :channel channel
                                :key key :velocity vel))))
      (#xA (make-instance 'polyphonic-key-pressure-message
                          :status status :channel channel
                          :key (%read-byte stream)
                          :pressure (%read-byte stream)))
      (#xB (make-instance 'control-change-message
                          :status status :channel channel
                          :controller (%read-byte stream)
                          :value (%read-byte stream)))
      (#xC (make-instance 'program-change-message
                          :status status :channel channel
                          :program (%read-byte stream)))
      (#xD (make-instance 'channel-pressure-message
                          :status status :channel channel
                          :pressure (%read-byte stream)))
      (#xE (let ((lsb (%read-byte stream))
                 (msb (%read-byte stream)))
             (make-instance 'pitch-bend-message
                            :status status :channel channel
                            :value (logior lsb (ash msb 7))))))))

(defun %parse-meta-event (stream)
  (let ((meta-type (%read-byte stream))
        (len (%read-vlq stream)))
    (ecase meta-type
      (#x00 (let ((data (%read-bytes stream len)))
              (make-instance 'sequence-number-message
                             :status #xFF
                             :sequence (if (= len 2)
                                           (logior (ash (aref data 0) 8)
                                                   (aref data 1))
                                           0))))
      (#x01 (make-instance 'text-message :status #xFF
                           :text (%read-string stream len)))
      (#x02 (make-instance 'copyright-message :status #xFF
                           :text (%read-string stream len)))
      (#x03 (make-instance 'sequence/track-name-message :status #xFF
                           :text (%read-string stream len)))
      (#x04 (make-instance 'instrument-name-message :status #xFF
                           :text (%read-string stream len)))
      (#x05 (make-instance 'lyric-message :status #xFF
                           :text (%read-string stream len)))
      (#x06 (make-instance 'marker-message :status #xFF
                           :text (%read-string stream len)))
      (#x07 (make-instance 'cue-point-message :status #xFF
                           :text (%read-string stream len)))
      (#x20 (make-instance 'channel-prefix-message :status #xFF
                           :channel (%read-byte stream)))
      (#x2F (make-instance 'end-of-track-message :status #xFF))
      (#x51 (make-instance 'tempo-message :status #xFF
                           :tempo (logior (ash (%read-byte stream) 16)
                                          (ash (%read-byte stream) 8)
                                          (%read-byte stream))))
      (#x54 (make-instance 'smpte-offset-message :status #xFF
                           :hr (%read-byte stream)
                           :mn (%read-byte stream)
                           :se (%read-byte stream)
                           :fr (%read-byte stream)
                           :ff (%read-byte stream)))
      (#x58 (make-instance 'time-signature-message :status #xFF
                           :numerator (%read-byte stream)
                           :denominator (%read-byte stream)
                           :cc (%read-byte stream)
                           :bb (%read-byte stream)))
      (#x59 (let ((sf (%read-byte stream))
                  (mi (%read-byte stream)))
              (make-instance 'key-signature-message :status #xFF
                             :sf (if (> sf 127) (- sf 256) sf)
                             :mi mi)))
      (#x7F (make-instance 'meta-message :status #xFF
                           :data (%read-bytes stream len))))))

(defun %parse-sysex-event (stream)
  (let ((len (%read-vlq stream)))
    (make-instance 'system-exclusive-message :status #xF0
                   :data (%read-bytes stream len))))

(defun %parse-event (stream running-status)
  "Read one timed event from STREAM. Returns (values message new-running-status).
RUNNING-STATUS is the last channel status byte or NIL."
  (let ((delta (%read-vlq stream)))
    (multiple-value-bind (event new-running-status)
        (let ((byte (%peek-byte stream)))
          (cond
            ((= byte #xFF)
             (%read-byte stream)
             (values (%parse-meta-event stream) nil))
            ((= byte #xF0)
             (%read-byte stream)
             (values (%parse-sysex-event stream) nil))
            ((>= byte #x80)
             (let ((status (%read-byte stream)))
               (values (%parse-channel-event stream status) status)))
            (running-status
             (values (%parse-channel-event stream running-status) running-status))
            (t
             (error 'unknown-event :status byte))))
      (setf (message-time event) delta)
      (values event new-running-status))))

(defun %parse-track (stream end)
  "Parse a track as a list of messages with absolute times.
END is the absolute position of the track's end in the stream."
  (loop :with running-status := nil
        :with absolute-time := 0
        :until (%stream-end-p stream end)
        :collect (multiple-value-bind (event new-rs)
                     (%parse-event stream running-status)
                   (setf running-status new-rs)
                   (incf absolute-time (message-time event))
                   (setf (message-time event) absolute-time)
                   event)))

;;; Top-level reader

(declaim (inline %read-u16-be %read-u32-be))
(defun %read-u16-be (stream)
  (logior (ash (%read-byte stream) 8) (%read-byte stream)))

(defun %read-u32-be (stream)
  (logior (ash (%read-byte stream) 24)
           (ash (%read-byte stream) 16)
           (ash (%read-byte stream) 8)
           (%read-byte stream)))

(defun %read-magic (stream len)
  (let ((bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below len
          :do (setf (aref bytes i) (%read-byte stream)))
    bytes))

(defun read-midi-file (filename)
  "Read an entire Standard MIDI File from FILENAME.
Returns a MIDIFILE instance with FORMAT, DIVISION, and TRACKS.
Tracks are lists of MESSAGE instances with absolute times."
  (with-open-file (input filename :direction :input
                         :element-type '(unsigned-byte 8))
    (let* ((data (make-array (file-length input)
                             :element-type '(unsigned-byte 8)))
           (n (read-sequence data input))
           (stream (make-midi-stream :data data :pos 0)))
      (declare (ignore n))
      ;; Parse header: MThd magic (4) + length (4) + format (2) + num-tracks (2) + division (2)
      (let ((magic (%read-magic stream 4)))
        (unless (string= (map 'string #'code-char magic) "MThd")
          (error 'header :header "MThd")))
      (let ((header-len (%read-u32-be stream)))
        (declare (ignore header-len)))
      (let ((format (%read-u16-be stream))
            (num-tracks (%read-u16-be stream))
            (division (%read-u16-be stream)))
        ;; Parse tracks
        (make-instance 'midifile
                       :format format
                       :division division
                       :tracks (loop :repeat num-tracks
                                     :collect (let ((magic (%read-magic stream 4)))
                                                (unless (string= (map 'string #'code-char magic) "MTrk")
                                                  (error 'header :header "MTrk"))
                                                (let ((track-len (%read-u32-be stream)))
                                                  (let ((track-end (+ (midi-stream-pos stream) track-len)))
                                                    (prog1 (%parse-track stream track-end)
                                                      (setf (midi-stream-pos stream) track-end)))))))))))
