(in-package #:binstruct)

;;; Channel event structs

(defbinstruct midi-note-off-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #x8))))
  (note 0 :type (unsigned-byte 7))
  (velocity 0 :type (unsigned-byte 7)))

(defbinstruct midi-note-on-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #x9))))
  (note 0 :type (unsigned-byte 7))
  (velocity 0 :type (unsigned-byte 7)))

(defbinstruct midi-poly-pressure-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #xA))))
  (note 0 :type (unsigned-byte 7))
  (pressure 0 :type (unsigned-byte 7)))

(defbinstruct midi-controller-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #xB))))
  (controller 0 :type (unsigned-byte 7))
  (value 0 :type (unsigned-byte 7)))

(defbinstruct midi-program-change-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #xC))))
  (program 0 :type (unsigned-byte 7)))

(defbinstruct midi-channel-pressure-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #xD))))
  (pressure 0 :type (unsigned-byte 7)))

(defbinstruct midi-pitch-bend-event ()
  (status 0 :type (satisfies (unsigned-byte 8) (lambda (b) (eql (ldb (byte 4 4) b) #xE))))
  (lsb 0 :type (unsigned-byte 7))
  (msb 0 :type (unsigned-byte 7)))

;;; Meta event type enum + struct

(defbinenum (midi-meta-type (:type (unsigned-byte 8))) ()
  (sequence-number #x00) (text-event #x01) (copyright #x02) (sequence-track-name #x03)
  (instrument-name #x04) (lyric-text #x05) (marker-text #x06) (cue-point #x07)
  (midi-channel-prefix-assignment #x20) (end-of-track #x2F) (tempo #x51)
  (smpte-offset #x54) (time-signature #x58) (key-signature #x59) (sequencer-specific-event #x7F))

(defbinstruct midi-meta-event ()
  (status #xFF :type (satisfies (unsigned-byte 8) (curry #'eql #xFF)))
  (meta-type 'sequence-number :type midi-meta-type)
  (len 0 :type vlq-base128-be)
  (body (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; SysEx event struct

(defbinstruct midi-sysex-event ()
  (status #xF0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF0)))
  (len 0 :type vlq-base128-be)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; Track event — parametric with position sentinel

(defbinstruct midi-track-event (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq-base128-be)
  (event nil :type (or midi-note-off-event midi-note-on-event midi-poly-pressure-event
                       midi-controller-event midi-program-change-event midi-channel-pressure-event
                       midi-pitch-bend-event midi-meta-event midi-sysex-event)))

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

(defbinio midi-file)
