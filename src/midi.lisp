(in-package #:binstruct)

;;; Channel event structs

(defbinstruct midi-note-off-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #x80))
                            (satisfies (unsigned-byte 8) (curry #'eql #x81))
                            (satisfies (unsigned-byte 8) (curry #'eql #x82))
                            (satisfies (unsigned-byte 8) (curry #'eql #x83))
                            (satisfies (unsigned-byte 8) (curry #'eql #x84))
                            (satisfies (unsigned-byte 8) (curry #'eql #x85))
                            (satisfies (unsigned-byte 8) (curry #'eql #x86))
                            (satisfies (unsigned-byte 8) (curry #'eql #x87))
                            (satisfies (unsigned-byte 8) (curry #'eql #x88))
                            (satisfies (unsigned-byte 8) (curry #'eql #x89))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8A))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8B))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8C))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8D))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8E))
                            (satisfies (unsigned-byte 8) (curry #'eql #x8F)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #x80)))
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(defbinstruct midi-note-on-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #x90))
                            (satisfies (unsigned-byte 8) (curry #'eql #x91))
                            (satisfies (unsigned-byte 8) (curry #'eql #x92))
                            (satisfies (unsigned-byte 8) (curry #'eql #x93))
                            (satisfies (unsigned-byte 8) (curry #'eql #x94))
                            (satisfies (unsigned-byte 8) (curry #'eql #x95))
                            (satisfies (unsigned-byte 8) (curry #'eql #x96))
                            (satisfies (unsigned-byte 8) (curry #'eql #x97))
                            (satisfies (unsigned-byte 8) (curry #'eql #x98))
                            (satisfies (unsigned-byte 8) (curry #'eql #x99))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9A))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9B))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9C))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9D))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9E))
                            (satisfies (unsigned-byte 8) (curry #'eql #x9F)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #x90)))
  (note 0 :type (unsigned-byte 8))
  (velocity 0 :type (unsigned-byte 8)))

(defbinstruct midi-poly-pressure-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #xA0))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA1))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA2))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA3))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA4))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA5))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA6))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA7))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA8))
                            (satisfies (unsigned-byte 8) (curry #'eql #xA9))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAA))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAB))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAC))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAD))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAE))
                            (satisfies (unsigned-byte 8) (curry #'eql #xAF)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #xA0)))
  (note 0 :type (unsigned-byte 8))
  (pressure 0 :type (unsigned-byte 8)))

(defbinstruct midi-controller-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #xB0))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB1))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB2))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB3))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB4))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB5))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB6))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB7))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB8))
                            (satisfies (unsigned-byte 8) (curry #'eql #xB9))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBA))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBB))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBC))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBD))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBE))
                            (satisfies (unsigned-byte 8) (curry #'eql #xBF)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #xB0)))
  (controller 0 :type (unsigned-byte 8))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct midi-program-change-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #xC0))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC1))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC2))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC3))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC4))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC5))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC6))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC7))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC8))
                            (satisfies (unsigned-byte 8) (curry #'eql #xC9))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCA))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCB))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCC))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCD))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCE))
                            (satisfies (unsigned-byte 8) (curry #'eql #xCF)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #xC0)))
  (program 0 :type (unsigned-byte 8)))

(defbinstruct midi-channel-pressure-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #xD0))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD1))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD2))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD3))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD4))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD5))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD6))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD7))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD8))
                            (satisfies (unsigned-byte 8) (curry #'eql #xD9))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDA))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDB))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDC))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDD))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDE))
                            (satisfies (unsigned-byte 8) (curry #'eql #xDF)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #xD0)))
  (pressure 0 :type (unsigned-byte 8)))

(defbinstruct midi-pitch-bend-event ()
  (channel 0 :type (map (or (satisfies (unsigned-byte 8) (curry #'eql #xE0))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE1))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE2))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE3))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE4))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE5))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE6))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE7))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE8))
                            (satisfies (unsigned-byte 8) (curry #'eql #xE9))
                            (satisfies (unsigned-byte 8) (curry #'eql #xEA))
                            (satisfies (unsigned-byte 8) (curry #'eql #xEB))
                            (satisfies (unsigned-byte 8) (curry #'eql #xEC))
                            (satisfies (unsigned-byte 8) (curry #'eql #xED))
                            (satisfies (unsigned-byte 8) (curry #'eql #xEE))
                            (satisfies (unsigned-byte 8) (curry #'eql #xEF)))
                      (curry #'ldb (byte 4 0))
                      (rcurry #'dpb (byte 4 0) #xE0)))
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

;;; Mode message structs — share controller status nibble #xB

(defbinstruct midi-mode-message ()
  (nil 0 :type (or (satisfies (unsigned-byte 8) (curry #'eql #xB0))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB1))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB2))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB3))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB4))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB5))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB6))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB7))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB8))
                   (satisfies (unsigned-byte 8) (curry #'eql #xB9))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBA))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBB))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBC))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBD))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBE))
                   (satisfies (unsigned-byte 8) (curry #'eql #xBF)))))

(defbinstruct (midi-reset-all-controllers-event (:include midi-mode-message)) ()
  (nil #x79 :type (satisfies (unsigned-byte 8) (curry #'eql #x79)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-local-control-event (:include midi-mode-message)) ()
  (nil #x7A :type (satisfies (unsigned-byte 8) (curry #'eql #x7A)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-all-notes-off-event (:include midi-mode-message)) ()
  (nil #x7B :type (satisfies (unsigned-byte 8) (curry #'eql #x7B)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-omni-mode-off-event (:include midi-mode-message)) ()
  (nil #x7C :type (satisfies (unsigned-byte 8) (curry #'eql #x7C)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-omni-mode-on-event (:include midi-mode-message)) ()
  (nil #x7D :type (satisfies (unsigned-byte 8) (curry #'eql #x7D)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-mono-mode-on-event (:include midi-mode-message)) ()
  (nil #x7E :type (satisfies (unsigned-byte 8) (curry #'eql #x7E)))
  (value 0 :type (unsigned-byte 8)))

(defbinstruct (midi-poly-mode-on-event (:include midi-mode-message)) ()
  (nil #x7F :type (satisfies (unsigned-byte 8) (curry #'eql #x7F)))
  (value 0 :type (unsigned-byte 8)))

;;; System common message structs

(defbinstruct midi-timing-code-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF1)))
  (code 0 :type (unsigned-byte 8)))

(defbinstruct midi-song-position-pointer-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF2)))
  (lsb 0 :type (unsigned-byte 8))
  (msb 0 :type (unsigned-byte 8)))

(defbinstruct midi-song-select-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF3)))
  (song 0 :type (unsigned-byte 8)))

(defbinstruct midi-tune-request-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF6))))

;;; System real-time message structs

(defbinstruct midi-timing-clock-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF8))))

(defbinstruct midi-start-sequence-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xFA))))

(defbinstruct midi-continue-sequence-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xFB))))

(defbinstruct midi-stop-sequence-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xFC))))

(defbinstruct midi-active-sensing-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xFE))))

;;; SysEx event structs

(defbinstruct midi-sysex-event ()
  (nil #xF0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF0)))
  (len 0 :type vlq-base128-be)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

(defbinstruct midi-authorization-sysex-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xF7)))
  (len 0 :type vlq-base128-be)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; Meta event structs — fine-grained, :include from midi-meta-event parent

(defbinstruct midi-meta-event ()
  (nil 0 :type (satisfies (unsigned-byte 8) (curry #'eql #xFF))))

(defbinstruct (midi-sequence-number-event (:include midi-meta-event)) ()
  (nil #x00 :type (satisfies (unsigned-byte 8) (curry #'eql #x00)))
  (len 0 :type vlq-base128-be)
  (ssss 0 :type (unsigned-byte 16)))

(defbinstruct (midi-text-event (:include midi-meta-event)) ()
  (nil #x01 :type (satisfies (unsigned-byte 8) (curry #'eql #x01)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-copyright-event (:include midi-meta-event)) ()
  (nil #x02 :type (satisfies (unsigned-byte 8) (curry #'eql #x02)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-sequence-track-name-event (:include midi-meta-event)) ()
  (nil #x03 :type (satisfies (unsigned-byte 8) (curry #'eql #x03)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-instrument-name-event (:include midi-meta-event)) ()
  (nil #x04 :type (satisfies (unsigned-byte 8) (curry #'eql #x04)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-lyric-event (:include midi-meta-event)) ()
  (nil #x05 :type (satisfies (unsigned-byte 8) (curry #'eql #x05)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-marker-event (:include midi-meta-event)) ()
  (nil #x06 :type (satisfies (unsigned-byte 8) (curry #'eql #x06)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-cue-point-event (:include midi-meta-event)) ()
  (nil #x07 :type (satisfies (unsigned-byte 8) (curry #'eql #x07)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-program-name-event (:include midi-meta-event)) ()
  (nil #x08 :type (satisfies (unsigned-byte 8) (curry #'eql #x08)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-device-name-event (:include midi-meta-event)) ()
  (nil #x09 :type (satisfies (unsigned-byte 8) (curry #'eql #x09)))
  (len 0 :type vlq-base128-be)
  (text "" :type (simple-base-string len)))

(defbinstruct (midi-channel-prefix-event (:include midi-meta-event)) ()
  (nil #x20 :type (satisfies (unsigned-byte 8) (curry #'eql #x20)))
  (len 0 :type vlq-base128-be)
  (cc 0 :type (unsigned-byte 8)))

(defbinstruct (midi-end-of-track-event (:include midi-meta-event)) ()
  (nil #x2F :type (satisfies (unsigned-byte 8) (curry #'eql #x2F)))
  (len 0 :type vlq-base128-be))

(defbinstruct (midi-tempo-event (:include midi-meta-event)) ()
  (nil #x51 :type (satisfies (unsigned-byte 8) (curry #'eql #x51)))
  (len 0 :type vlq-base128-be)
  (tttttt 0 :type (unsigned-byte 24)))

(defbinstruct (midi-smpte-offset-event (:include midi-meta-event)) ()
  (nil #x54 :type (satisfies (unsigned-byte 8) (curry #'eql #x54)))
  (len 0 :type vlq-base128-be)
  (hr 0 :type (unsigned-byte 8))
  (mn 0 :type (unsigned-byte 8))
  (se 0 :type (unsigned-byte 8))
  (fr 0 :type (unsigned-byte 8))
  (ff 0 :type (unsigned-byte 8)))

(defbinstruct (midi-time-signature-event (:include midi-meta-event)) ()
  (nil #x58 :type (satisfies (unsigned-byte 8) (curry #'eql #x58)))
  (len 0 :type vlq-base128-be)
  (nn 0 :type (unsigned-byte 8))
  (dd 0 :type (unsigned-byte 8))
  (cc 0 :type (unsigned-byte 8))
  (bb 0 :type (unsigned-byte 8)))

(defbinstruct (midi-key-signature-event (:include midi-meta-event)) ()
  (nil #x59 :type (satisfies (unsigned-byte 8) (curry #'eql #x59)))
  (len 0 :type vlq-base128-be)
  (sf 0 :type (signed-byte 8))
  (mi 0 :type (unsigned-byte 8)))

(defbinstruct (midi-sequencer-specific-event (:include midi-meta-event)) ()
  (nil #x7F :type (satisfies (unsigned-byte 8) (curry #'eql #x7F)))
  (len 0 :type vlq-base128-be)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (len))))

;;; Track event — parametric with position sentinel

(defbinstruct midi-track-event (end)
  (nil 0 :type (satisfies position (rcurry #'< end)))
  (delta 0 :type vlq-base128-be)
  (event nil :type (or midi-note-off-event midi-note-on-event midi-poly-pressure-event
                       midi-reset-all-controllers-event midi-local-control-event
                       midi-all-notes-off-event midi-omni-mode-off-event
                       midi-omni-mode-on-event midi-mono-mode-on-event midi-poly-mode-on-event
                       midi-controller-event
                       midi-program-change-event midi-channel-pressure-event midi-pitch-bend-event
                       midi-timing-code-event midi-song-position-pointer-event midi-song-select-event
                       midi-tune-request-event
                       midi-timing-clock-event midi-start-sequence-event
                       midi-continue-sequence-event midi-stop-sequence-event midi-active-sensing-event
                       midi-sysex-event midi-authorization-sysex-event
                       midi-sequence-number-event
                       midi-copyright-event midi-sequence-track-name-event
                       midi-instrument-name-event midi-lyric-event midi-marker-event
                       midi-cue-point-event midi-program-name-event midi-device-name-event
                       midi-text-event
                       midi-channel-prefix-event midi-end-of-track-event midi-tempo-event
                       midi-smpte-offset-event midi-time-signature-event midi-key-signature-event
                       midi-sequencer-specific-event)))

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
