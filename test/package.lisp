(defpackage #:cl-smf.test
  (:use #:cl #:parachute #:parsonic #:binstruct)
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:smf
   #:vlq #:read-file #:write-file
   #:header-format #:header-division
   #:file-header #:file-tracks #:track-events)
  (:import-from #:binstruct #:parser #:parser-run)
  (:import-from #:flexi-streams
   #:make-in-memory-input-stream
   #:make-in-memory-output-stream
   #:get-output-stream-sequence)
  (:nicknames #:smf.test))

(in-package #:smf.test)

(define-test suite)

(define-test vlq :parent suite
  (let ((read-vlq (lambda (bytes)
                    (let ((binstruct::*positions* nil))
                      (parser-run (parser (vlq))
                                  (coerce bytes '(simple-array (unsigned-byte 8) (*))))))))
    (is = 0 (funcall read-vlq #(#x00)))
    (is = 127 (funcall read-vlq #(#x7F)))
    (is = 128 (funcall read-vlq #(#x81 #x00)))
    (is = 840 (funcall read-vlq #(#x86 #x48))))
  (loop :for value :in '(0 127 128 840 16383 16384 100000)
        :for bytes := (let ((binstruct::*positions* nil))
                        (multiple-value-bind (output vector)
                            (binstruct::vector-emitter-output)
                          (smf::emitter/vlq output value)
                          vector))
        :do (is = value
                 (let ((binstruct::*positions* nil))
                   (parser-run (parser (vlq)) bytes)))))

;; A minimal self-contained Standard MIDI File (format 1, 2 tracks,
;; division 48) embedded as a literal byte vector.  Track 0 holds the
;; tempo and time-signature meta events; track 1 holds a track-name
;; meta event, a program-change, a note-on/note-off pair, a text meta
;; event, and the end-of-track marker.  Exercising channel events,
;; meta events and the MThd/MTrk framing is enough to validate the
;; reader and writer without depending on external .mid files.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bytes*
    (make-array
     87 :element-type '(unsigned-byte 8)
     :initial-contents
     '(#x4D #x54 #x68 #x64 #x00 #x00 #x00 #x06 #x00 #x01 #x00 #x02 #x00 #x30 #x4D #x54
       #x72 #x6B #x00 #x00 #x00 #x13 #x00 #xFF #x51 #x03 #x07 #xA1 #x20 #x00 #xFF #x58
       #x04 #x04 #x02 #x18 #x08 #x00 #xFF #x2F #x00 #x4D #x54 #x72 #x6B #x00 #x00 #x00
       #x26 #x00 #xFF #x03 #x0A #x54 #x65 #x73 #x74 #x20 #x54 #x72 #x61 #x63 #x6B #x00
       #xC0 #x00 #x00 #x90 #x3C #x64 #x30 #x80 #x3C #x00 #x00 #xFF #x01 #x05 #x68 #x65
       #x6C #x6C #x6F #x00 #xFF #x2F #x00))))

(defun input-stream ()
  "Return an in-memory binary input stream over the embedded SMF bytes."
  (make-in-memory-input-stream *bytes*))

(define-test read :parent suite
  (let ((smf (read-file (input-stream))))
    (is = 1 (header-format (file-header smf)))
    (is = 48 (header-division (file-header smf)))
    (is = 2 (length (file-tracks smf)))
    (true (plusp (length (track-events (aref (file-tracks smf) 0))))
         "track 0 should have events")
    (true (plusp (length (track-events (aref (file-tracks smf) 1))))
         "track 1 should have events")))

(define-test write :parent suite
  (let* ((smf (read-file (input-stream)))
         (out-stream (make-in-memory-output-stream))
         (result-stream (write-file out-stream smf))
         (out-bytes (get-output-stream-sequence result-stream)))
    (let ((smf2 (read-file (make-in-memory-input-stream out-bytes))))
      (is = (header-format (file-header smf))
           (header-format (file-header smf2)))
      (is = (header-division (file-header smf))
           (header-division (file-header smf2)))
      (is = (length (file-tracks smf))
           (length (file-tracks smf2)))
      (loop :for track :across (file-tracks smf)
            :for track2 :across (file-tracks smf2)
            :do (is = (length (track-events track))
                     (length (track-events track2)))))))
