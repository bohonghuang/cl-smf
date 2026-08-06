(defpackage #:cl-smf.test
  (:use #:cl #:parachute #:parsonic #:binstruct)
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:binstruct #:parser #:parser-run)
  (:import-from #:flexi-streams
   #:make-in-memory-input-stream
   #:make-in-memory-output-stream
   #:get-output-stream-sequence)
  (:nicknames #:smf.test))

(in-package #:smf.test)

(define-test suite)

(defparameter *bytes*
  (coerce
   '(#x4D #x54 #x68 #x64 #x00 #x00 #x00 #x06 #x00 #x01 #x00 #x02 #x00 #x30 #x4D #x54
     #x72 #x6B #x00 #x00 #x00 #x13 #x00 #xFF #x51 #x03 #x07 #xA1 #x20 #x00 #xFF #x58
     #x04 #x04 #x02 #x18 #x08 #x00 #xFF #x2F #x00 #x4D #x54 #x72 #x6B #x00 #x00 #x00
     #x26 #x00 #xFF #x03 #x0A #x54 #x65 #x73 #x74 #x20 #x54 #x72 #x61 #x63 #x6B #x00
     #xC0 #x00 #x00 #x90 #x3C #x64 #x30 #x80 #x3C #x00 #x00 #xFF #x01 #x05 #x68 #x65
     #x6C #x6C #x6F #x00 #xFF #x2F #x00)
   '(simple-array (unsigned-byte 8) (*))))

(define-test read :parent suite
  (let ((smf (smf:read *bytes*)))
    (is = 1 (smf::header-format (smf::file-header smf)))
    (is = 48 (smf::header-division (smf::file-header smf)))
    (is = 2 (length (smf::file-tracks smf)))
    (true (plusp (length (smf::track-events (aref (smf::file-tracks smf) 0))))
         "track 0 should have events")
    (true (plusp (length (smf::track-events (aref (smf::file-tracks smf) 1))))
         "track 1 should have events")))

(define-test write :parent suite
  (let* ((smf (smf:read *bytes*))
         (out-bytes (smf:write smf nil)))
    (let ((smf2 (smf:read (make-in-memory-input-stream out-bytes))))
      (is = (smf::header-format (smf::file-header smf))
           (smf::header-format (smf::file-header smf2)))
      (is = (smf::header-division (smf::file-header smf))
           (smf::header-division (smf::file-header smf2)))
      (is = (length (smf::file-tracks smf))
           (length (smf::file-tracks smf2)))
      (loop :for track :across (smf::file-tracks smf)
            :for track2 :across (smf::file-tracks smf2)
            :do (is = (length (smf::track-events track))
                     (length (smf::track-events track2)))))))
