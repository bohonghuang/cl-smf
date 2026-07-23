(defpackage #:cl-smf.test
  (:use #:cl #:parachute #:parsonic #:binstruct)
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:smf
   #:vlq #:read-smf #:write-smf
   #:smf-header-format #:smf-header-division
   #:smf-header #:smf-tracks #:smf-track-events)
  (:import-from #:binstruct #:parser #:parser-run)
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
  (defparameter *smf-bytes*
    (make-array
     87 :element-type '(unsigned-byte 8)
     :initial-contents
     '(#x4D #x54 #x68 #x64 #x00 #x00 #x00 #x06 #x00 #x01 #x00 #x02 #x00 #x30 #x4D #x54
       #x72 #x6B #x00 #x00 #x00 #x13 #x00 #xFF #x51 #x03 #x07 #xA1 #x20 #x00 #xFF #x58
       #x04 #x04 #x02 #x18 #x08 #x00 #xFF #x2F #x00 #x4D #x54 #x72 #x6B #x00 #x00 #x00
       #x26 #x00 #xFF #x03 #x0A #x54 #x65 #x73 #x74 #x20 #x54 #x72 #x61 #x63 #x6B #x00
       #xC0 #x00 #x00 #x90 #x3C #x64 #x30 #x80 #x3C #x00 #x00 #xFF #x01 #x05 #x68 #x65
       #x6C #x6C #x6F #x00 #xFF #x2F #x00))))

(defun smf-from-bytes (bytes)
  "Write BYTES to a fresh temporary file and return its pathname."
  (let ((path (merge-pathnames (make-pathname :name (format nil "cl-smf-test-~A" (random most-positive-fixnum))
                                              :type "mid")
                               #P"/tmp/")))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
      (write-sequence bytes out))
    path))

(define-test smf-read :parent suite
  (let ((path (smf-from-bytes *smf-bytes*)))
    (unwind-protect
         (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
           (let ((smf (read-smf s)))
             (is = 1 (smf-header-format (smf-header smf)))
             (is = 48 (smf-header-division (smf-header smf)))
             (is = 2 (length (smf-tracks smf)))
             (true (plusp (length (smf-track-events (aref (smf-tracks smf) 0))))
                  "track 0 should have events")
             (true (plusp (length (smf-track-events (aref (smf-tracks smf) 1))))
                  "track 1 should have events")))
      (delete-file path))))

(define-test smf-write :parent suite
  (let ((path (smf-from-bytes *smf-bytes*)))
    (unwind-protect
         (let ((smf (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
                      (read-smf s))))
           (let ((out-path (merge-pathnames (make-pathname :name (format nil "cl-smf-test-out-~A" (random most-positive-fixnum))
                                                         :type "mid")
                                              #P"/tmp/")))
             (with-open-file (out out-path :direction :output :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
               (write-smf out smf))
             (unwind-protect
                  (with-open-file (r out-path :direction :input :element-type '(unsigned-byte 8))
                    (let ((smf2 (read-smf r)))
                      (is = (smf-header-format (smf-header smf))
                           (smf-header-format (smf-header smf2)))
                      (is = (smf-header-division (smf-header smf))
                           (smf-header-division (smf-header smf2)))
                      (is = (length (smf-tracks smf))
                           (length (smf-tracks smf2)))
                      (loop :for track :across (smf-tracks smf)
                            :for track2 :across (smf-tracks smf2)
                            :do (is = (length (smf-track-events track))
                                     (length (smf-track-events track2))))))
               (when (probe-file out-path) (delete-file out-path)))))
      (delete-file path))))
