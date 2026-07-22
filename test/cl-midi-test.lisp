(defpackage #:cl-midi.test
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:binstruct
   #:vlq #:read-midi-file #:write-midi-file
   #:midi-header-format #:midi-header-division
   #:midi-file-header #:midi-file-tracks #:midi-track-events
   #:parser #:parser-run)
  (:use #:cl #:parachute #:parsonic #:binstruct))

(in-package #:cl-midi.test)

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
                          (binstruct::emitter/vlq output value)
                          vector))
        :do (is = value
                 (let ((binstruct::*positions* nil))
                   (parser-run (parser (vlq)) bytes)))))

(define-test midi-file-read :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-midi/test/mid/*.mid")
        :do (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
              (let ((midi (read-midi-file s)))
                (is = 1 (midi-header-format (midi-file-header midi)))
                (is = 48 (midi-header-division (midi-file-header midi)))
                (true (plusp (length (midi-file-tracks midi))))
                (true (plusp (length (midi-track-events (aref (midi-file-tracks midi) 0)))))))))
