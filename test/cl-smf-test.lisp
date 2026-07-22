(defpackage #:cl-smf.test
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:binstruct
   #:vlq #:read-smf #:write-smf
   #:smf-header-format #:smf-header-division
   #:smf-header #:smf-tracks #:smf-track-events
   #:parser #:parser-run)
  (:use #:cl #:parachute #:parsonic #:binstruct))

(in-package #:cl-smf.test)

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

(define-test smf-read :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-smf/test/mid/*.mid")
        :do (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
              (let ((smf (read-smf s)))
                (is = 1 (smf-header-format (smf-header smf)))
                (is = 48 (smf-header-division (smf-header smf)))
                (true (plusp (length (smf-tracks smf))))
                (true (plusp (length (smf-track-events (aref (smf-tracks smf) 0)))))))))

(define-test smf-write :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-smf/test/mid/*.mid")
        :do (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
              (let ((smf (read-smf s)))
                (with-open-file (out #P"/tmp/smf-test.mid" :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
                  (write-smf out smf))
                (with-open-file (r #P"/tmp/smf-test.mid" :direction :input
                                   :element-type '(unsigned-byte 8))
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
                                   (length (smf-track-events track2))))))))))
