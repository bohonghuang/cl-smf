(defpackage #:cl-midi.test
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:import-from #:binstruct
   #:vlq-base128-be #:read-midi-file
   #:midi-header-format #:midi-header-division
   #:midi-file-header #:midi-file-tracks #:midi-track-events
   #:parser #:parser-run)
  (:use #:cl #:parachute #:parsonic #:binstruct))

(in-package #:cl-midi.test)

(define-test suite)

(defmacro is-rw (test struct &body tests)
  (with-gensyms (input output)
    (let ((reader-eval (make-symbol (format nil "~A [~A]" struct '#:reader/eval)))
          (reader-compiled (make-symbol (format nil "~A [~A]" struct '#:reader/compiled)))
          (writer-eval (make-symbol (format nil "~A [~A]" struct '#:writer-eval)))
          (writer-compiled (make-symbol (format nil "~A [~A]" struct '#:writer-compiled))))
      `(let* ((,reader-eval (lambda (,input) (parser-run (parser ,struct) ,input)))
              (,reader-compiled (lambda (,input) (parser-run (parser ,struct) (the (simple-array (unsigned-byte 8) (*)) ,input)))))
         ,@(loop :for (input expected) :in tests
                 :for bytes := (make-symbol (format nil "~A" input))
                 :collect (once-only (expected)
                            `(progn
                               (let ((,bytes (coerce ,input '(simple-array (unsigned-byte 8) (*)))))
                                 (let ((binstruct::*positions* nil))
                                   (is ,test ,expected (funcall ,reader-eval ,bytes)))
                                 (let ((binstruct::*positions* nil))
                                   (is ,test ,expected (funcall ,reader-compiled ,bytes)))
                                 (let ((,output (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
                                   (let ((binstruct::*positions* nil))
                                     ,(binstruct::expand-writer-type-unit struct :output `(binstruct::vector-emitter-output ,output) :value expected)
                                     (binstruct::flush-pointer-positions))
                                   (let* ((,writer-eval (coerce ,output '(simple-array (unsigned-byte 8) (*))))
                                          (,writer-compiled ,writer-eval))
                                     (let ((binstruct::*positions* nil))
                                       (is ,test ,expected (funcall ,reader-eval ,writer-eval)))
                                     (let ((binstruct::*positions* nil))
                                       (is ,test ,expected (funcall ,reader-compiled ,writer-compiled)))))))))))))

(defmacro is-rw-equalp (struct &body tests)
  `(is-rw equalp ,struct . ,tests))

(define-test vlq :parent suite
  (let ((read-vlq (lambda (bytes)
                    (let ((binstruct::*positions* nil))
                      (parser-run (parser (vlq-base128-be))
                                  (coerce bytes '(simple-array (unsigned-byte 8) (*))))))))
    (is = 0 (funcall read-vlq #(#x00)))
    (is = 127 (funcall read-vlq #(#x7F)))
    (is = 128 (funcall read-vlq #(#x81 #x00)))
    (is = 840 (funcall read-vlq #(#x86 #x48)))))

(define-test midi-file-read :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-midi/test/mid/*.mid")
        :do (let* ((arr (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
                          (let ((a (make-array (file-length s) :element-type '(unsigned-byte 8))))
                            (read-sequence a s) a)))
                   (midi (read-midi-file arr)))
              (is = 1 (midi-header-format (midi-file-header midi)))
              (is = 48 (midi-header-division (midi-file-header midi)))
              (true (plusp (length (midi-file-tracks midi))))
              (true (plusp (length (midi-track-events (aref (midi-file-tracks midi) 0))))))))
