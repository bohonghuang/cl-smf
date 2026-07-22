(defpackage #:cl-midi.test
  (:import-from #:midi
   #:read-midi-file #:midifile
   #:midifile-format #:midifile-tracks #:midifile-division
   #:message #:note-off-message #:note-on-message
   #:control-change-message #:program-change-message
   #:meta-message #:tempo-message #:end-of-track-message
   #:text-message #:sequence/track-name-message #:time-signature-message
   #:key-signature-message #:system-exclusive-message
   #:message-time #:message-channel #:message-key #:message-velocity
   #:message-program #:message-tempo #:message-controller #:message-value
   #:message-text)
  (:use #:cl #:parachute))

(in-package #:cl-midi.test)

(define-test suite)

(define-test midifile-structure :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-midi/test/mid/*.mid")
        :do (let ((midi (read-midi-file file)))
              (is = 1 (midifile-format midi))
              (is = 48 (midifile-division midi))
              (true (plusp (length (midifile-tracks midi))))
              (true (plusp (length (first (midifile-tracks midi))))))))

(define-test message-types :parent suite
  (let ((midi (read-midi-file #P"~/.quicklisp/local-projects/cl-midi/test/mid/SEQ_BA_CHANP.mid")))
    (let ((track0 (first (midifile-tracks midi))))
      ;; Track 0 should have meta events (tempo, text) and channel events
      (true (some #'(lambda (m) (typep m 'tempo-message)) track0))
      (true (some #'(lambda (m) (typep m 'text-message)) track0))
      ;; Some track should have note-on messages
      (true (some #'(lambda (m) (typep m 'note-on-message))
                  (alexandria:flatten (midifile-tracks midi)))))))

(define-test absolute-time :parent suite
  (loop :for file :in (directory #P"~/.quicklisp/local-projects/cl-midi/test/mid/*.mid")
        :do (let ((midi (read-midi-file file)))
              (loop :for track :in (midifile-tracks midi)
                    :do (loop :for msg :in track
                             :for prev := 0 :then (message-time msg)
                             :do (true (>= (message-time msg) prev)))))))

(define-test note-on-values :parent suite
  (let ((midi (read-midi-file #P"~/.quicklisp/local-projects/cl-midi/test/mid/SEQ_BA_CHANP.mid")))
    (loop :for track :in (midifile-tracks midi)
          :do (loop :for msg :in track
                    :when (typep msg 'note-on-message)
                      :do (true (<= 0 (message-key msg) 127))
                          (true (<= 0 (message-velocity msg) 127))
                          (true (<= 0 (message-channel msg) 15)))))

  (let ((midi (read-midi-file #P"~/.quicklisp/local-projects/cl-midi/test/mid/SEQ_BA_CHANP.mid")))
    (loop :for track :in (midifile-tracks midi)
          :do (loop :for msg :in track
                    :when (typep msg 'control-change-message)
                      :do (true (<= 0 (message-controller msg) 127))
                          (true (<= 0 (message-value msg) 127))))))
