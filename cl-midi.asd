(asdf:defsystem #:cl-midi
  :depends-on (#:binstruct)
  :components
  ((:file "src/vlq")
   (:file "src/midi" :depends-on ("src/vlq"))))

(asdf:defsystem #:cl-midi/test
  :depends-on (#:cl-midi #:parachute)
  :components
  ((:file "test/cl-midi-test")))
