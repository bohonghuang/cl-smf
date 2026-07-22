(asdf:defsystem #:cl-midi
  :components
  ((:file "src/cl-midi")))

(asdf:defsystem #:cl-midi/test
  :depends-on (#:cl-midi #:parachute #:alexandria)
  :components
  ((:file "test/cl-midi-test")))
