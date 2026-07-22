(defsystem cl-midi
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "MIT"
  :description "MIDI file reader/writer for Common Lisp."
  :depends-on (#:binstruct #:closer-mop)
  :serial t
  :components ((:file "src/midi"))
  :in-order-to ((test-op (test-op #:cl-midi/test))))

(defsystem cl-midi/test
  :depends-on (#:cl-midi #:parachute)
  :pathname "test/"
  :components ((:file "cl-midi-test"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test '#:cl-midi.test)))
