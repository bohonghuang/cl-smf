(defsystem cl-smf
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "MIT"
  :description "Standard MIDI File (SMF) reader/writer for Common Lisp."
  :depends-on (#:binstruct #:closer-mop)
  :serial t
  :components ((:file "src/smf"))
  :in-order-to ((test-op (test-op #:cl-smf/test))))

(defsystem cl-smf/test
  :depends-on (#:cl-smf #:parachute)
  :pathname "test/"
  :components ((:file "cl-smf-test"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test '#:cl-smf.test)))
