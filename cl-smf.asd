(defsystem cl-smf
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Standard MIDI File (SMF) reader/writer for Common Lisp."
  :depends-on (#:binstruct #:closer-mop)
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op #:cl-smf/test))))

(defsystem cl-smf/test
  :depends-on (#:cl-smf #:parachute)
  :pathname "test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:smf.test))))
