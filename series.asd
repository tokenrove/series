;;; -*- Mode: lisp -*-

(defpackage #:series-system
  (:use #:common-lisp #:asdf))

(in-package #:series-system)

(asdf:defsystem series
    :description "See <http://series.sourceforge.net/>."
    :author "Richard C. Waters"
    :maintainer "See <http://series.sourceforge.net/>."
    :licence "MIT"
    :version "2.2.11"
    :serial t
    :components ((:file "s-package")
                 (:file "s-code")))

(defmethod perform ((op test-op) (c (eql (find-system :series))))
  (oos 'test-op 'series-tests))

(asdf:defsystem series-tests
  :depends-on (series)
  :version "2.2.11"			; Same as series
  :in-order-to ((test-op (load-op :series)))
  :components
  ((:file "s-test")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :series-tests))))
  nil)


(defmethod perform ((op test-op) (c (eql (find-system :series-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RT")))
      (error "TEST-OP failed for SERIES-TESTS")))
