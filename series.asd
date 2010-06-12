;;; -*- Mode: lisp -*-

(defpackage #:series-system
  (:use #:common-lisp))

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

(asdf:defsystem series-tests
    :depends-on (series)
    :components ((:file "s-test")))
