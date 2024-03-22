(defpackage murja.middleware.json
  (:use :cl))

(in-package :murja.middleware.json)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))
