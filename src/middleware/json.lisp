(defpackage murja.middleware.json
  (:use :cl))

(in-package :murja.middleware.json)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((result (funcall next)))
    (if result
	result
	(progn (setf (hunchentoot:return-code*) 404) ""))))
	
