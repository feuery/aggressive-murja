(defpackage murja.middleware.json
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify))

(in-package :murja.middleware.json)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((result (funcall next)))
    (if result
	(if (stringp result)
	    result
	    (stringify result))
	(progn (setf (hunchentoot:return-code*) 404) ""))))
	
