(defpackage murja.session
  (:use :cl)
  (:import-from :murja.session.db :assert-ownership :set-session-val* :get-session-val*)
  (:import-from :murja.middleware.auth :*user* :*session-key*))

(in-package :murja.session)

(defun set-session-value (key val)
  (assert *user*)
  (assert *session-key*)
  (let ((user-id (gethash "id" *user*)))
    (assert-ownership user-id *session-key*)
    
    (set-session-val* *session-key* (str:downcase (symbol-name key)) val)
    (setf (hunchentoot:session-value key) val)))


(defun get-session-value (key)  
  (assert *user*)
  (assert *session-key*)
  (let* ((user-id (gethash "id" *user*))
	 (res (coerce (get-session-val* user-id key) 'list)))
    (when res
      (gethash "val" (first res)))))
    
    
