(defpackage murja.session.db
  (:use :cl)
  (:import-from :halisql :defqueries)
  (:export :ensure-username-session* :assert-ownership
	   :assert-ownership-username :all-session-vals
	   :login-query-session*
	   :set-session-val* :ensure-session*
	   :insert-session* :get-session-val*)) 	   

(in-package :murja.session.db)

(defqueries "session-fns")

(defun assert-ownership (user-id session-key)
  (let ((session (ensure-session* user-id session-key)))
    (assert session)))

(defun assert-ownership-username (username session-key)
  (let ((session (ensure-username-session* username session-key)))
    (assert session)))
