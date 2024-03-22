(defpackage murja.middleware.auth
  (:use :cl :postmodern)
  (:import-from :murja.users.user-db :get-user-by-id)
  (:export :*user* :@can?))

(in-package :murja.middleware.auth)

(defvar *user* nil
  "A special variable for storing the logged in user (as defined in the db)")

(defun @authenticated (next)
  (let ((user-id (hunchentoot:session-value :logged-in-user-id)))
    (if user-id
	(let ((user (get-user-by-id user-id)))
	  (if (and user
		   (string= (hunchentoot:session-value :logged-in-username)
			    (gethash "username" user)))
	      (let ((*user* user))
		(funcall next))
	      (progn
		(setf (hunchentoot:return-code*) 401)
		"not authorized")))
	(progn
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defun @can? (ability next)
  (if (and *user*
	   (member ability 
		   (gethash "permissions" *user*)
		   :test #'string=))
      (funcall next)
      (progn
	(setf (hunchentoot:return-code*) 401)
	(format nil "you need to be able to ~a" ability))))



