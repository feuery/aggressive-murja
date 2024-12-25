(defpackage murja.middleware.auth
  (:use :cl :postmodern)
  (:import-from :murja.users.user-db :get-user-by-id)
  (:export :*session-key* :*user* :@can?))

(in-package :murja.middleware.auth)

(defvar *user* nil
  "A special variable for storing the logged in user (as defined in the db)")

(defvar *session-key* nil
  "A special var that stores a non-expired database session-key used for re-populating http-session upon its death")

(defun kw (str)
  (read-from-string (format nil ":~a" str)))

(defun populate-http-session (username session-key)
  (let ((session-vals (coerce (murja.session.db:all-session-vals username session-key) 'list)))
    (log:info "populating session for user ~a" username)
    (dolist (pair session-vals)
      (let ((k (gethash "var_name" pair))
	    (v (gethash "val" pair)))

	
	(setf (hunchentoot:session-value (kw k)) v)

	;; want these logs only in dev
	(when lisp-fixup:*dev?*
	  (log:info "populating session var from db ~a => ~a" k v))))))

(defun @authenticated (next &key (retries 0))
  (let ((session-cookie (hunchentoot:cookie-in "murja-session"))
	(username-cookie (hunchentoot:cookie-in "murja-username"))
	(user-id (hunchentoot:session-value :logged-in-user-id)))
    (if (and (not user-id)
	     session-cookie
	     (< retries 1))
	;; if session-cookie is found but hunchentoot's session is expired, lets try to restore
	;; it from the db and retry calling this middleware function. If retries > 0 and
	;; restoring-from-db has failed, we're returning 401 to the caller.
	(progn
	  ;; if this assertion fails, currently it probably returns 500. Should we return 401 to
	  ;; callers providing non-matching username and cookie?
	  (murja.session.db:assert-ownership-username username-cookie session-cookie)
	  (populate-http-session username-cookie session-cookie)
	  (@authenticated next :retries (1+ retries)))
	(if user-id
	    (let ((user (get-user-by-id user-id)))
	      (if (and user
		       (string= (hunchentoot:session-value :logged-in-username)
				(gethash "username" user)))
		  (let ((*user* user)
			(*session-key* session-cookie))
		    (funcall next))
		  (progn
		    (setf (hunchentoot:return-code*) 401)
		    "not authorized")))
	    (progn
	      (setf (hunchentoot:return-code*) 401)
	      (log:warn "failed auth at @authenticated")
	      "not authorized")))))

(defun @can? (ability next)
  (if (and *user*
	   (member ability 
		   (gethash "permissions" *user*)
		   :test #'string=))
      (funcall next)
      (progn
	(setf (hunchentoot:return-code*) 401)
	(format nil "you need to be able to ~a" ability))))



