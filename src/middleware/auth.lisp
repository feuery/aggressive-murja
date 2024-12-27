(defpackage murja.middleware.auth
  (:use :cl :postmodern)
  (:import-from :murja.users.user-db :get-user-by-id)
  (:export :*now* :*session-key* :*user* :@can?))

(in-package :murja.middleware.auth)

(defvar *user* nil
  "A special variable for storing the logged in user (as defined in the db)")

(defvar *session-key* nil
  "A special var that stores a non-expired database session-key used for re-populating http-session upon its death")

(defun kw (str)
  (read-from-string (format nil ":~a" str)))

(defun populate-http-session (username session-key)
  (let ((session-vals (coerce (murja.session.db:all-session-vals (murja.session.db:now) username session-key) 'list)))
    (log:info "populating session for user ~a" username)
    (dolist (pair session-vals)
      (let ((k (gethash "var_name" pair))
	    (v (gethash "val" pair)))

	
	(setf (hunchentoot:session-value (kw k)) v)

	;; want these logs only in dev
	(if lisp-fixup:*dev?*
	    (log:info "populating session var from db ~a => ~a" k v)
	    (log:info "populating session var from db ~a" k))))))

(defun @authenticated (next &key (retries 0))
  (let ((session-cookie (hunchentoot:cookie-in "murja-session"))
	(username-cookie (hunchentoot:cookie-in "murja-username"))
	(user-id (hunchentoot:session-value :logged-in-user-id)))
    (when lisp-fixup:*dev?*
      (log:info "Read session-cookie ~a for user ~a" session-cookie username-cookie))
    (if (and (not user-id)
	     session-cookie
	     (< retries 1))
	;; if session-cookie is found but hunchentoot's session is expired, lets try to restore
	;; it from the db and retry calling this middleware function. If retries > 0 and
	;; restoring-from-db has failed, we're returning 401 to the caller.
	(if (murja.session.db:assert-ownership-username username-cookie session-cookie)
	    (progn
	      (log:info "populating http-session and retrying")
	      (populate-http-session username-cookie session-cookie)
	      (@authenticated next :retries (1+ retries)))
	    (progn 
	      (setf (hunchentoot:return-code*) 401)
	      (log:warn "assert-ownership-username failed for ~a" username-cookie)
	      "not authorized"))
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
	      (log:warn "failed auth at @authenticated, ~a" (list :retries retries
								  :session-cookie session-cookie
								  :username-cookie username-cookie
								  :user-id user-id))
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

(defun @test-now (next)
  (if (and murja.middleware.db:*automatic-tests-on?*
	   (hunchentoot:header-in* :x-murja-now))
      (let ((lisp-fixup:*now* (lisp-fixup:if-modified-since->simpledate-timestamp
			       (hunchentoot:header-in* :x-murja-now))))
	(log:info "parsed the :now in a test as ~a" lisp-fixup:*now*)
	(funcall next))
      (progn
	(when murja.middleware.db:*automatic-tests-on?*
	  (log:info "didn't find header x-murja-now"))
	(funcall next))))
      
