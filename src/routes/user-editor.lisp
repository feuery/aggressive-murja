(defpackage murja.routes.user-editor
  (:use :cl)
  (:import-from :murja.json :bind-json)
  (:import-from :lisp-fixup :sha-512)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.middleware.auth :@authenticated :@can? :*user*)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :easy-routes :defroute)

  (:local-nicknames (:user-db :murja.users.user-db)
		    (:login :murja.routes.login-routes)
		    (:settings :murja.routes.settings-routes)))

(in-package :murja.routes.user-editor)

(defun can-save-user? (user-id old-password)
  (and *user*
       (equalp (gethash "id" *user*)
	       user-id)
       (user-db:search-with-id-and-pwd* user-id (sha-512 old-password))))

(defmacro patch (map symbol)
  (let ((symbol-str (str:downcase (format nil "~s" symbol))))
    `(setf (gethash ,symbol-str ,map) ,symbol)))

(defroute submit-user ("/api/user/submit" :method :post
					  :decorators (@transaction
						       @authenticated
						       @json)) ()
  (bind-json (nickname username img_location id old-password) (new-password) (hunchentoot:raw-post-data :force-text t)
    (if (can-save-user? id old-password)
	(let* ((user (user-db:get-user-by-id id)))
	  (patch user nickname)
	  (patch user username)

	  (when (and new-password
		     (not (string= new-password "")))	    
	    (setf (gethash "password" user)
		  (sha-512 new-password)))

	  (user-db:patch-user user)
	  (setf (hunchentoot:return-code*) 204)

	  (multiple-value-bind (session-key max-age) (login:get-session-key username)
	    (login:set-session-cookies username session-key max-age (settings:get-settings))
	    (murja.session:set-session-value :logged-in-username username))
	  
	  "")

	(progn
	  (log:warn "can-save-user? failed due to ~a" (cond
							((not *user*) "*user* failing")
							((not (equalp (gethash "id" *user*)
								      id))
							 (format nil "id ~a != ~a" (gethash "id" *user*)
								 id))
							((not (user-db:search-with-id-and-pwd* id (sha-512 old-password)))
							 "password lookup failing")))
	  (setf (hunchentoot:return-code*) 500)
	  ""))))

	
