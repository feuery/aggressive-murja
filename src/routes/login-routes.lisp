(defpackage murja.routes.login-routes
  (:use :cl)
  (:import-from :lisp-fixup :sha-512)
  (:import-from :murja.middleware.auth :@authenticated :*user*)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:import-from :com.inuoe.jzon :parse :stringify))

(in-package :murja.routes.login-routes)

(defroute post-login ("/api/login/login" :method :post :decorators (@transaction @json)) ()
  (let* ((body-params (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-params))
	 (password (gethash "password" body-params))
	 (user-row (murja.users.user-db:select-user-by-login username (sha-512 password))))
    (if (and user-row
	     (string= (gethash "username" user-row) username))
	(progn
	  (setf (hunchentoot:session-value :logged-in-username) username)
	  (setf (hunchentoot:session-value :logged-in-user-id) (gethash "userid" user-row))
	  (stringify user-row))

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defroute api-session ("/api/login/session" :method :get :decorators (@transaction
								      @json
								      @authenticated)) ()
  (if *user*
      (com.inuoe.jzon:stringify (murja.users.user-db:get-session-user-by-id (gethash "id" *user*)))
      (progn
	(setf (hunchentoot:return-code*) 401)
	nil)))
