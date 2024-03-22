(defpackage murja.routes.login-routes
  (:use :cl)
  (:import-from :murja.middleware.auth :@authenticated)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:import-from :com.inuoe.jzon :parse))

(in-package :murja.routes.login-routes)

(defroute post-login ("/api/login" :method :post :decorators (@transaction @json)) ()
  (let* ((body-params (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-params))
	 (password (gethash "password" body-params))
	 (user-row (select-user-by-login username (sha-512 password))))
    (if (and user-row
	     (string= (gethash "username" user-row) username))
	(progn
	  (setf (hunchentoot:session-value :logged-in-username) username)
	  (setf (hunchentoot:session-value :logged-in-user-id) (gethash "id" user-row))
	  (stringify data-for-frontend))

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defroute api-session ("/api/login/session" :method :get :decorators (@transaction
								      @json
								      @authenticated)) ()
  (if *user*
      (com.inuoe.jzon:stringify *user*)
      (progn
	(setf (hunchentoot:return-code*) 401)
	nil)))
