(defpackage murja.routes.login-routes
  (:use :cl)
  (:import-from :murja.session :set-session-value)
  (:import-from :lisp-fixup :sha-512)
  (:import-from :murja.middleware.auth :@authenticated :*user*)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:import-from :com.inuoe.jzon :parse :stringify))

(in-package :murja.routes.login-routes)

(defun get-session-key (username)
  "Creates a new db-backed session for new logins"
  (let ((old-session (murja.session.db:login-query-session* username)))
    (when old-session
      (log:error "~a tried to log in with an existing session" username))
    
    (unless old-session
      (let* ((session-data (first (coerce (murja.session.db:insert-session* username) 'list)))
	     (key (gethash "session_key" session-data))
	     (max-age (gethash "max_age" session-data)))
	(multiple-value-bind (year month day hour min sec ms)
	    (simple-date:decode-interval max-age)
	  (values key (lisp-fixup:to-secs year month day hour min sec ms)))))))

(defroute post-login ("/api/login/login" :method :post :decorators (@transaction @json)) ()
  (let* ((body-params (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-params))
	 (password (gethash "password" body-params))
	 (user-row (murja.users.user-db:select-user-by-login username (sha-512 password))))
    (if (and user-row
	     (string= (gethash "username" user-row) username))
	(let ((settings (murja.routes.settings-routes:get-settings))
	      (murja.middleware.auth:*user* (murja.users.user-db:get-user-by-id (gethash "userid" user-row))))
	  (multiple-value-bind (session-key max-age) (get-session-key username)
	    (if session-key
	      (let ((murja.middleware.auth:*session-key* session-key))
		
		(set-session-value :logged-in-username username)
		(set-session-value :logged-in-user-id (gethash "userid" user-row))
		
		(hunchentoot:set-cookie "murja-username" :value username
							 :secure t
							 :max-age max-age 
							 :http-only t
							 :domain ;;send :domain only in linux production envs
							 (unless lisp-fixup:*dev?*
							   (gethash "domain" settings))
							 :same-site "Strict")
		
		(hunchentoot:set-cookie "murja-session" :value session-key
							:secure t
							:max-age max-age 
							:http-only t
							:domain (unless lisp-fixup:*dev?*
								  (gethash "domain" settings))
							:same-site "Strict")
	        
		(stringify user-row))
	      (progn
		(log:error "~a tried to log-in but get-session-key didn't return a session key. This happening signifies a bug" username)
		(setf (hunchentoot:return-code*) 500)
		"catastrophic error"))))

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
