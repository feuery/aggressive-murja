(defpackage murja.routes.login-routes
  (:use :cl)
  (:export :get-session-key :set-session-cookies)
  (:import-from :cl-hash-util :hash)
  (:import-from :murja.session :set-session-value)
  (:import-from :lisp-fixup :sha-512)
  (:import-from :murja.middleware.auth :@test-now :@authenticated :*user*)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:import-from :com.inuoe.jzon :parse :stringify)
  (:local-nicknames (:user-db :murja.users.user-db)
		    (:settings :murja.routes.settings-routes)))

(in-package :murja.routes.login-routes)

(defun get-session-key (username)
  "Creates a new db-backed session for new logins"
  (let ((old-sessions (coerce (murja.session.db:login-query-session* (murja.session.db:now) username) 'list)))
    (if old-sessions
	;; logging in from a new device? return the old session-key and expiration
	(let* ((session-key (gethash "session_key" (first old-sessions)))
	       (age (gethash "max_age" (first old-sessions))))
	  (multiple-value-bind (year month day hour min sec ms)
	      (simple-date:decode-interval age)
	    (values session-key (lisp-fixup:to-secs year month day hour min sec ms))))
	
	;; a fresh session!
	(let* ((session-data (first (coerce (murja.session.db:insert-session* (murja.session.db:now) username) 'list)))
	       (key (gethash "session_key" session-data))
	       (max-age (gethash "max_age" session-data)))
	  (multiple-value-bind (year month day hour min sec ms)
	      (simple-date:decode-interval max-age)
	    (values key (lisp-fixup:to-secs year month day hour min sec ms)))))))

(defun set-session-cookies (username session-key max-age settings)
  (hunchentoot:set-cookie "murja-username" :value username
					   :secure t
					   :path "/"
					   :max-age max-age 
					   :http-only t
					   :domain ;;send :domain only in linux production envs
					   (unless lisp-fixup:*dev?*
					     (gethash "domain" settings))
					   :same-site "Strict")
  
  (hunchentoot:set-cookie "murja-session" :value session-key
					  :secure t
					  :path "/"
					  :max-age max-age 
					  :http-only t
					  :domain (unless lisp-fixup:*dev?*
						    (gethash "domain" settings))
					  :same-site "Strict"))

(defroute post-login ("/api/login/login" :method :post :decorators (@test-now @transaction @json)) ()
  (let* ((body (hunchentoot:raw-post-data :force-text t))
	 (body-params (parse body))
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
		
		(set-session-cookies username session-key max-age settings)
	        
		(stringify user-row))
	      (progn
		(log:error "~a tried to log-in but get-session-key didn't return a session key. This happening signifies a bug" username)
		(setf (hunchentoot:return-code*) 500)
		"catastrophic error"))))

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defroute api-session ("/api/login/session" :method :get :decorators (@test-now
								      @transaction
								      @json
								      @authenticated)) ()
  (if *user*
      (com.inuoe.jzon:stringify (murja.users.user-db:get-session-user-by-id (gethash "id" *user*)))
      (progn
	(setf (hunchentoot:return-code*) 401)
	nil)))

(defun save-initial-data-dump (username nickname password domain blog_title rss_title rss_link rss_description rss_lang rss_email)
  (user-db:register-user username nickname "" password)
  (user-db:cast-only-user-as-admin)

  (settings:update-setting "domain" domain)
  (settings:update-setting "blog-title" blog_title)
  (settings:update-setting "rss-title" rss_title)
  (settings:update-setting "rss-link" rss_link)
  (settings:update-setting "rss-description" rss_description)
  (settings:update-setting "rss-lang" rss_lang)
  (settings:update-setting "rss-email" rss_email))
  

(defroute initial-pageview? ("/api/initial" :method :post :decorators (@transaction
								       @json)) ()
  (murja.json:bind-json (username nickname password domain blog_title rss_title rss_link rss_description rss_lang rss_email) () (hunchentoot:raw-post-data :force-text t)
			(if (user-db:no-users?)
			    (progn 
			      (save-initial-data-dump username nickname password domain blog_title rss_title rss_link rss_description rss_lang rss_email)
			      (setf (hunchentoot:return-code*) 204)
			      "")
			    (progn
			      (log:warn "Someone called POST /api/initial while there are users")
			      (setf (hunchentoot:return-code*) 500)
			      ""))))
