(defpackage murja.activity-pub.routes
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :cl-hash-util :hash)
  (:import-from :binding-arrows :->> :->)
  (:import-from :lisp-fixup :partial)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:local-nicknames (:user-db :murja.users.user-db)
		    (:settings :murja.routes.settings-routes)))

(in-package :murja.activity-pub.routes)

;; {
;; 	"@context": [
;; 		"https://www.w3.org/ns/activitystreams",
;; 		"https://w3id.org/security/v1"
;; 	],

;; 	"id": "https://my-example.com/actor",


;; 	"type": "Person",
;; 	"preferredUsername": "alice",
;; 	"inbox": "https://my-example.com/inbox",
;; 	"publicKey": {
;; 		"id": "https://my-example.com/actor#main-key",
;; 		"owner": "https://my-example.com/actor",
;; 		"publicKeyPem": 
;; 	}
;; }

(defun get-domain ()
  (let* ((settings (settings:get-settings))
	 (domain (gethash "domain" settings)))
    (if (string= domain "")
	"localhost:3010"
	domain)))

;; needs rewriting in nginx
(defroute actor-route ("/@/:person" :method :get :decorators (@json @transaction)) ()
  (let* ((domain (get-domain))
	 (user (first (coerce (user-db:get-user-by-username* person) 'list)))
	 (id (format nil "https://~a/@~a" domain person)))
    (hash ("@context" (list "https://www.w3.org/ns/activitystreams" "https://w3id.org/security/v1"))
	  ("id" id)
	  ("type" "Person")
	  ("preferredUsername" (gethash "nickname" user))
	  ("inbox" (format nil "https://~a/@~a/inbox" domain person))
	  ("publicKey" (hash ("id" (format nil "~a#main-key" id))
			     ("owner" id)
			     ("publicKeyPem" "-----BEGIN PUBLIC KEY-----...-----END PUBLIC KEY-----"))))))

(defmacro assert-let (p &rest body)
  (destructuring-bind (binding form . _) p
    `(let ((,binding ,form))
      (if ,binding
	  (progn
	    ,@body)
	  (log:error "~a failed" ,binding)))))
    

(defun inner-webfinger (subject account)
;;   {
;; 	"subject": "acct:alice@my-example.com",

;; 	"links": [
;; 		{
;; 			"rel": "self",
;; 			"type": "application/activity+json",
;; 			"href": "https://my-example.com/actor"
;; 		}
;; 	]
  ;; }
  (assert-let (user (user-db:get-user-by-username* account))
	      (hash ("subject" subject)
		    ("preferredUsername" (gethash "nickname" (aref user 0)))
		    ("links " (list (hash ("rel" "self")
					  ("type" "application/activity+json")
					  ("href" (format nil "https://~a/@~a" (get-domain) account))))))))

	
 

(defroute webfinger ("/.well-known/webfinger" :method :get :decorators (@transaction)) (&get resource)
  ;; acct:bob@my-example.com
  (setf (hunchentoot:content-type*) "application/activity+json")
  (let* ((acct (->>
		   resource
		   (str:split ":")
		   second
		   (str:split "@")))
	 (account (first acct))
	 (searched-domain (second acct))
	 (domain (get-domain)))
    (if (string= searched-domain
		 domain)
	(stringify
	 (inner-webfinger resource account))
	(progn 
	  (log:error "(string/= ~s ~s)" searched-domain domain)
	  (setf (hunchentoot:return-code*) 404)
	  ""))))
