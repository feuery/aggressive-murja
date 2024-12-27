(defpackage murja.tests.session
  (:use :cl :fiveam)
  (:import-from :binding-arrows :->>)
  (:import-from :murja.users.user-db :register-user)
  (:import-from :halisql :*system-name*)
  (:import-from :murja.tests :url :prepare-db-and-server :drakma->string :url :main-suite :prepare-db-and-server))

(in-package :murja.tests.session)
(in-suite main-suite)

(defvar username "testuser")
(defvar passwd "passw0rd")

(defvar cookie-dada "murja-session=cb5b4da0-7d79-4b05-ad0d-dd0856cb758e; Max-Age=7776000; SameSite=Strict; Secure; HttpOnly,murja-username=testuser; Max-Age=7776000; SameSite=Strict; Secure; HttpOnly,hunchentoot-session=4:05D82A7F4C9389BC7C267239AC0DFAB3; Path=/; HttpOnly")

(defun cookies->hash (cookies)
  (alexandria:plist-hash-table (->>
				 cookies 
				 (str:split #\,)
				 (mapcar (lisp-fixup:partial #'str:split #\;))
				 (mapcar #'first)
				 (mapcar (lisp-fixup:partial #'str:split #\=))
				 (apply #'concatenate 'list))
			       :test 'equal))
	   

(def-test session-test (:fixture prepare-db-and-server)
  (register-user username "Testuser" "" passwd)

  ;; pre-state is not insane 
  (is (not (equalp nil
		   (postmodern:query "SELECT * FROM blog.users"))))

  (postmodern:execute "INSERT INTO blog.groupmapping
SELECT usr.id, grp.id, true
FROM blog.users usr
JOIN blog.usergroup grp ON grp.name = 'Admins'
ON CONFLICT DO NOTHING")

  (is (not (equalp nil
		   (postmodern:query "SELECT * FROM blog.groupmapping"))))
  (is (equalp nil
	      (postmodern:query "SELECT * FROM blog.session_store")))
  (is (equalp nil
	      (postmodern:query "SELECT * FROM blog.serialized_session")))

  ;; does session-route return 401 as expected with an uninitialized session?
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url)))
    (is (equalp 401 status)))

  ;; how does it handle rubbish session-key cookies?
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							  :additional-headers `(("Cookie" . "murja-session=ihme_roskaa; murja-username=testuser")
										("x-murja-now" . "Fri, 27 Dec 2024 09:48:46 EST")))
    (is (equalp 401 status)))

  ;; how about innocent looking keys that parse as cookies? 
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							  :additional-headers `(("Cookie" . ,(format nil "murja-session=~a; murja-username=testuser" (uuid:make-v4-uuid)))))
    (is (equalp 401 status)))

  ;; how does it handle rubbish usernames?
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							  :additional-headers `(("Cookie" . "murja-session=ihme_roskaa; murja-username=user_that_never_existed")))
    (is (equalp 401 status)))

  ;; let's log in
  (multiple-value-bind (body status headers)
      (drakma:http-request (format nil "~a/api/login/login" (url))
			   :method :post
			   :content (format nil "{\"username\": \"~a\", \"password\": \"~a\"}" username passwd))
    (is (equalp 200 status))
    (let* ((cookies-str (cdr (assoc :set-cookie headers)))
	   (cookies (cookies->hash cookies-str))
	   (hunchentoot-session (gethash "hunchentoot-session" cookies))
	   (murja-session (gethash "murja-session" cookies))
	   (murja-username (gethash "murja-username" cookies)))

      ;; you could probably integration-test a lot by seeing what that body contains

      ;; valid hunchentoot-session overrides rubbish cookies
      (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							  :additional-headers `(("Cookie" . ,(format nil "hunchentoot-session=~a;murja-session=~a; murja-username=~a" hunchentoot-session murja-session "NON_EXISTANT_DUDE"))))
	(is (equalp 200 status)))

      ;; valid cookies repopulate the session 
      (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							      :additional-headers `(("Cookie" . ,(format nil "murja-session=~a; murja-username=~a" murja-session murja-username))))
	(is (equalp 200 status)))


      ;; let's timejump 4 months and see if the session expires correctly

      (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login/session" (url))
							      :additional-headers `(("Cookie" . ,(format nil "murja-session=~a; murja-username=~a" murja-session murja-username))
										    ("x-murja-now" . ,(let ((lisp-fixup:*rfc822* t))
													(lisp-fixup:fix-timestamp (caar (postmodern:query "SELECT now() + '4 months'")))))))
	(is (equalp 401 status))))))

;; (setf fiveam:*run-test-when-defined* t)


(if (and (sb-ext:posix-getenv "GHA")
	 (not (run! 'main-suite)))
    (sb-ext:exit :code 666))
