(defpackage :murja.users.user-db
  (:use :cl :postmodern)
  (:export :get-session-user-by-id :select-user-by-login)
  (:import-from :halisql :defqueries))

(in-package :murja.users.user-db)

(defqueries "user-fns")

(defun jsonize-key (hash key)
  (setf (gethash key hash)
	(coerce
	 (com.inuoe.jzon:parse (gethash key hash))
	 'list))
  hash)

(defun get-user-by-id (id)
  (let ((result (coerce (get-user-by-id* id) 'list)))
    (when result
      (jsonize-key (first result) "permissions"))))

(defun get-session-user-by-id (id)
  (jsonize-key (aref (query-user-for-session id) 0) "permissions"))

(defun select-user-by-login (username password-sha)
  (let ((usr (first (coerce  (query-users* username password-sha) 'list))))
    (when usr
      (jsonize-key usr "permissions"))))

  ;;(postmodern:connect-toplevel "blogdb" "blogadmin" "blog" "localhost")
