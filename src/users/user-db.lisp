(defpackage :murja.users.user-db
  (:use :cl :postmodern)
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
  (jsonize-key (aref (get-user-by-id* id) 0) "permissions"))

(defun select-user-by-login (username password-sha)
  (jsonize-key (aref (query-users* username password-sha) 0) "permissions"))

  ;;(postmodern:connect-toplevel "blogdb" "blogadmin" "blog" "localhost")
