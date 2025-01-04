(defpackage :murja.users.user-db
  (:use :cl :postmodern)
  (:import-from :lisp-fixup :sha-512)
  (:export :patch-user-img* :get-session-user-by-id :search-with-id-and-pwd* :get-user-by-id :select-user-by-login :register-user :patch-user)
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
    (if usr
	(jsonize-key usr "permissions")
	(let ((usrs (coerce (postmodern:query "SELECT * FROM blog.Users" :alists) 'list)))
	  (unless usrs
	    (log:error "There are no users in the db"))
	  nil))))

(defun register-user (username nickname img-location password)
  "Inserts the new user into db and returns its id" 
  (caar (postmodern:query "INSERT INTO blog.Users (username, nickname, img_location, password) VALUES ($1, $2, $3, $4) returning id"
			  username
			  nickname
			  img-location
			  (sha-512 password))))

  ;;(postmodern:connect-toplevel "blogdb" "blogadmin" "blog" "localhost")

(defun patch-user (usr)
  (cl-hash-util:with-keys ("nickname" "username" "password" "id") usr
    (patch-user* nickname username password id)))
