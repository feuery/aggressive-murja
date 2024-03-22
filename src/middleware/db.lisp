(defpackage murja.middleware.db
  (:use :cl :postmodern))

(in-package :murja.middleware.db)

(defun db-config () 
  (list :db (or (sb-ext:posix-getenv "MURJA_DB")
		"blogdb")
	:username (or (sb-ext:posix-getenv "MURJA_DB_USER")
		      "blogadmin")
	:password (or (sb-ext:posix-getenv "MURJA_DB_PASSWD")
		      "blog")
	:host (or (sb-ext:posix-getenv "MURJA_DB_HOST")
		  "localhost")
	:port (let ((port-str (sb-ext:posix-getenv "MURJA_DB_PORT")))
		 (if port-str
		     (parse-integer port-str)
		     5432))))

(defun @transaction (next)
  (destructuring-bind (&key db username password host port) (db-config)
    (with-connection (list db username password host :port port)

      (with-schema (:blog :if-not-exist nil)
	(handler-bind ((cl-postgres:database-error
			 (lambda (c)
			   (format t "Error from db: ~a~%" c)
			   (setf (hunchentoot:return-code*) 500)
			   "Internal Server Error")))
	  (with-transaction ()
	    (funcall next)))))))
