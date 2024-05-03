(defpackage murja.middleware.db
  (:use :cl :postmodern)
  (:export :connect-murjadb-toplevel
	   :with-db
	   :*automatic-tests-on?*))

(in-package :murja.middleware.db)

(defvar *automatic-tests-on?* nil)

(defun db-config () 
  (list :db (or (sb-ext:posix-getenv "MURJA_DB")
		"blogdb")
	:username (or (sb-ext:posix-getenv "MURJA_DB_USER")
		      "blogadmin")
	:password (or (sb-ext:posix-getenv "MURJA_DB_PASSWD")
		      "blog")
	:host (or (sb-ext:posix-getenv "MURJA_DB_HOST")
		  "localhost")
	:port (let ((port-str (if *automatic-tests-on?*
				  "2345"
				  (sb-ext:posix-getenv "MURJA_DB_PORT"))))
		 (if port-str
		     (parse-integer port-str)
		     5432))))

(defun connect-murjadb-toplevel ()
  (destructuring-bind (&key db username password host port) (db-config)
    (postmodern:connect-toplevel db username password host :port port)))

;; (connect-murjadb-toplevel)

(defmacro with-db (&rest body)
  `(destructuring-bind (&key db username password host port) (db-config)
     (format t "Connecting to db ~a ~%" (list db username "$password" host :port port))
     (with-connection (list db username password host :port port)
       ,@body)))

(defun @transaction (next)
  (with-db
      (handler-bind ((cl-postgres:database-socket-error
		     (lambda (c)
		       (format t "Socket error from db: ~a~%" c)
		       (setf (hunchentoot:return-code*) 500)
		       (return-from @transaction "Internal Server Error")))
		     (cl-postgres:database-error
		       (lambda (c)
			 (format t "Error from db: ~a~%" c)
			 (setf (hunchentoot:return-code*) 500)
			 (return-from @transaction "Internal Server Error"))))
	(with-transaction ()
	  (funcall next)))))
