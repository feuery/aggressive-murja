(defpackage murja.tests
  (:use :cl
   :fiveam)
  (:export :main-suite))

(in-package :murja.tests)

(def-suite main-suite)

(defvar *test-server* nil)
(defvar *test-port* 3001)

(defun url ()
  (format nil "http://localhost:~d" *test-port*))

(def-fixture prepare-db-and-server ()
    (let ((murja.middleware.db:*automatic-tests-on?* t))
      (murja.middleware.db:with-db 
	(postmodern:execute "DROP SCHEMA IF EXISTS blog CASCADE;")
	(postmodern:execute "DROP TABLE IF EXISTS public.ragtime_migrations")
	(postmodern:execute "DROP TABLE IF EXISTS public.migrations_tracker")

	(unwind-protect 
	     (progn 
	       (setf *test-server* (murja:start-server :port *test-port*))
	       (format t "Starting the test &body~%")
	       (&body))

	  (postmodern:execute "DROP SCHEMA IF EXISTS blog CASCADE;")
	  (postmodern:execute "DROP TABLE IF EXISTS public.ragtime_migrations")
	  (postmodern:execute "DROP TABLE IF EXISTS public.migrations_tracker")
	  (hunchentoot:stop *test-server*)
	  (setf *test-server* nil)))))

(def-test multiple-migrations (:suite main-suite :fixture prepare-db-and-server)
  (let ((successfully-migrated nil))
    (unwind-protect
	 (progn
	   (log:info "Does 001-users.up exist? ~a" (murja.migrations:migration-does-exist "001-users.up"))
	   (log:info "Existing migrations: ~{~a~%~}" (coerce
						      (postmodern:query "SELECT * FROM public.migrations_tracker" :alists) 'list))
	   (log:info "Re-running migrations")
	   (handler-case 
	       (murja.migrations:migrate)
	     (error (c)
	       (log:error "Migrations failed ~a" c)
	       (error 'fail)))
	   (log:info "Re-ran migrations")
	   (setf successfully-migrated t)))
    (is (equalp successfully-migrated t))))
	 
(def-test history (:suite main-suite :fixture prepare-db-and-server)
  ;;(is (equalp 3 55))
  (is (equalp 1 1)))

;; (run! 'main-suite)
