(defpackage murja.tests
  (:use :cl :fiveam)
  (:import-from :murja.users.user-db :register-user)
  (:import-from :murja.tests.literal :literal)
  (:export :main-suite))

(in-package :murja.tests)

(def-suite main-suite)
(in-suite main-suite)

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

(def-test multiple-migrations (:fixture prepare-db-and-server)
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
	       (error "fail")))
	   (log:info "Re-ran migrations")
	   (setf successfully-migrated t)))
    (is (equalp successfully-migrated t))))
	 
(def-test history (:fixture prepare-db-and-server)
  (literal
   We are interested in seeing if the hidden? and unlisted? flags move correctly thorugh Lisp -> blog.Post -> blog.Post_History.

   First lets see if the blog.Post and blog.Post_History tables are empty and we are in a sane initial state.

   !L 
   (let ((posts (coerce (postmodern:query "SELECT * FROM blog.Post") 'list))
	 
	 (history-data (postmodern:query "SELECT * FROM blog.Post_History")))
     (is (not posts))
     (is (not history-data)))

   if that passes we need a test-user that posts content (othis should maybe be moved to a fixture "...") Using this new users ID we can post a new post that is initially both hidden AND unlisted. 

   !L
   (let* ((user-id (register-user "test-user" "Test User" "" "passu"))
	  (post-id (caar (murja.posts.post-db:insert-post "Test title" "new post" user-id "[]" t t))))

     (literal
      Updating the post moves the initial version into the history table (amount of hidden posts 1)
      !L    
      (murja.posts.post-db:update-post "New title" "New Content" "[]" t t post-id)

      Now there is supposed to be 2 hidden posts 
      !L 
      (murja.posts.post-db:update-post "Newest title" "Newes Content" "[]" t t post-id)

      Now there is supposed to be 3 hidden posts and the post in blog.Post is visible
      
      !L
      (murja.posts.post-db:update-post "Newester title" "Newes Content" "[\"test-tag\"]" nil nil post-id)

      !L
      (let ((history-data (coerce (postmodern:query "SELECT * FROM blog.Post_History where hidden" :array-hash) 'list)))
	(is (equalp 3
		    (length history-data))))))))

;; (setf fiveam:*run-test-when-defined* t)

(if (and (sb-ext:posix-getenv "GHA")
	 (not (run! 'main-suite)))
    (sb-ext:exit :code 666))
