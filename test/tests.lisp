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
	(is (equalp 3 (length history-data))))


      After making sure hidden-flags pass to the history table correctly "," we should see if all the apis that have "allow-hidden?" parameter respect that.

      First"," lets see if the initial state is sane

      !L
      (let ((existing-hidden-posts (postmodern:query "SELECT * FROM blog.Post where hidden or unlisted"))
	    (existing-hidden-history-posts (postmodern:query "SELECT * FROM blog.Post_History where hidden or unlisted")))
	(is (equalp 0 (length existing-hidden-posts)))
	(is (equalp 3 (length existing-hidden-history-posts))))

      Then we must insert a few hidden and unlisted posts. The binding we are introducing here will be used later

      !L
      (let ((hidden-post-id (caar (murja.posts.post-db:insert-post "Hidden title" "new post" user-id "[]" t nil)))
	    (unlisted-post-id (caar (murja.posts.post-db:insert-post "Unlisted title" "new post" user-id "[]" nil t))))
	(murja.posts.post-db:insert-post "Second title" "new post" user-id "[\"test-hidden\"]" t t)

	(let ((existing-hidden-posts (postmodern:query "SELECT * FROM blog.Post where hidden"))
	      (existing-unlisted-posts (postmodern:query "SELECT * FROM blog.Post where unlisted")))
	  (is (equalp 2 (length existing-hidden-posts)))
	  (is (equalp 2 (length existing-unlisted-posts))))

	(literal 
      A simple grep of the repository tells us that the interesting functions to test are":"
      "feuer@vivacia aggressive-murja % grep -iR hidden src |grep -i defun" => 
      - "src/posts/post-db.lisp:(defun get-titles-by-year (&key allow-hidden?)"
      - "src/posts/post-db.lisp:(defun get-page (page page-size &key allow-hidden?)"
      - "src/posts/post-db.lisp:(defun get-post (id &key allow-hidden?)"
      - "src/posts/post-db.lisp:(defun get-tagged (tag &key allow-hidden?)"

      !L
      (let ((titles (murja.posts.post-db:get-titles-by-year :allow-hidden? nil))
	    (first-page (murja.posts.post-db:get-page 1 555 :allow-hidden? nil))
	    (hidden-post (murja.posts.post-db:get-post hidden-post-id :allow-hidden? nil))
	    (unlisted-post (murja.posts.post-db:get-post unlisted-post-id :allow-hidden? nil))
	    (test-hidden-tagged-posts (murja.posts.post-db:get-tagged "test-hidden" :allow-hidden? nil)))

	(is (null hidden-post))
	(is (null test-hidden-tagged-posts))
	
	;; unlisted posts should be always visible
	(is (atom unlisted-post))
	;;  just missing from the sidebar titles 
	(is (not (member unlisted-post-id
			 (mapcar (lambda (title) (gethash "Id" title)) titles)
			 :test 'equal)))
	

	;; test sidebar titles 
	(is (not (member "Hidden title" (mapcar (lambda (title) (gethash "Title" title)) titles) :test 'equal)))
	(is (not (member "Unlisted title" (mapcar (lambda (title) (gethash "Title" title)) titles) :test 'equal)))

	;; test the page
	(is (not (member "Hidden title" (mapcar (lambda (title) (gethash "Title" title)) first-page) :test 'equal)))
	(is (not (member "Unlisted title" (mapcar (lambda (title) (gethash "Title" title)) first-page) :test 'equal))))))))))

;; (setf fiveam:*run-test-when-defined* t)

(if (and (sb-ext:posix-getenv "GHA")
	 (not (run! 'main-suite)))
    (sb-ext:exit :code 666))
