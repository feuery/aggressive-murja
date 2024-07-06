(defpackage :murja.posts.post-db
  (:use :cl :postmodern)
  (:import-from :com.inuoe.jzon :parse)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :fix-timestamp)
  (:export :search-posts :link-previously :get-tagged :get-post-version :get-page :get-titles-by-year :insert-post :update-post :get-post))

(in-package :murja.posts.post-db)

(defqueries "post-fns")

(defun get-titles-by-year (&key allow-hidden?)
  (mapcar (lambda (title)

	    (when (gethash "Tags" title)
	      (setf (gethash "Tags" title)
		    (parse (gethash "Tags" title))))
	    title)
	  (coerce
	   (get-titles-by-year* allow-hidden?) 'list)))

(defun fix-post (post)
  (dolist (key (list "creator" "tags" "versions" "previously"))
    (when (gethash key post)
      (setf (gethash key post)
	    (parse (gethash key post)))))

  (setf (gethash "previously" post)
	(or
	 (remove-duplicates
	  (remove-if-not #'hash-table-p (coerce (gethash "previously" post) 'list))
	  :test 'equalp)
	 #()))

  (setf (gethash "created_at" post)
	(fix-timestamp (gethash "created_at" post)))
  post)

(defun get-page (page page-size &key allow-hidden? modified-since)
  (let* ((page (if (< page 1)
		   1
		   page))
	 (resulting-page (coerce
			  (get-page* (* (1- page) page-size)
				     page-size allow-hidden? (if modified-since
								 modified-since
								 :null)) 'list)))
    (mapcar #'fix-post 
	    resulting-page)))

(defun get-post (id &key allow-hidden?)
  (let* ((posts (coerce (get-by-id* id allow-hidden?) 'list))
	 (post (first posts)))
    (when post
      (fix-post post))))
    
(defun get-post-version (id version)
  (let ((post (first (coerce (get-versioned-by-id* id version) 'list))))
    (when post 
      (fix-post post ))))

(defun get-tagged (tag &key allow-hidden?)
  (let ((posts (coerce (get-tagged* tag allow-hidden?) 'list)))
    (log:info "Tag ~a returns posts ~a~%" tag (mapcar #'alexandria:hash-table-alist posts))
    (when posts
      (mapcar #'fix-post
	      posts))))
