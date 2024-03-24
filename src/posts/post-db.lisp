(defpackage :murja.posts.post-db
  (:use :cl :postmodern)
  (:import-from :com.inuoe.jzon :parse)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :fix-timestamp)
  (:export :get-page :get-titles-by-year :insert-post :update-post :get-post))

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
  (dolist (key (list "creator" "tags"))
    (setf (gethash key post)
	  (parse (gethash key post))))
  
  (setf (gethash "created_at" post)
	(fix-timestamp (gethash "created_at" post)))
  post)

(defun get-page (page page-size &key allow-hidden?)
  (let ((resulting-page (coerce 
	       (get-page* page page-size allow-hidden?) 'list)))
    (mapcar #'fix-post 
	    resulting-page)))

(defun get-post (id &key allow-hidden?)
  (let ((post (aref (get-by-id* id allow-hidden?) 0)))
    (fix-post post)))
    
