(defpackage :murja.posts.post-db
  (:use :cl :postmodern)
  (:import-from :com.inuoe.jzon :parse)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :fix-timestamp)
  (:export :get-page :get-titles-by-year :insert-post :update-post))

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

(defun get-page (page page-size &key allow-hidden?)
  (mapcar (lambda (post)
	    (dolist (key (list "creator" "tags"))
	      (setf (gethash key post)
		    (parse (gethash key post))))
	    
	    (setf (gethash "created_at" post)
		  (fix-timestamp (gethash "created_at" post)))
	    post)
	  (coerce 
	   (get-page* page page-size allow-hidden?) 'list)))
