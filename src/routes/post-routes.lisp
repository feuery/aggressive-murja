(defpackage murja.routes.post-routes
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.posts.post-db :get-page :get-titles-by-year)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.post-routes)

(defroute title-routes ("/api/posts/titles" :method :get
					    :decorators (@json @transaction)) ()
  (let ((titles (get-titles-by-year)))
    (stringify titles)))
    

(defroute get-page-route ("/api/posts/page/:page/page-size/:page-size" :method :get
								       :decorators (@json @transaction))
    (&path (page 'integer)
	   &path (page-size 'integer))
  (let* ((page (1- page))
	 (posts (murja.posts.post-db:get-page page page-size))
	 (id page)
	 (last-page? (zerop (length (murja.posts.post-db:get-page (1+ page) page-size))))
	 (result (make-hash-table)))
    (setf (gethash "id" result) id)
    (setf (gethash "posts" result) posts)
    (setf (gethash "last-page?" result) last-page?)

    (com.inuoe.jzon:stringify result)))
