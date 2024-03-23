(defpackage murja.routes.post-routes
  (:use :cl)
  (:import-from :lisp-fixup :partial)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?)
  (:import-from :murja.posts.post-db :get-page :get-titles-by-year)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.post-routes)

(defroute title-routes ("/api/posts/titles" :method :get
					    :decorators (@json @transaction)) ()
  (let ((titles (get-titles-by-year)))
    (stringify titles)))

(defroute manager-title-routes ("/api/posts/all-titles" :method :get
							:decorators (@json @transaction)) ()
  (let ((titles (get-titles-by-year :allow-hidden? t)))
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


;; routes that write to the db
(defroute post-creation-route ("/api/posts/post" :method :post
						 :decorators (@json
							      @transaction
							      @authenticated
							      (@can? "create-post"))) ()
    (let* ((request-body (parse (hunchentoot:raw-post-data :force-text t)))
	   (content (gethash "content" request-body))
	   (title (gethash "title" request-body))
	   (tags (stringify
		  (remove-if (partial #'string= "")
			     (coerce 
			      (gethash "tags" request-body) 'list))))

	   (creator-id (gethash "id" *user*)))
      (assert creator-id)
      (murja.posts.post-db:insert-post title content creator-id tags)
      ""))
    
