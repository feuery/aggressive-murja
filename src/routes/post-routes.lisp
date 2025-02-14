(defpackage murja.routes.post-routes
  (:use :cl)
  (:import-from :murja.routes.settings-routes :get-settings)
  (:import-from :lisp-fixup :partial)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?)
  (:import-from :murja.posts.post-db :get-post-version :get-post :get-page :get-titles-by-year)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.post-routes)

(defroute title-routes ("/api/posts/titles" :method :get
					    :decorators (@json @transaction)) ()
  (let ((titles (or (get-titles-by-year) #())))
    (stringify titles)))

(defroute manager-title-routes ("/api/posts/all-titles" :method :get
							:decorators (@json @transaction @authenticated (@can? "edit-post"))) ()
  
  (let ((titles (get-titles-by-year :allow-hidden? t)))
    (stringify (or titles #()))))    

(defroute get-page-route ("/api/posts/page/:page" :method :get
						  :decorators (@json @transaction))
    (&path (page 'integer))
  (let* ((settings (get-settings))
	 (page-size (gethash "recent-post-count" settings)))
    (let* ((id page)
	   (posts (or (murja.posts.post-db:get-page page page-size) #()))
	   (last-page? (zerop (length (murja.posts.post-db:get-page (1+ page) page-size))))
	   (result (make-hash-table)))
      (setf (gethash "id" result) id)
      (setf (gethash "posts" result) posts)
      (setf (gethash "last-page?" result) last-page?)

      (com.inuoe.jzon:stringify result))))

(defroute hidden-post ("/api/posts/post/:id/allow-hidden/:hidden" :method :get
								  :decorators (@json
									       @transaction
									       @authenticated
									       (@can? "edit-post"))) ()
  
  (let* ((show-hidden? (string= hidden "true"))
	 (post (get-post id :allow-hidden? show-hidden?)))
    (when post
      (log:info "returning post (hidden allowed? ~a) { ~{~a~%~} }~%" hidden (alexandria:hash-table-alist post))
      (stringify post))))

(defroute get-post-version-route ("/api/posts/post/:id/version/:version" :method :get
								   :decorators (@json
										@transaction)) ()
  (let ((post (get-post-version id version)))
    (if post 
	(stringify post)
	(progn
	  (setf (hunchentoot:return-code*) 404)
	  ""))))

(defroute unhidden-post ("/api/posts/post/:id" :method :get :decorators (@json
									 @transaction)) ()
  (let* ((post (get-post id)))
    (if post
	(progn 
	  (log:info "returning unhidden post { ~{~a~%~} }~%" (alexandria:hash-table-alist post))
	  (stringify post))
	(progn
	  (setf (hunchentoot:return-code*) 404)
	  ""))))

(defroute tagged-posts-route ("/api/posts/tagged/:tag" :method :get
						       :decorators (@json
								    @transaction)) ()
  (when (and (not (string= tag "hidden"))
	     (not (string= tag "unlisted")))
    (stringify (murja.posts.post-db:get-tagged tag :allow-hidden? nil))))

(defroute create-empty-post-route ("/api/posts/new_post" :method :post
							 :decorators (@json
								      @transaction
								      @authenticated
								      (@can? "create-post"))) ()
  (let ((creator-id (gethash "id" *user*)))
    (prin1-to-string (caar (murja.posts.post-db:insert-post "New title" "New post" creator-id "[]" t nil)))))

(defvar *excerpt-html-template*
" <blockquote class=\"excerpt\">
  <header>
    <a href=~s> ~s says...</a>
  </header>
  ~a 
</blockquote> ")

(defroute create-excerpt ("/api/posts/excerpt/:feed-item-id" :method :post
					       :decorators (@json
							    @transaction
							    @authenticated
							    (@can? "create-post"))) ()
  (let* ((creator-id (gethash "id" *user*))
	 (name-and-url (first
			(coerce
			 (murja.rss.reader-db:get-feed-name-and-url feed-item-id creator-id)
			 'list)))
	 (name (gethash "name" name-and-url))
	 (url (gethash "url" name-and-url))
	 (excerpt (hunchentoot:raw-post-data :force-text t)))
    (prin1-to-string (caar (murja.posts.post-db:insert-post "New title"
							    (format nil *excerpt-html-template*
								    url name excerpt)
							    creator-id "[]" t nil)))))
  

(defroute search-prev ("/api/posts/search-previously" :method :post
						      :decorators (@json
								   @transaction
								   @authenticated
								   (@can? "create-post"))) ()
  (let* ((search-body (hunchentoot:raw-post-data :force-text t)))
    (stringify (murja.posts.post-db:search-posts search-body))))

(defroute post-update-route ("/api/posts/post" :method :put 
					       :decorators (@json
							    @transaction
							    @authenticated
							    (@can? "edit-post"))) ()
  (let* ((request-body (parse (hunchentoot:raw-post-data :force-text t)))
	 (content (gethash "content" request-body))
	 (title (gethash "title" request-body))
	 (tags (stringify
		(or (remove-if (partial #'string= "")
			       (coerce 
				(gethash "tags" request-body) 'list))
		    #())))
	 (post-id (gethash "id" request-body))
	 (previously-links (coerce (gethash "previously" request-body) 'list))
	 (hidden (gethash "hidden" request-body))
	 (unlisted (gethash "unlisted" request-body)))
    (log:info "updating post ~d" post-id)

    (murja.posts.post-db:update-post title content tags hidden unlisted post-id)
    (dolist (link previously-links)
      (let ((id (gethash "id" link)))
	(murja.posts.post-db:link-previously post-id id)))
    ""))
