(defpackage murja.routes.rss-routes
  (:use :cl)
  (:import-from :easy-routes :defroute)
  (:import-from :murja.posts.post-db :get-page)
  (:import-from :murja.routes.settings-routes :get-settings)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?))

(in-package :murja.routes.rss-routes)

(defun posts->rss (posts)
  (let* ((settings (get-settings))
	 (title (gethash "rss-title" settings))
	 (link (gethash "rss-link" settings))
	 (description (gethash "rss-description" settings))
	 (lang (gethash "rss-lang" settings))
	 (mail (gethash "rss-mail" settings))
	 (output (make-string-output-stream)))
    (xml-emitter:with-rss2 (output)
      (xml-emitter:rss-channel-header title
				      link
				      :description description
				      :language lang)
      (dolist (post posts)
	(xml-emitter:rss-item (gethash "title" post)
			      :link (format nil "~a/post/~d" link (gethash "id" post))
			      :description (gethash "content" post)
			      :author (gethash "nickname"
					       (gethash "creator" post))
			      :pubdate (gethash "created_at" post))))
    (get-output-stream-string output)))

(defroute rsssss ("/api/rss" :method :get
			     :decorators ( @transaction)) ()
  (let ((lisp-fixup:*rfc822* t))
    (let* ((settings (get-settings))
	   (if-modified-since (when (hunchentoot:header-in* :if-modified-since)
				(lisp-fixup:if-modified-since->simpledate-timestamp
				 (hunchentoot:header-in* :if-modified-since))))
	   (page-size (gethash "recent-post-count" settings))
	   (page (get-page 1 page-size :allow-hidden? nil
				       :modified-since if-modified-since)))

      (setf (hunchentoot:content-type*) "application/rss+xml")
      (posts->rss page))))
