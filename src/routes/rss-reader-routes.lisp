(defpackage murja.routes.rss-reader-routes
  (:use :cl)
  (:import-from :easy-routes :defroute)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?)
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.rss.reader-db :update-feeds :get-user-feeds :subscribe-to-feed))

(in-package :murja.routes.rss-reader-routes)

(defroute user-feeds-route ("/api/user/feeds"
			    :method :get
			    :decorators (@json
					 @transaction
					 @authenticated)) ()
  (assert (not (null *user*)))
  (assert (not (null (gethash "id" *user*))))
	  
  (let ((feeds (or (get-user-feeds (gethash "id" *user*)) #())))
    (com.inuoe.jzon:stringify feeds)))

(defroute updater-metadata ("/api/user/feeds/meta" :method :get
						   :decorators (@json
								@transaction
								@authenticated)) ()
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "last-update-timestamps" result)
	  (or murja.rss.reader-db:*updates*
	      #()))
    (com.inuoe.jzon:stringify result)))

(defroute user-feeds-saving ("/api/user/feeds"
			     :method :post
			     :decorators (@transaction @authenticated)) ()
  (let* ((request-body (parse (hunchentoot:raw-post-data :force-text t)))
	(name (gethash "name" request-body))
	(url (gethash "url" request-body)))
    (assert (not (null *user*)))
    (subscribe-to-feed name url *user*)
    (setf (hunchentoot:return-code*) 204)
    ""))

(defroute mark-as-read ("/api/user/feeds/:feed-id/:item-id/mark-read" :method :post
								      :decorators (@transaction
										   @authenticated)) ()
  (murja.rss.reader-db:mark-as-read item-id feed-id (gethash "id" *user*))
  (setf (hunchentoot:return-code*) 204)
  "")

(defroute delete-feed ("/api/user/feeds/:feed-id" :method :delete
						  :decorators (@transaction
							       @authenticated)) ()
  (murja.rss.reader-db:delete-feed feed-id (gethash "id" *user*))
  (setf (hunchentoot:return-code*) 204)
  "")

;; This will be called by cron/curl
(defroute update-feeds-rotue ("/api/rss/update" :method :get
						:decorators (@transaction)) ()
  (update-feeds)
  (setf (hunchentoot:return-code*) 204)
    "")
