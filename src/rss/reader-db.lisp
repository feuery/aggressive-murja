(defpackage murja.rss.reader-db
  (:use :cl :postmodern :binding-arrows)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :partial :compose)
  (:import-from :cl-date-time-parser :parse-date-time)
  (:export :get-user-feeds :subscribe-to-feed :mark-as-read :delete-feed))
	
(in-package :murja.rss.reader-db)

(defqueries "reader-fns")

(defun parse (key hashmap)
  (setf (gethash key hashmap)
	(com.inuoe.jzon:parse (gethash key hashmap)))
  hashmap)

(defun get-user-feeds (user-id)
  (let* ((feeds (coerce (get-user-feeds* user-id) 'list))
	 (fixed-feeds
	   (mapcar (compose (partial #'parse "items")
			    (partial #'parse "creator"))
		   feeds)))
    (dolist (feed fixed-feeds)
      (setf (gethash "items" feed)
	    (coerce (gethash "items" feed) 'list))
      
      (if (equalp (list 'null) (gethash "items" feed))
	  (setf (gethash "items" feed) #())
	  
	  (dolist (item (gethash "items" feed))
            (setf (gethash "is_read" item)
		  (not (equalp 'NULL (gethash "read_at" item))))

	    ;; frontend doesn't need this
	    (remhash "read_at" item))))

    fixed-feeds))

(defun subscribe-to-feed (feed-name feed-url owner)
  (insert-feed feed-name feed-url (gethash "id" owner)))

(defun download (url)
  "Drakma decides to return either strings or array<byte>s based on random
(can't find the documentation about this logic) whims. This function performs
the http-request through drakma, checks which is returned, and in case of array<byte>s,
pipes it through trivial-utf-8:utf-8-bytes-to-string"
  (let ((result (drakma:http-request url :user-agent "drakma / murja-blog-engine rss updater")))
    (if (and (arrayp result)
	     (not (stringp result)))
	(trivial-utf-8:utf-8-bytes-to-string result)
	result)))

;; (setf drakma:*header-stream* *standard-output*)
(defun get-child-item-value (name children)
  (some->>
    children
    (remove-if-not (lambda (node)
		     (string= (xmls:node-name node)
			      name)))
    first
    xmls:node-children
    first))

(defun update-feed (feed)
  (let* ((url (gethash "url" feed))
	 (feed-id (gethash "id" feed))
	 (feed-contents (download url))
	 (feed-parsed (xmls:parse feed-contents))
	 (channel (first (xmls:node-children feed-parsed))))
    (dolist (item (remove-if-not (lambda (item)
				   (string= (xmls:node-name item) "item"))
				 (xmls:node-children channel)))
      (let ((title (or (get-child-item-value "title" (xmls:node-children item)) ""))
	    (link (get-child-item-value "link" (xmls:node-children item)))
	    (description (get-child-item-value "description" (xmls:node-children item)))
	    (author (or
		     (get-child-item-value "author" (xmls:node-children item))
		     ;; author seems to be optional value, let's get title from <channel> if missing
		     (get-child-item-value "title" (xmls:node-children channel))))
	    (pubDate (cl-epoch:universal->unix-time
		      (parse-date-time (get-child-item-value "pubDate" (xmls:node-children item))))))
	(log:info "Parsed ~a as ~a"
		  (get-child-item-value "pubDate" (xmls:node-children item))
		  pubdate)
	(insert-feed-item title link description author pubDate feed-id)))))

(defun current-hour ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    hour))

(defun current-minute ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    hour))

(defvar *last-updated* nil)

(defun update-feeds ()
  (setf *last-updated* nil)
  (when (or (not *last-updated*)
	    ;; hourly rate limit
	    (> (- (current-hour) *last-updated*) 1))
    (log:info "Updating all feeds")
    (dolist (feed (coerce (get-all-feeds) 'list))
      (update-feed feed))

    (setf *last-updated* (current-hour))))
