(defpackage murja.rss.reader-db
  (:use :cl :postmodern :binding-arrows)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :partial :compose)
  (:import-from :cl-date-time-parser :parse-date-time)
  (:export :*last-updated* :get-feed-name-and-url :*updates* :get-user-feeds :subscribe-to-feed :mark-as-read :delete-feed :download))
	
(in-package :murja.rss.reader-db)

(defparameter unix-epoch
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix (universal-time)
  (- universal-time unix-epoch))

(defqueries "reader-fns")

(defun parse (key hashmap)
  (setf (gethash key hashmap)
	(com.inuoe.jzon:parse (gethash key hashmap)))
  hashmap)

(defun get-user-feeds (user-id archived?)
  (let* ((feeds (coerce (get-user-feeds* user-id archived?) 'list))
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

(defun download (url modified-since)
  "Drakma decides to return either strings or array<byte>s based on random
(can't find the documentation about this logic) whims. This function performs
the http-request through drakma, checks which is returned, and in case of array<byte>s,
pipes it through trivial-utf-8:utf-8-bytes-to-string.

The second value returned is the last-modified response header as simpledate-timestamp"
  (multiple-value-bind (body status headers) (drakma:http-request url
								  :user-agent "drakma / murja-blog-engine rss updater"
								  :additional-headers (when modified-since
											(list (cons "If-Modified-Since" modified-since))))
    (let* ((last-modified-header (cdr (assoc :last-modified headers)))
	   (_ (log:info "last-modified-header: ~a~%" last-modified-header))
	   (last-modified (lisp-fixup:if-modified-since->simpledate-timestamp
			   last-modified-header)))
      (log:info "last-modified: ~a~%" last-modified)

      (values
       (if (and (arrayp body)
		(not (stringp body)))
	   (trivial-utf-8:utf-8-bytes-to-string body)
	   body)
       last-modified))))

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

(defun parse-rss (feed-id feed-parsed)
  (log:info "Parsing rss")
  (let ((channel (first (xmls:node-children feed-parsed))))
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
	    (pubDate (universal-to-unix
		      (parse-date-time (get-child-item-value "pubDate" (xmls:node-children item))))))
	(log:info "Parsed ~a as ~a"
		  (get-child-item-value "pubDate" (xmls:node-children item))
		  pubdate)
	(insert-feed-item title link description author pubDate feed-id)))))

(defun parse-atom (author feed-id feed-parsed)
  (log:info "parsing atom~%")
  (let ((entries (->>
		   feed-parsed
		   (xmls:node-children)
		   (remove-if-not (lambda (node)
				    (string= (xmls:node-name node) "entry"))))))
    (dolist (entry entries)
      (let ((title (or (get-child-item-value "title" (xmls:node-children entry)) ""))
	    (link (get-child-item-value "id" (xmls:node-children entry))) ;; atom calls hrefs 'id'
	    (description (get-child-item-value "content" (xmls:node-children entry)))
	    (pubDate (universal-to-unix
		      (parse-date-time (get-child-item-value "updated" (xmls:node-children entry))))))

	(log:info "Inserting ~a~%" (list :title title :link link :description description :author author :pubDate pubDate :feed-id feed-id))
	(insert-feed-item title link description author pubDate feed-id)))))

(defun update-feed (feed)
  (let* ((if-modified-since (gethash "last_modified" feed))
	 (if-modified-since (unless (equalp if-modified-since :null)
			      if-modified-since))
	 (url (gethash "url" feed))
	 (feed-id (gethash "id" feed)))
    (log:info "Updating feed ~a~%" url)
    (multiple-value-bind (feed-contents last-modified) (download url if-modified-since)
      (let* ((feed-parsed (xmls:parse feed-contents))
	     (feed-ns (xmls:node-ns feed-parsed)))

	(log:info "did we get rss or atom?")
	(if (and feed-ns
		 (cl-ppcre:all-matches-as-strings "Atom" feed-ns))
	    (let ((author (first (xmls:node-children
				  (get-child-item-value "author" (xmls:node-children feed-parsed))))))
	      (parse-atom author feed-id feed-parsed))
	    (parse-rss feed-id feed-parsed))

	(log:info "Updating ~a last-modified to ~a~%" feed-id last-modified)
	(update-last-modified feed-id last-modified)
	(log:info "Updated~%")))))

(defun current-hour ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    hour))

(defun current-minute ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    hour))

(defun current-datetime ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defvar *last-updated* nil)
(defvar *updates* nil)

(defun update-feeds ()
  (when (or (not *last-updated*)
	    ;; hourly rate limit
	    (> (- (current-hour) *last-updated*) 1))
    (log:info "Updating all feeds~%")
    (dolist (feed (coerce (get-all-feeds) 'list))
      (update-feed feed))

    (setf *last-updated* (current-hour))
    (push (current-datetime) *updates*)))
