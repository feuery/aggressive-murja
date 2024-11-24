(defpackage murja.tests.rss 
  (:use :cl :fiveam)
  (:import-from :murja.users.user-db :register-user)
  (:import-from :murja.rss.reader-db :update-feeds)
  (:import-from :easy-routes :defroute)
  (:import-from :halisql :*system-name*)
  (:import-from :lisp-fixup :slurp-utf-8)
  (:import-from :murja.tests :prepare-db-and-server :drakma->string :url :main-suite :prepare-db-and-server))

(in-package :murja.tests.rss)
(in-suite main-suite)

;; let's mock a few rss feeds that return something sensible in their Last-Modified
(defvar rss-sample (slurp-utf-8
		    (asdf:system-relative-pathname *system-name*
						   "resources/test/rss-sample.xml")))
(defvar rss1-hook nil)

(defvar rss1route-called nil)
(defroute test-route ("/api/rssdemo1" :method :get) ()
  (assert (not (equalp rss1route-called nil)))
  (incf rss1route-called)
  (format t "Returning rss from rssdemo1, callcounter is ~d~%" rss1route-called)
  (when rss1-hook
    (log:info "calling test hook #1~%")
    (funcall rss1-hook))
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (setf (hunchentoot:header-out "Last-Modified") "Mon, 06 May 2024 23:23:23 GMT")
  rss-sample)

(defvar rss2route-called nil)
(defvar rss2-hook nil)
(defroute test-route2 ("/api/rssdemo2" :method :get) ()
  (assert (not (equalp rss2route-called nil)))
  (incf rss2route-called)
  (format t "Returning rss from rssdemo2, callcounter is ~d~%" rss2route-called)
  (when rss2-hook
    (log:info "rss2-hook: ~a" rss2-hook)
    (funcall rss2-hook))
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (setf (hunchentoot:header-out "Last-Modified") "Mon, 01 Jan 2024 22:21:20 GMT")
  (log:info "returning from rssdemo2~%")
  rss-sample)

(defvar amount-of-if-modified-sinces nil)
(defvar is-called nil)

;; the actual tests 
(def-test reader-test (:fixture prepare-db-and-server)
  (register-user "testuser" "Testuser" "" "passw0rd")
  (setf rss1route-called 0)
  (setf rss2route-called 0)
  (setf amount-of-if-modified-sinces 0)
  (unwind-protect 
       (let ((feed-urls (mapcar
			 (lisp-fixup:partial #'format nil "~a/api/rssdemo~d" (url))
			 (lisp-fixup:range2 1 3)))
	     (feed-names (mapcar
			  (lisp-fixup:partial #'format nil "rss-~d")
			  (lisp-fixup:range2 1 3)))
	     (owner (caar (postmodern:query "SELECT ID FROM blog.Users"))))
	 (is (not (equalp nil owner)))

	 ;; insert the test feeds (update) shall poll
	 (dotimes (i 2)
	   (postmodern:execute "INSERT INTO blog.feed_subscription (name, url, owner) VALUES ($1, $2, $3)"
			       (nth i feed-names)
			       (nth i feed-urls)
			       owner))

	 ;; ;; these are supposed to be NULL each
	 (let ((last-modifieds (mapcar #'car (postmodern:query "SELECT last_modified FROM blog.feed_subscription"))))
	   (is (equalp 2
		       (length last-modifieds)))

	   
	   (is (every (lisp-fixup:partial #'equalp :null) last-modifieds)))

	 (update-feeds)

	 ;; both feed endpoints are called
	 (is (equalp 1 rss1route-called))
	 (is (equalp 1 rss2route-called))

	 ;; these are now supposed be something sensible
	 (let ((last-modifieds (mapcar #'car (postmodern:query "SELECT last_modified FROM blog.feed_subscription"))))
	   (is (every (complement (lisp-fixup:partial #'equalp :null)) last-modifieds))

	   ;; let's use hooks to see if the last last-modified is sent in the if-modified-since
	   (let ((hook (lambda ()
			       (setf is-called t)
			       (log:info "Set is-called into t; if-modified-since: ~a~%" (hunchentoot:header-in* "if-modified-since"))
			       
			       (when (hunchentoot:header-in* "if-modified-since")
				 (incf amount-of-if-modified-sinces)))))
	     (setf rss1-hook hook)
	     (setf rss2-hook hook)
	     
	     (setf murja.rss.reader-db:*last-updated* nil)
	     (log:info "re-updating feeds")
	     (update-feeds)
	     (is (not (equalp nil is-called)))
	     (is (equalp amount-of-if-modified-sinces 2)))))
    (setf rss1route-called nil)
    (setf rss2route-called nil)

    (setf rss1-hook nil)
    (setf rss2-hook nil)
    (setf amount-of-if-modified-sinces nil)
    (setf is-called nil)))

;; (setf fiveam:*run-test-when-defined* t)

(if (and (sb-ext:posix-getenv "GHA")
	 (not (run! 'main-suite)))
    (sb-ext:exit :code 666))
