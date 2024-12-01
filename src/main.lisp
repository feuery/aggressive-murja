(defpackage murja
  (:use :cl)
  (:import-from :murja.posts.post-db)
  (:import-from :murja.middleware.db :with-db)
  (:import-from :murja.migrations :migrate)
  (:export :main :start-server))

(in-package :murja)

;; (setf hunchentoot:*catch-errors-p* nil)

(cl-advice:make-advisable 'hunchentoot:set-cookie)
(cl-advice:add-advice :around
		      'hunchentoot:set-cookie
		      (lambda (next-fn name &key (value "") expires max-age path domain secure http-only (reply hunchentoot:*reply*))
			(let ((session-cookie? (string= name "hunchentoot-session")))
			  (funcall next-fn
				     name
				     :value value
				     :expires expires
				     :max-age (if session-cookie?
						  31536666
						  max-age)
				     :path path
				     :domain domain
				     :secure secure
				     :http-only http-only
				     :reply reply))))
(defun stop-server ()
  (hunchentoot:stop *server*))

(defun start-server (&key (port 3010) stream)
  (format t "Starting murja server~%")
  (with-db
      (migrate))
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))

    (when stream
      (setf (hunchentoot:acceptor-access-log-destination server) stream))
    
    (when (equalp 3010 port)
      (setf *server* server))
    
    (hunchentoot:start server)
    (format t "Started murja server on ~a ~%" port)
    server))

(defun main (&key (port 3010))
  (with-open-file (f murja.routes.settings-routes:*log-file* :direction :output :if-does-not-exist :create)
    (let ((*standard-output* f))
      (start-server :port port :stream f)
      (handler-case
	  (loop do (sleep 1000))
	(condition () nil)))))

;; (start-server :port 3010)
