(defpackage murja
  (:use :cl)
  (:import-from :murja.posts.post-db)
  (:import-from :murja.middleware.db :with-db)
  (:import-from :murja.migrations :migrate)
  (:export :main :start-server))

(in-package :murja)

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
  (with-open-file (f murja.routes.settings-routes:*log-file* :direction :output :if-does-not-exist :create :if-exists :append)
    (let ((*standard-output* f))
      (start-server :port port :stream f)
      (handler-case
	  (loop do (sleep 1000))
	(condition () nil)))))

(in-package :common-lisp-user)
(defun run ()
  "Starts up the aggressive-murja system. Sets logging up in a way that should show up in the logs view"
  (setf hunchentoot:*catch-errors-p* nil)
  (bordeaux-threads:make-thread
   (lambda ()
     (murja:main))))

;; (start-server :port 3010)
