(defpackage murja
  (:use :cl)
  (:import-from :murja.posts.post-db))

(in-package :murja)

(defvar *server* nil)
(setf hunchentoot:*catch-errors-p* nil)

(defun start-server (&key (port 3010))
  (format t "Starting murja server~%")
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))
    (when (equalp 3010 port)
      (setf *server* server))
    (hunchentoot:start server)
    (format t "Started murja server on ~a ~%" port)
    server))

;;(start-server :port 3010)
