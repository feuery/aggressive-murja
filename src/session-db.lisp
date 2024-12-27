(defpackage murja.session.db
  (:use :cl)
  (:import-from :halisql :defqueries)
  (:export :ensure-username-session* :assert-ownership
	   :assert-ownership-username :all-session-vals
	   :login-query-session*
	   :now
	   :set-session-val* :ensure-session*
	   :insert-session* :get-session-val*)) 	   

(in-package :murja.session.db)

(defqueries "session-fns")

(defun now ()
  (or lisp-fixup:*now*
      (simple-date:universal-time-to-timestamp (get-universal-time))))

(defun assert-ownership (user-id session-key)
  (let ((session (ensure-session* (now) user-id session-key)))
    (assert session)
    t))

;; (uuid:make-uuid-from-string "465a810c-25f5-40eb-9ef4-b5c127dcd3f0")
;; (uuid:make-uuid-from-string "lol tää ei kyl oo uuid :D")

(defun assert-ownership-username (username session-key)
  (handler-case
      (uuid:make-uuid-from-string session-key)
    (simple-error (sm)
      (log:warn "~a ~% ~ais not an uuid" sm session-key)
      (return-from assert-ownership-username nil)))
  
  (let ((session (ensure-username-session* (now) username session-key)))
    ;; cast into t/nil
    (not (equalp session nil))))
