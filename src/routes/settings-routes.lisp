(defpackage murja.routes.settings-routes
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :binding-arrows :->>)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?)
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :easy-routes :defroute)
  (:export :get-settings :*log-file*))

(in-package :murja.routes.settings-routes)

(defun get-settings ()
  (reduce (lambda (acc pair)
	    (destructuring-bind (k v) pair
	      (setf (gethash k acc) (com.inuoe.jzon:parse v))
	      acc)) 
	  (postmodern:query "SELECT key, value FROM blog.Settings")

	  :initial-value (make-hash-table :test 'equal)))

(defroute client-settings ("/api/settings/client-settings" :method :get
							   :decorators (@transaction
									@json)) ()
  (com.inuoe.jzon:stringify
   (get-settings)))

(defroute update-setting ("/api/settings/client-settings" :method :put 
							  :decorators (@transaction
								       @json
								       @authenticated
								       (@can? "update-settings"))) ()
  (let ((req (alexandria:hash-table-alist
	      (parse (hunchentoot:raw-post-data :force-text t)))))
    (dolist (p req)
      (destructuring-bind (k . v) p
	(format t "execute returned for ~a => ~a: ~a~%" k v
		(postmodern:execute "UPDATE blog.Settings SET value = $2 WHERE key = $1" k (stringify v)))))
    (setf (hunchentoot:return-code*) 204)
    ""))

(defvar *log-file* (asdf:system-relative-pathname :aggressive-murja "murja.log"))

(defroute api-logs ("/api/logs" :method :get
				:decorators (@transaction
					     @json
					     @authenticated
					     (@can? "update-settings"))) ()
  (log:info "~a is asking for logs ~%" *user*)
  (labels ((logify (l)
	     (let ((ht (make-hash-table)))
	       (setf (gethash "row" ht) l)
	       ht)))
    (if (uiop:file-exists-p *log-file*)
	(->>
	  (lisp-fixup:slurp-utf-8 *log-file*)
	  (str:lines)
	  (mapcar #'logify)
	  stringify)
	(->>
	  (list "no logs found")
	  (mapcar #'logify)
	  stringify))))
