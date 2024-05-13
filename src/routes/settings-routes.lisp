(defpackage murja.routes.settings-routes
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:import-from :murja.middleware.auth :@authenticated :*user* :@can?)
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.settings-routes)

(defroute client-settings ("/api/settings/client-settings" :method :get
							   :decorators (@transaction
									@json)) ()
  (com.inuoe.jzon:stringify
   (reduce (lambda (acc pair)
	     (destructuring-bind (k v) pair
	       (setf (gethash k acc) (com.inuoe.jzon:parse v))
	       acc)) 
	   (postmodern:query "SELECT key, value FROM blog.Settings")

	   :initial-value (make-hash-table))))

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
