(defpackage murja.routes.settings-routes
  (:use :cl)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :partial)
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

(defun get-logs ()
  (->> 
    (lisp-fixup:slurp-utf-8 *log-file*)
    (str:lines)))

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
	  (get-logs)
	  (mapcar #'logify)
	  stringify)
	(->>
	  (list "no logs found")
	  (mapcar #'logify)
	  stringify))))

(defroute post-logs-groups ("/api/logs/groups" :method :post
					       :decorators (@transaction
							    @json
							    @authenticated
							    (@can? "update-settings"))) ()
  (let* ((body-str (hunchentoot:raw-post-data :force-text t))
	 (body (coerce (parse body-str) 'list)))
    (log:info "Trying to save ~a~%" body-str)
    (postmodern:execute "DELETE FROM blog.log_group;")
    (dolist (group body)
      (postmodern:execute "INSERT INTO blog.log_group (name, alarmy) VALUES ($1, $2)"
			  (gethash "name" group)
			  (gethash "alarmy" group)))

    (setf (hunchentoot:return-code*) 204)
    ""))

(defun get-groups ()
  (coerce 
   (postmodern:query "SELECT name, alarmy FROM blog.log_group" :array-hash)
   'list))

(defun count-reads (groups logs)
  "Takes in whatever groups there are in the database, current logs and returns counts of each group in a hashmap"
  (reduce (lambda (acc group-regex)
	    (let ((count (->>
			   logs
			   (remove-if-not (partial #'cl-ppcre:scan group-regex))
			   length)))
	      (setf (gethash group-regex acc)
		    count)
	      acc))
	  (->>
	    groups
	    (mapcar (partial #'gethash "name")))
	  :initial-value (make-hash-table)))

(defqueries "log-fns")

(defun db-counts (user-id)
  (let ((counts (coerce (get-log-group-counts* user-id) 'list)))
    (format t "counts: ~a~&" (mapcar #'alexandria:hash-table-alist counts))
    (reduce (lambda (acc hash-table)
		     (setf (gethash (gethash "name" hash-table) acc)
			   (gethash "read_count" hash-table))
	      acc)
	    counts
	    :initial-value (make-hash-table :test 'equal))))


(defroute get-logs-groups ("/api/logs/groups" :method :get 
					      :decorators (@transaction
							   @json
							   @authenticated
							   (@can? "update-settings"))) ()
  (let ((user-id (gethash "id" *user*)))
    (assert user-id)
    (let* ((groups (get-groups))
	   (read-counts (count-reads groups (get-logs)))
	   (db-counts (db-counts user-id))
	   (alarmy-groups (->>
			    groups
			    (remove-if-not (partial #'gethash "alarmy"))
			    (remove-if (lambda (group)
					 (let ((name (gethash "name" group)))
					   (= (or (gethash name db-counts) 0)
					      (gethash name read-counts)))))
			    (mapcar (partial #'gethash "name")))))

      (dolist (group groups)
	(let ((name (gethash "name" group)))
	  (if (member name alarmy-groups)
	      (setf (gethash "sound-alarm" group) t)
	      (setf (gethash "sound-alarm" group) nil))))

      (dolist (k (alexandria:hash-table-alist read-counts))
	(destructuring-bind (group . count) k
	  (log:info "Updating ~a to ~d~%" group count)
	  (if (readcount-exists? user-id group)
	      (update-readcount* count user-id group)
	      (insert-readcount* count user-id group))))
      
       (stringify groups))))
