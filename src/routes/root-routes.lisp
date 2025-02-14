(defpackage murja.routes.root-routes
  (:use :cl)
  (:import-from :binding-arrows :->> :->)
  (:import-from :lisp-fixup :partial)
  (:import-from :murja.middleware.db :@transaction)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :easy-routes :defroute)
  (:local-nicknames (:user-db :murja.users.user-db)))

(in-package :murja.routes.root-routes)

(defparameter *allowed-resources*
  (let ((result nil))
    (cl-fad:walk-directory
     (asdf:system-relative-pathname halisql:*system-name*
				    "resources/")
     (lambda (n)
       (push n result))
     :directories nil
     )

    (reduce (lambda (hash path)
	      (let ((filename (file-namestring path)))
		(setf (gethash (if (string= "elm.js" filename)
				   "murja.js"
				   filename)
			       hash)
		     path)
	       hash))
	     (->> result 
	       (mapcar (partial #'format nil "~a"))
	       (remove-if (partial #'str:ends-with-p "~"))
	       (remove-if (partial #'str:ends-with-p ".sql"))
	       (mapcar #'pathname))
	     :initial-value (make-hash-table :test 'equalp))))

(define-condition unknown-mime (error)
  ((file-type :initarg :file-type
              :initform nil
              :accessor file-type))
  ;; the :report is the message into the debugger:
  (:report (lambda (condition stream)
	     (format stream
		     "Don't know how to transform file of type ~a to a mime type"
		     (file-type condition)))))

(defun path->mime (path)
  (let ((type (pathname-type path)))
    (cond ((string= type "js") "text/javascript")
	  ((string= type "css") "text/css")
	  (t (error 'unknown-mime :file-type type)))))

(defun get-resource (file)
  (let ((path (gethash file *allowed-resources*)))
    (if path
	(let ((source (lisp-fixup:slurp-utf-8 path)))
	  (setf (hunchentoot:content-type*) (path->mime path))
	  source)
	(progn
	  (setf (hunchentoot:return-code*) 404)
	  ""))))
(defroute resources ("/resources/:file" :method :get) ()
  (get-resource file))

;; (defroute resources ("/blog/resources/:file" :method :get) ()
;;   (get-resource file))

(defun @check-if-initial (next)
  (if (user-db:no-users?)
      (progn
	(setf (hunchentoot:return-code*) 302)
	(setf (hunchentoot:header-out :location) "/blog/initial-setup")
	"")
      (funcall next)))

(defvar *root* 
  "<!DOCTYPE html>
<html xmlns:of=\"http://ogp.me/ns#\"
      xmlns:fb=\"http://www.facebook.com/2008/fbml\">
  <head>
    <link href=\"/resources/murja.css\" rel=\"stylesheet\" type=\"text/css\">
    <script src=\"https://unpkg.com/ace-custom-element@latest/dist/index.min.js\" type=\"module\"></script>
    <meta charset=\"UTF-8\" />
    <script src=\"/resources/murja.js\"></script>
  </head>
  <body>
    <div id=\"app\" />
    <script src=\"/resources/murja-helper.js\"></script>
  </body>
</html>")

(defroute root ("/" :method :get
		    :decorators (murja.middleware.db:@transaction
				 @check-if-initial)) ()
  *root*)

(defroute spurasdasdasd ("/blog/initial-setup" :method :get) ()
  *root*)

(defroute root-blg ("/blog/" :method :get
			     :decorators (murja.middleware.db:@transaction
					  @check-if-initial)) ()
  *root*)

(defroute root-blg-new ("/blog/new_post" :method :get) ()
  *root*)

(defroute root-blg-newer ("/blog/new_post/" :method :get) ()
  *root*)

(defroute mediamgr ("/blog/mediamanager" :method :get) ()
  *root*)

(defroute postmgr ("/blog/postadmin" :method :get) ()
  *root*)


(defroute post-view ("/blog/post/:id" :method :get) ()
  *root*)

(defroute edit ("/blog/post/edit/:id" :method :get) ()
  *root*)

(defroute root-blg-2 ("/blog/tags/:tag" :method :get) ()
  *root*)

(defroute sdkfpsokopfs ("/blog/post/:post/version/:ver" :method :get) ()
  *root*)

(defroute ddddddd ("/blog/settings" :method :get) ()
  *root*)

(defroute asjdisdjfiosd ("/blog/logs" :method :get) ()
  *root*)

(defroute aesdfg ("/blog/settings" :method :get) ()
  *root*)


(defroute ddddddd1234 ("/blog/page/:page" :method :get) ()
  *root*)

(defroute sdfdsfopsf ("/blog/feeds" :method :get) ()
  *root*)

(defroute kgvfokgf ("/blog/usersettings" :method :get) () *root*)
