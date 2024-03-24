(defpackage murja.routes.media-routes
  (:use :cl)
  (:import-from :lisp-fixup :slurp-bytes)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.media.media-db :list-pictures :insert-media :select-referencing-posts* :get-media)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.middleware.auth :@authenticated :@can? :*user*)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.media-routes)

(defroute post-pic ("/api/pictures" :method :post
				    :decorators (@transaction
						 @authenticated
						 (@can? "create-post")))
    (&post file)
  (destructuring-bind (tmp-file filename mime) file
    (let* ((bytes (slurp-bytes tmp-file))
	   (result (insert-media filename bytes))
	   (response (make-hash-table)))

      (log:info "Inserting picture ~a of mime ~a" filename mime)
      (when result
	(setf (gethash "id" response)
	      (caar result))
	(stringify response)))))

(defroute all-pics ("/api/pictures/list/all" :method :get
					     :decorators (@transaction
							  @authenticated
							  (@can? "create-post"))) ()
    (let ((pics (list-pictures)))
      (if pics
	  (stringify pics)
	  "[]")))
      
(defroute referencing-route ("/api/pictures/referencing/:guid" :method :get
							       :decorators (@transaction
									    @authenticated)) ()
  (stringify (select-referencing-posts* guid)))

(defroute picture-route ("/api/pictures/:guid" :method :get
					       :decorators (@transaction)) ()
  (let ((pic (aref (get-media guid) 0)))
    ;; murja doesn't persist images' exact mime (yet), and we don't want
    ;; browsers calling straight to this endpoint downloading these images
    ;; (so application/octet-stream can't be used), so we return nil as
    ;; content-type (as the old clj-murja does) for now
    (setf (hunchentoot:content-type*) nil)
    (setf (hunchentoot:header-out "Content-Disposition")
	  (format nil "inline; filename=~a" (gethash "name" pic)))
    (gethash "data" pic)))
