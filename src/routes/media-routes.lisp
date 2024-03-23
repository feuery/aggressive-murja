(defpackage murja.routes.media-routes
  (:use :cl)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :murja.middleware.db :@transaction)
  (:import-from :murja.media.media-db :list-pictures)
   
  (:import-from :murja.middleware.json :@json)
  (:import-from :murja.middleware.auth :@authenticated :@can? :*user*)
  (:import-from :easy-routes :defroute))

(in-package :murja.routes.media-routes)

(defroute all-pics ("/api/pictures/list/all" :method :get
					     :decorators (@transaction
							  @authenticated
							  (@can? "create-post"))) ()
    (let ((pics (list-pictures)))
      (if pics
	  (stringify pics)
	  "[]")))
      
