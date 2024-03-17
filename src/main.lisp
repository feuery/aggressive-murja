(defpackage murja
  (:use :cl)
  (:import-from :murja.posts.post-db :test))

(format t "Loaded murja, calling inner fns: ~a~%" (test 3))
  
