(defpackage :murja.media.media-db
  (:use :cl :postmodern)
  (:import-from :com.inuoe.jzon :parse)
  (:import-from :halisql :defqueries)
  (:import-from :lisp-fixup :fix-timestamp)
  (:export))

(in-package :murja.media.media-db)

(defqueries "media-fns")

(defun list-pictures ()
  (list-pictures*))
  
