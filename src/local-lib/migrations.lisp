(defpackage murja.migrations
  (:use :halisql)
  (:use :cl)
  (:export :defmigration :migrate))

(in-package :murja.migrations)

(defparameter *migrations* nil)

(defqueries "migration-helper-queries")

(defun migration-does-exist (name)
  (> (gethash "count" 
	      (aref 
	       (migration-exists name) 0))
     0))

(defun migration-table-exists ()
  (gethash "exists" (aref (migration-table-exists*) 0)))
  
 
(defun defmigration (path &key initial)
  (let* ((filename (asdf:system-relative-pathname *system-name*
						  (format nil "resources/sql/~a.sql" path)))
	 ;; ragtime legacy, migration filenames are named .up.sql but they were saved into the public.ragtime_migrations without the .up.sql postfix
	 ;; and murja.migrations/halisql system drops the .sql extension, but halisql functions don't handle the .up. string correctly
	 (path (str:replace-all ".up" "" path))
	 (fn (lambda ()
	       (cond ((and initial (not (migration-table-exists)))
		      (postmodern:execute-file filename))

		     ((and (migration-table-exists) (not (migration-does-exist path)))
		      (postmodern:execute-file filename))

		     (t (log:info "Didn't run ~a" path)))))
	 (found-migration? nil))
    (dolist (mig *migrations*)
      (when (string= (first mig) path)
	(setf (cdr mig) fn)
	(setf found-migration? t)
	(return)))

    (unless found-migration?
      (push (cons path fn) *migrations*))))

(defun migrate ()
  (postmodern:with-transaction ()
    (dolist (mig (reverse *migrations*))
      (log:info "Running ~a" (car mig))
      (funcall (cdr mig)))))

(defmigration "init-migration-tables" :initial t)

;; (migrate)
