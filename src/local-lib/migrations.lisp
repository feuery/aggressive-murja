(defpackage murja.migrations
  (:use :halisql)
  (:use :cl)
  (:export :migration-does-exist :deflispmigration :defmigration :migrate))

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

(defmacro deflispmigration (filename-sym path &rest body)
  `(let* ((,filename-sym (asdf:system-relative-pathname *system-name*
							(format nil "resources/sql/~a.sql" ,path)))
	  ;; ragtime legacy, migration filenames are named .up.sql but they were saved into the public.ragtime_migrations without the .up.sql postfix
	  ;; and murja.migrations/halisql system drops the .sql extension, but halisql functions don't handle the .up. string correctly
	  (path (str:replace-all ".up" "" ,path))
	  (fn (lambda ()
		,@body))
	  (found-migration? nil))
     (dolist (mig *migrations*)
       (when (string= (first mig) path)
	 (setf (cdr mig) fn)
	 (setf found-migration? t)
	 (return)))

     (unless found-migration?
       (push (cons path fn) *migrations*))))

(defun defmigration (file-path &key initial)
  (deflispmigration filename file-path
    (log:info "Migration result: ~a" 
      (cond ((and initial (not (migration-table-exists)))
	     (postmodern:execute-file filename)
	     (mark-migration-done path))

	    ((and (migration-table-exists) (not (migration-does-exist path)))
	     (log:info "Really running ~a" path)
	     (postmodern:execute-file filename)
	     (mark-migration-done path))

	    (t (log:info "Didn't run ~a" path))))))

(defun migrate ()
  (postmodern:with-transaction ()
    (dolist (mig (reverse *migrations*))
      (log:info "Running ~a" (car mig))
      (funcall (cdr mig)))))

(defmigration "init-migration-tables" :initial t)

;; (migrate)
