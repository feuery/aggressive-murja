(defpackage murja.tests.literal
  (:use :cl)
  (:import-from :lisp-fixup :partial)
  (:export :literal :drop-while))

(in-package :murja.tests.literal)

(defun drop-while (list pred)
  (if (and (first list) (funcall pred (first list)))
      (drop-while (rest list) pred)
      list))

(defmacro literal (&rest body)
  "(literal Transforms a block of documentation that returns lisp code like !L (defun fac (n) (if (equalp n 1) 1 (* n (fac (1- n))))) and other stuff like !L (format t \"fac of 7 is ~d~%\" (fac 7)) into just the quotation-wrapped code)
=>
 (progn
   (defun fac (n) (if (equalp n 1) 1 (* n (fac (1- n)))))
   (format t \"fac of 7 is ~d~%\" (fac 7)))"
  (let ((body (rest (drop-while body (complement (partial #'string= '!L))))))
    (if body
	`(progn
	   ,(first body)
	   (literal ,@(rest body))))))
	
