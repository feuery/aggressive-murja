(defpackage murja.tests.literal-test
  (:use :cl :fiveam)
  (:import-from :murja.tests.literal :literal :drop-while))

(in-package :murja.tests.literal-test)

(def-suite literal-suite)
(in-suite literal-suite)

(def-test literal-helper-function-tests ()
  (is (equalp (list 12 23 5434 34 5 2 34)
	      (drop-while (list 0 1 2 3 4 5 12 23 5434 34 5 2 34) (lambda (x) (< x 10)))))

  (is (equalp `(PROGN
		(DEFUN FAC (N)
		  (IF (EQUALP N 1)
		      1
		      (* N (FAC (1- N)))))
		(LITERAL AND OTHER STUFF LIKE !L (FORMAT T "fac of 7 is ~d~%" (fac 7)) INTO
			 JUST THE QUOTATION-WRAPPED CODE))
	      (macroexpand `(literal Transforms a block of documentation that returns lisp code like
				     !L
				     (defun fac (n)
				       (if (equalp n 1)
					   1
					   (* n (fac (1- n)))))

				     and other stuff like
				     !L
				     (format t "fac of 7 is ~d~%" (fac 7))

				     into just the quotation-wrapped code)))))

(def-test lists-do-not-error ()
    (is (equalp `(progn (format t "lol~%") (literal code ))
		(macroexpand `(literal Dumb documentation (with a parenthesis-wrapped block) that 666 returns this !L (format t "lol~%") code )))))


(if (and (sb-ext:posix-getenv "GHA")
	 (not (run! 'literal-suite)))
    (sb-ext:exit :code 666))
