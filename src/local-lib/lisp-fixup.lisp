(defpackage lisp-fixup
  (:use :cl)
  (:export :if-modified-since->simpledate-timestamp :*rfc822*
	   :sha-512 :partial
	   :compose :drop
	   :slurp-bytes :slurp-utf-8))

(in-package :lisp-fixup)

(defun sha-512 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha512
                              (ironclad:ascii-string-to-byte-array str))))

;; https://www.n16f.net/blog/reading-files-faster-in-common-lisp/
(defun slurp-bytes (path)
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type '(unsigned-byte 8))
      (loop
        (let* ((capacity (array-total-size data))
               (nb-left (- capacity offset)))
          (when (< nb-left block-size)
            (let ((new-length (max (+ capacity (- block-size nb-left))
                                   (floor (* capacity 3) 2))))
              (setf data (adjust-array data new-length)))))
        (let ((end (read-sequence data file :start offset)))
          (when (= end offset)
            (return-from slurp-bytes (adjust-array data end)))
          (setf offset end))))))

(defun slurp-utf-8 (path)
  (trivial-utf-8:utf-8-bytes-to-string (slurp-bytes path)))

(defun drop (n lst)
  "Returns a sequence that skips the first N elements of the given list."
  (cond ((or (null lst) (<= n 0)) lst)
        ((> n 0) (drop (1- n) (cdr lst)))))

(defun partial (f &rest args)
  (lambda (&rest rst-args)
    (apply f (concatenate 'list args rst-args))))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defvar *rfc822* nil)

(defun weekday->string (day)
  (case day
    (1 "Mon")
    (2 "Tue")
    (3 "Wed")
    (4 "Thu")
    (5 "Fri")
    (6 "Sat")
    (0 "Sun")
    (t "")))

(defun month->string (day)
  (case day
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")
    (t "")))

(defun month->ordinal (month)
  (alexandria:switch (month :test #'equal)
    ("January" 1)
    ("February" 2)
    ("March" 3)
    ("April" 4)
    ("May" 5)
    ("June" 6)
    ("July" 7)
    ("August" 8)
    ("September" 9)
    ("October" 10)
    ("November" 11)
    ("December" 12)
    (t "")))

(defun fix-timestamp (timestamp)
  "Fixes timestamps returned from postmodern to a json-format elm can parse" 
  (multiple-value-bind (year month day hour minute second millisec)
      (simple-date:decode-timestamp timestamp)
    (let ((weekday (simple-date:day-of-week timestamp)))

      (if *rfc822*
	  (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d EST"
		  (weekday->string weekday)
		  day
		  (month->string month)
		  year
		  hour minute second)
	  (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))))

;; Wed, 30 March 2016 08:09:00 GMT
(defun if-modified-since->simpledate-timestamp (header)
  (let* ((header (str:trim (second (str:split #\, header)))))
    (destructuring-bind (day month year timestamp gmt ) (str:split #\Space header)
      (destructuring-bind (h m sec) (str:split #\: timestamp)
	(let ((month (month->ordinal month)))
	  (apply #'simple-date:encode-timestamp
		 (mapcar (lambda (e)
			   (if (stringp e)
			       (parse-integer e)
			       e))
			 (list year month day h m sec))))))))
