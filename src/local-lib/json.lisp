(defpackage murja.json
  (:use :cl)
  (:import-from :binding-arrows :->>)
  (:import-from :com.inuoe.jzon :stringify :parse)
  (:export :bind-json)
  (:documentation "Convenience tools for handling json"))

(in-package :murja.json)

(defmacro bind-json (requireds optionals json &rest body)
  "Explode a json-string into required and optional bindings. When an optional binding is missing from the string, binding's value is nil. When a required is missing, an assertion fails:

```
(bind-json (a sad ) (unknown-key) \"{\\\"sad\\\": 33, \\\"a\\\": 55, \\\"unknown-key\\\": {\\\"inner\\\": \\\"object\\\"}}\"

	   (format t \"a and sad: ~a & ~a~%\" a sad)
	   (when unknown-key
	     (format t \"We have an optional key too: ~a~%\" unknown-key)))
```"
  (let* ((obj-sym (gensym))
	 (req-pairs (->>
		      requireds
		      (mapcar (lambda (sym)
				(list sym `(gethash ,(str:downcase (format nil "~a" sym))  ,obj-sym ))))))
	 (optional-pairs (->>
			   optionals
			   (mapcar (lambda (sym)
				     (list sym `(gethash ,(str:downcase (format nil "~a" sym))  ,obj-sym ))))))
	 (all-bindings (concatenate 'list optional-pairs req-pairs))
	 (req-asserts (->>
		        requireds
			(mapcar (lambda (sym)
				  `(assert ,sym nil ,(format nil "Didn't find key ~a from json" sym)))))))
    `(let ((,obj-sym (parse ,json)))
       (let ,all-bindings
	 ,@req-asserts 
	 ,@body))))
