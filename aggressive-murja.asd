(defsystem "aggressive-murja"
  :version "3.0.0-devel"
  :author "Ilpo Lehtinen"
  :licence "GPLv3"
  :depends-on ("postmodern"
               "simple-date"
               "simple-date/postgres-glue"
	       "ironclad"
	       "trivial-utf-8"
	       "binding-arrows"
	       "hunchentoot"
	       "cl-ppcre"
	       "parse-number"
	       "com.inuoe.jzon"
	       "easy-routes"
	       "drakma"
               "str"
	       "cl-fad"
	       "log4cl"
	       "cl-advice"
	       "xml-emitter"
	       "drakma"
	       "xmls"
	       "cl-date-time-parser")
  :description "A rewrite of the <a href=\"https://github.com/feuery/murja-blog/\">murja blogging engine</a> in lisp"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")
			       (:file "halisql")
			       (:file "migrations")))
		 (:file "migration-list")
		 (:module "users"
		  :components ((:file "user-db")))
		 (:module "middleware"
		  :components ((:file "json")
			       (:file "db")
			       (:file "auth")))
		 (:module "posts"
		  :components
		  ((:file "post-db")))

		 (:module "media"
		  :components
		  ((:file "media-db")))

		 (:module "rss"
		  :components
		  ((:file "reader-db")))

		 (:module "routes"
		  :components
		  ((:file "settings-routes")
		   (:file "login-routes")
		   (:file "post-routes")
		   (:file "media-routes")
		   (:file "rss-routes")
		   (:file "rss-reader-routes")
		   (:file "root-routes")))
		 (:file "main"))))
  :in-order-to ((test-op (test-op "pichunter/tests"))))

(defsystem "aggressive-murja-tests"
  :author  "Ilpo Lehtinen"
  :licence "GPLv3"
  :depends-on ("aggressive-murja"
	       "fiveam")
  :components ((:module "test"
		:components
		((:file "literal")
		 (:file "literal-test")
		 (:file "tests"))))
  :perform (test-op (op c)
		    (eval (read-from-string "(fiveam:run! 'murja.tests:main-suite)"))))
