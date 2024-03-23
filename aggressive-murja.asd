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
	       "cl-advice")
  :description "A rewrite of the <a href=\"https://github.com/feuery/murja-blog/\">murja blogging engine</a> in lisp"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")
			       (:file "halisql")))
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

		 (:module "routes"
		  :components
		  ((:file "login-routes")
		   (:file "post-routes")
		   (:file "media-routes")
		   (:file "root-routes")))
		 (:file "main")))))


		 
