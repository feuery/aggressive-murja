(require 'asdf)
(in-package :asdf-user)

(defsystem "aggressive-murja"
  :version "3.0.0"
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
	       ;; works in cl universal time (epoch at 1900)
	       "cl-date-time-parser"
	       "alexandria"
	       "uuid"
	       "cl-hash-util")
  :description "A rewrite of the <a href=\"https://github.com/feuery/murja-blog/\">murja blogging engine</a> in lisp"
  :components ((:module "src"
		:components
		((:module "local-lib"
		  :components ((:file "lisp-fixup")
			       (:file "halisql")
			       (:file "migrations")
			       (:file "json")))
		 (:file "migration-list")
		 (:module "users"
		  :components ((:file "user-db")))
		 (:file "session-db")
		 (:module "middleware"
		  :components ((:file "json")
			       (:file "db")
			       (:file "auth")))
		 (:file "session")
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
		   (:file "user-editor")
		   (:file "root-routes")))
		 (:file "main"))))
  :in-order-to ((test-op (test-op "aggressive-murja/tests"))))

(defsystem "aggressive-murja/tests"
  :author  "Ilpo Lehtinen"
  :licence "GPLv3"
  :depends-on ("aggressive-murja"
	       "fiveam")
  :components ((:module "test"
		:components
		((:file "literal")
		 (:file "literal-test")
		 (:file "tests")
		 (:file "rss-tests")
		 (:file "session-tests"))))
  :perform (test-op (op c)
		    (eval (read-from-string "(fiveam:run! 'murja.tests:main-suite)"))))

;; (asdf:make "aggressive-murja")
;; (asdf:make "aggressive-murja/tests")
;; (murja:start-server)
