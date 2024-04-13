(ql:quickload :aggressive-murja)
(save-lisp-and-die "murja_server"
		   :toplevel (lambda ()
			       (murja:main))
		   :executable t)
