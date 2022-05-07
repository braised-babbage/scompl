(asdf:defsystem #:scompl
  :depends-on (#:trivia)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
	       (:file "interpret")
	       (:module "langs"
		:serial t
		:components ((:file "lint")
			     (:file "lvar")))
	       (:file "parser")
	       (:file "assembler")))
