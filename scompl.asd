(asdf:defsystem #:scompl
  :depends-on (#:trivia)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
	       (:file "ast")
	       (:file "assembler")))
