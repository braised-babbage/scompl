(asdf:defsystem #:scompl-tests
  :depends-on (#:scompl #:fiasco)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
	       (:file "passes-tests")))
