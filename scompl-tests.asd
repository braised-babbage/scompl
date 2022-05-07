(asdf:defsystem #:scompl-tests
  :depends-on (#:scompl #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':scompl-tests
                                           '#:run-scompl-tests))
  :serial t
  :pathname "tests/"
  :components ((:file "package")
	       (:file "passes-tests")))
