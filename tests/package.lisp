(fiasco:define-test-package #:scompl-tests
  (:use #:cl)
  (:export #:run-scompl-tests))

(in-package #:scompl-tests)

(defun run-scompl-tests ()
  (run-package-tests :package ':scompl-tests
		     :verbose t
		     :describe-failures t
		     :interactive nil))
