(in-package #:scompl-tests)


(defvar *uniquify-programs*
  (list
   '(+ 5 (- 3))
   '(let ((x 1)) x)
   '(let ((x 1))
     (let ((x x))
       x))
   '(let ((x (let ((x 1)) x)))
     x)
   '(+
     (let ((x 1)) x)
     (let ((y 1))
       (let ((x y))
	 x)))))

(deftest test-uniquify-program-equivalence ()
  (dolist (source *uniquify-programs*)
    (let* ((program (scompl:parse-program source))
	   (transformed (scompl:uniquify program)))
      (is (equalp (scompl:interpret program)
		  (scompl:interpret transformed))))))
