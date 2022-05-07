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

(defun equalp-after-transform (program transform)
  (let ((orig (scompl:interpret program))
	(tran (scompl:interpret (funcall transform program))))
    (equalp orig tran)))

(deftest test-uniquify-program-equivalence ()
  (dolist (source *uniquify-programs*)
    (is (equalp-after-transform (scompl:parse-program source)
				#'scompl:uniquify))))

(defvar *remove-complex-operands-programs*
  (list
   '(+ (- 10 2) (- 7))
   '(let ((a (let ((a 3)) a))) a)
   '(let ((a (+ 10 2)))
     (let ((b (- a)))
       b))))

(deftest test-remove-complex-operands-program-equivalence ()
  (dolist (source (append *uniquify-programs*
			  *remove-complex-operands-programs*))
    (is (equalp-after-transform
	 (scompl:parse-program source)
	 #'scompl:remove-complex-operands))))
