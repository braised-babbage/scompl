(in-package #:scompl-tests)

(defun preserves-meaning (program &key transform
				    (eval-orig #'scompl:interpret)
				    (eval-transformed #'scompl:interpret)
				    (cmp #'equalp))
  "Check that the meaning of PROGRAM is preserved by TRANSFORM."
  (let ((orig (funcall eval-orig program))
	(tran (funcall eval-transformed (funcall transform program))))
    (funcall cmp orig tran)))

(defvar *uniquify-cases*
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

(deftest test-uniquify-program-equivalence (&optional (cases *uniquify-cases*))
  "Check that every program in CASES has meaning preserved by UNIQUIFY."
  (dolist (source cases)
    (is (preserves-meaning (scompl:parse-program source)
			   :transform #'scompl:uniquify))))

(defvar *remove-complex-operands-cases*  
  (append
   (list
    '(+ (- 10 2) (- 7))
    '(let ((a (let ((a 3)) a))) a)
    '(let ((a (+ 10 2)))
      (let ((b (- a)))
	b)))
   *uniquify-cases*))

(deftest test-remove-complex-operands-program-equivalence (&optional (cases *remove-complex-operands-cases*)) 
  (dolist (source cases)
    (is (preserves-meaning
	 (scompl:parse-program source)
	 :transform #'scompl:remove-complex-operands))))

(defvar *explicate-control-cases*
  (list
   '(let ((y (let ((x 20))
	       (let ((z 22))
		 (+ x z)))))
     y)
   '(let ((z 10))
     (+ z z))
   '(let ((x (let ((y (- 10 5))) y)))
     (+ x x))))

(deftest test-explicate-control-program-equivalence (&optional (cases *explicate-control-cases*))
  (dolist (source cases)
    (is (preserves-meaning (scompl:parse-program source)
			   :transform #'scompl:explicate-control
			   :eval-transformed #'scompl:interpret-cvar))))
