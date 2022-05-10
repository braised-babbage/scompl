(in-package #:x86-64)

(defvar *standard-boilerplate-main*
  (basic-block
   (label :_main)
   (list
    (pushq :rbp)
    (movq :rsp :rbp)
    (subq 16 :rsp)
    (jmp :start))
   t)
  "The entry point for compiled programs.")

(defvar *standard-boilerplate-conclusion*
  (basic-block
   (label :conclusion)
   (list
    (addq 16 :rsp)
    (popq :rbp)
    (retq)))
  "The exit point for compiled programs.")

(defun wrap-in-standard-boilerplate (program)
  (flet ((find-block (name)
	   (find name (program-blocks program)
		 :key (lambda (blk)
			(label-name (basic-block-label blk))))))
    (unless (find-block ':start)
      (error "Expected a START block, but none was found."))'
    (when (find-block ':_main)
      (error "Found a _MAIN block, but this is supposed to be boilerplate."))
    (when (find-block ':conclusion)
      (error "Found a CONCLUSION block, but this is supposed to be boilerplate."))
    (program (program-info program)
	     (append (list *standard-boilerplate-main*
			   *standard-boilerplate-conclusion*)
		     (program-blocks program)))))
