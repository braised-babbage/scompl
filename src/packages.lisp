(defpackage #:x86-64
  (:use :cl)
  (:shadow #:deref)
  (:export
   #:imm
   #:reg
   #:deref
   #:var
   #:label
   
   #:addq
   #:subq
   #:negq
   #:movq
   #:pushq
   #:popq
   #:retq
   #:callq
   #:jmp
   
   #:basic-block
   #:basic-block-info
   #:basic-block-globalp
   #:basic-block-label
   #:basic-block-instrs
   
   #:program
   #:program-info
   #:program-blocks
   
   #:print-assembly
   #:print-program

   ;; passes
   #:assign-homes
   #:patch-instructions
   #:wrap-in-standard-boilerplate   
   
   #:assemble-and-run))

(defpackage #:scompl
  (:use :cl)
  (:export
   ;; passes
   #:uniquify
   #:remove-complex-operands
   #:explicate-control
   #:select-x86-instructions

   #:interpret
   #:interpret-cvar
   
   #:parse-program
   #:print-program))
