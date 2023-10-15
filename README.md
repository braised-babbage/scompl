# scompl

A small compiler from a subset of Common Lisp to x86-64, based on [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation)

## Example Usage

```
SCOMPL> (defvar test-program 
          (parse-program '(let ((a (+ 10 2)))
		            	   (let ((b (- a)))
            			     b))))
TEST-PROGRAM
SCOMPL> (defvar asm-program (compile test-program))
ASM-PROGRAM
SCOMPL> (x86-64:assemble-and-run asm-program)
244
```

Note that the value returned is the exit code of the resulting program. This is always computed mod 256, hence why 244 appears for the above.
