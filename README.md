# Implementing a Logical Type System in the Villain Compiler Framework


This repository contains the source code to three extensions to Villain that make it closer to Typed Racket. The paper attached to this repository explains the details of these incremental extensions. While other files to Villain have been changed for each language, the file ```typecheck.rkt``` is the most significant extension.


## Installing

To run and look at the source code for either implementation, you would need to install the folder, go into the directory of the language you want, and go to the directory named ```Villain```.

Ex:
```
raco pkg install TypedAlpha/
cd TypedAlpha
cd Villain
```

## Testing 

From there, you could run test programs with ```compile-file.rkt``` that will type check and compile programs or go to the test folder and run ```compile.rkt``` to run the tests. 
```
racket -t compile-file.rkt -m text.rkt
cd test
racket compile.rkt
```
```test-runner.rkt``` contains these tests. The commented tests are of programs that would fail typing check cause a type mismatch error to be thrown crashing the entire program.
