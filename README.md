# an_evolution.lisp 

##Introduction:
This is kind of a 'life-simulation' written in CommonLisp.
The code is free,as in freedom, as it is licensed under the terms of the AGPL.
The program is based on an example of the book `Land of Lisp' and got a lot of features added.
At first I wrote it to understand loops, but I got quite a lot of ideas to implement, and so the project became big enough for git.

##Requirements:
- A working CommonLisp (most should work - tested with SBCL)

##How
###to simulate:
```
(load "an_evolution.lisp")
(startup)
```
###or to build:
```
(saveinitmem "an_evolution" :quiet t :norc t :executable t :init-function 'startup)
```
