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
Further instruction will follow on screen, but here are some commands:
- 'info': Gives you a general overview.
- 'source': Prints you the link to the source code.
- 'lightning': This is used to simulate a lightning-bolt hitting a plant to produce fire.
- 'debug': Will switch debugging on and off.
- 'quit': Will exit the game.

###or to build:
```
(saveinitmem "an_evolution" :quiet t :norc t :executable t :init-function 'startup)
```
