#|
    an_evolution.lisp version git-current
    Copyright (C) 2013  @d3f (http://identi.ca/d3f)
 
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

;; At first load all the needed files. This was made to have a better structured source-code.
(load "define.lisp")
(load "move-turn.lisp")
(load "reproduce.lisp")
(load "eat-sick.lisp")
(load "typ-age-count-statistic.lisp")
(load "sex.lisp")
(load "fire.lisp")
(load "update-world.lisp")
(load "draw-world.lisp")
(load "move-spec.lisp")

(defun startup ()
(format t "~%type `info' to get info about the programm and the map itself")
(format t "~%type in a number of days you want to simulate or just press ENTER for 1 day")
(format t "~%type `quit' to exit")
(format t "~%type `source' to get the link to the source code.")
;; (format t "~%type `down-source' to get a copy of the source-code in you current directory (Only works in CLISP. wget required)") ;Removed 
(format t "~%type `lightning' to emulate a lightning-bolt hitting a plant to produce fire, and see the world burn.~%")
;;(fresh-line) (princ #\newline)
(evolution))


;;Creating some UserInterface
(defun evolution ()
  (statistic)
  (setf *tmp-eaten-plants* 0)
  (setf *tmp-eaten-animals* 0)
  (setf *tmp-animals-born* 0)
  (setf *tmp-logic-moves* 0)
  (draw-world)
  (fresh-line)
  (let ((str (read-line))) ;;Waits for user INPUT => if 'quit' = exit evolution.
    (cond ((equal str "quit") (format t "~%Good Bye")
;;                              (bye) ;; Uncomment this before building Because saying good-bye is always fine.
                              )
          ((equal str "info") (format t "~%This programm is licensed under the AGPL.")
                             ;; (print " An X shows an untyped animal (only on startup)") Not needed anymore, because first animals will be omnivores.
                              (format t "~%An P shows a herbivore.")
                              (format t "~%An M shows an omnivore.")
                              (format t "~%An F shows a carnivore.")
                              (format t "~%An * shows a plant.")
                              (format t "~%A # shows a fire burning.~%")
                              (evolution)
                              )
          ((equal str "source") (format t "~%Look at the source here: https://github.com/codepony/an_evolution.lisp~%")
                                (evolution))
          ((equal str "lightning")
           (if (> (hash-table-count *plants*) 0)
             (setfire)
             (format t "~%There are no plants to burn.~%")
             )
           (evolution)
           )
	  (t (let ((x (parse-integer str :junk-allowed t))) ;; Gets the integer out of str, let's you type in other chars after it. => :junk-allowed
		(if x
		    (loop for i
			  below x
			  do (update-world)
                          if (zerop (mod i 100));;x - Number of days. @ all 100 days inserts a dot.
			  do (princ #\.))
		    (update-world)) ;; if there is no INPUT (or no right one) => simulates only 1 day
		(evolution)))))) ;;recursive function
