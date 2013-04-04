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

;;Create our world
(defun draw-world ()
  (loop for y below (+ *height* 1)
        do (progn (fresh-line)
                  (princ "|") ; Marks left side of the world
                  (loop for x below (+ *width* 1)
                        do (princ (let ((animal-found nil) (print-type nil))
                                    (mapcar (lambda (animal) ;Checking for different animals in a very special way
                                              (when (and (= (animal-x animal) x)
                                                         (= (animal-y animal) y))
                                                (setf animal-found t)
                                                (setf print-type 
                                                      (case (animal-typ animal)
                                                        ((omnivore)  #\M)
                                                        ((herbivore) #\P)
                                                        ((carnivore) #\F)
                                                        ((none) #\X)
                                                        )))) *animals*)
                                    (if (not animal-found) ;Printing the symbols to the map
                                      (cond ((some (lambda (fire)
                                                     (and (= (fire-x fire) x)
                                                          (= (fire-y fire) y)))
                                                   *fires*)
                                             #\#)
                                            ((gethash (cons x y) *plants*) #\*) ;Fire, plants, "nothing" 
                                            (t #\space)) ;In the case there are no animals.
                                      print-type)))) ;Else: print the animal
                  (princ "|")))) ;Marks right side of the world
