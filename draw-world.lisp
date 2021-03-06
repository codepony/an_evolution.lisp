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


(defun draw-world ()
  "Draws the world as output, with coordinates fetched for each object."
  (loop for y below (+ *height* 1)
        do (progn (fresh-line)
                  ;; Marks left side of the world:
                  (princ "|")
                  (loop for x below (+ *width* 1)
                        do (princ (let ((animal-found nil) (print-type nil))
                                    ;; Checking for different animals in a very special way:
                                    (mapcar (lambda (animal)
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
                                    ;; Printing the symbols to the map:
                                    (if (not animal-found) 
                                      (cond ((some (lambda (fire)
                                                     (and (= (fire-x fire) x)
                                                          (= (fire-y fire) y)))
                                                   *fires*)
                                             #\#)
                                            ;; Fire, plants, "nothing":
                                            ((gethash (cons x y) *plants*) #\*)
                                            ;; In the case there are no animals:
                                            (t #\space))
                                      ;; Else, print the animal:
                                      print-type))))
                  ;; Marks right side of the world:
                  (princ "|"))))

