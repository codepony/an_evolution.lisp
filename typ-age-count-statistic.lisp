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


(defun older (animal)
  "Manages the age of animals"
  (incf (animal-age animal))
  (decf (animal-energy animal))
  ;; Getting an energy-boot once in the life-time:
  (if (equal (animal-age animal) 75)
    (incf (animal-energy animal) 150)))


(defun typ (animal)
  "Manages the types of animals - Carnivores, herbivores and omnivores"
  (let ((typ (animal-typ animal)))
    (when (equal typ 'none)
      (let ((typset (random 3)))
        (when (= typset 0)
          (setf (animal-typ animal) 'omnivore))
        (when (= typset 1)
          (setf (animal-typ animal) 'herbivore))
        (when (= typset 2)
          (setf (animal-typ animal) 'carnivore))))))


(defvar *counter* 1)
(defun cnt (animal)
  "Count alive animals"
  (incf *counter*))


(defun statistic ()
  "Calculates and prints the statistics"
  (format t "~%Number of animals alive: ~a" *counter*)
  (format t "~%Number of eaten plants this round: ~a" *tmp-eaten-plants*)
  (format t "~%Number of eaten plants: ~a" *plants-eaten*)
  (format t "~%Number of eaten animals this round: ~a" *tmp-eaten-animals*)
  (format t "~%Number of eaten animals: ~a" *animals-eaten*)
  (format t "~%Number of born animals this round: ~a" *tmp-animals-born*)
  (format t "~%Number of born animals: ~a" *animals-born*)
  (format t "~%Number of logic-moves this round: ~a" *tmp-logic-moves*)
  (format t "~%Number of logic-moves: ~a" *logic-moves*)
  (format t "~%~%"))

