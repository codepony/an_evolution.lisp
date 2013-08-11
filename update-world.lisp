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


(defun update-world ()
  " Simulates a day in our world"
  ;; Removes dead animals from world:
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  ;; Removes fire, if it's out burned (after 10 days):
  (setf *fires* (remove-if (lambda (fire)
                             (>= (fire-lifet fire) 10))
                           *fires*))
  (setf *counter* 0)
  ;; Ugly fix because of cnt animal:
  (mapc (lambda (animal)
          (cnt)
          ) *animals*)
  ;; When all animals got "removed", the player looses the game:
  (when (<= *counter* 0)
    (format t "The Game Is Over ~% AUTOMATIC RESET")
    (exit))
  (resizemap)
  (mapc (lambda (animal)
          (turn animal)
          (move-logic animal)
          ;; Give parents & children 5 days to live together and/or have the time to run away:
          ;; At the age of 200 days, an animal won't be able to kill others anymore:
          (when (and (> (animal-age animal) 5) (or (equal (animal-typ animal) 'omnivore) (equal (animal-typ animal) 'carnivore)) (< (animal-age animal) 200)) 
            (eat-new animal))
          (when (or (equal (animal-typ animal) 'omnivore) (equal (animal-typ animal) 'herbivore))
            (eatp animal))
           (when (and (> (animal-age animal) 5) (equal (animal-sex animal) 'n))
            (givesex animal))
           (when (> (animal-age animal) 5) 
            (havesex animal))
           (when (= (animal-preg animal)1)
            (incf (animal-daysp animal)))
           (reproduce animal)
           (older animal)
           (typ animal)
           (burn animal)
           (issick animal))
        *animals*)
  (mapc (lambda (fire)
          (spreadfire fire)
          (incf (fire-lifet fire)))
        *fires*)
  (add-plants))


(defun resizemap ()
  "Making a simple dynamic map size"
  (setf *width* (+ *width-org* (round (/ *counter* 10))))
  (setf *height* (+ *height-org* (round (/ *counter* 30)))))

