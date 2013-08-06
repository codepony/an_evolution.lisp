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


(defun reproduce (animal)
  "Manages the whole reproduction, have a look at the source to understand how it works exactly"
  (let ((e (animal-energy animal)) (reproduction-energy 80))
    (when (and (>= e reproduction-energy) (or (<= *counter* 5) (and (>= (animal-daysp animal) 5) (> (animal-cfact animal) 0))))
      ;; Looses half of its energy to its child
      (setf (animal-energy animal) (ash e -1))
      ;; Copy structure from parent to child !!!!WARNING!!! this will do so called 'Shallow Copy' (wrong copy)
      (let ((animal-nu (copy-structure animal))
            ;; Copies complete list => to get rid of these shallows: 
            (genes  (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        (let ((mutatwo (random 8)))
              (setf (nth mutatwo genes)
                    (max 1 (+ (nth mutatwo genes) (random 3) -1))))
        (setf (animal-genes animal-nu) genes)
        (setf (animal-age animal-nu) 0) 
        (setf (animal-typ animal-nu) (animal-ctyp animal-nu))
        ;; Randomly create Omnivores my 1/3 chance of getting one:
        (when (= (random 3) 2)
          (setf (animal-typ animal-nu) 'omnivore))
        ;; when there are many animals number of carnivores should raise: 
        (when (and (>= *counter* 10) (= (random 3) 2))
          (setf (animal-typ animal-nu) 'carnivore))
        (setf (animal-sex animal-nu) 'n)
        (setf (animal-preg animal-nu) 0)
        (setf (animal-cfact animal-nu) 0)
        (setf (animal-ctyp animal-nu) 'none)
        (setf (animal-daysp animal-nu) 0) 
        ;; NOT - setting (animal-sick animal) because should be the same as parent
        (when (> (animal-cfact animal) 0)
          ;; Added 50 because sexeff is mostly low:
          (setf (animal-energy animal-nu) (+ (round (* (animal-cfact animal) (animal-energy animal-nu))) 50 )))
        (setf (animal-preg animal) 0)
        (setf (animal-cfact animal) 0)
        (setf (animal-ctyp animal) 'none)
        ;; Resets the mother:
        (setf (animal-daysp animal) 0)
        (push animal-nu *animals*)
        (incf *animals-born*) (incf *tmp-animals-born*)))))

