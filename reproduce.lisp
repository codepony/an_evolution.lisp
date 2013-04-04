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

;;Reproduction of animals
(defun reproduce (animal)
  (let ((e (animal-energy animal)) (reproduction-energy 80))
    (when (and (>= e reproduction-energy) (or (<= *counter* 5) (and (>= (animal-daysp animal) 5) (> (animal-cfact animal) 0)))) 
      (setf (animal-energy animal) (ash e -1)) ;;Looses half of its energy to its child
      (let ((animal-nu (copy-structure animal)) ;; Copy structure from parent to child !!!!WARNING!!! this will do so called 'Shallow Copy' (wrong copy)
	(genes	(copy-list (animal-genes animal))) ;;Copies complete list => to get rid of these shallows
	(mutation (random 8)))
    ;;(princ genes)
    (setf (nth mutation genes)
	  (max 1 (+ (nth mutation genes) (random 3) -1)))
    ;;(princ mutation)
    ;;(princ genes)
    (let ((mutatwo (random 8)))
          (setf (nth mutatwo genes)
                (max 1 (+ (nth mutatwo genes) (random 3) -1))))
    ;;(princ genes)
    (setf (animal-genes animal-nu) genes)
    (setf (animal-age animal-nu) 0) ;; Would be stupid to give a child the same age as the parent.
    (setf (animal-typ animal-nu) (animal-ctyp animal-nu))
    (when (= (random 3) 2) ;; Randomly create Omnivores my 1/3 chance of getting one 
      (setf (animal-typ animal-nu) 'omnivore)
      )
    (when (and (>= *counter* 10) (= (random 3) 2)) ;; when there are many animals number of carnivores should raise
      (setf (animal-typ animal-nu) 'carnivore) ;; Logic evolution-function.
      )
    (setf (animal-sex animal-nu) 'n)
    (setf (animal-preg animal-nu) 0)
    (setf (animal-cfact animal-nu) 0)
    (setf (animal-ctyp animal-nu) 'none)
    (setf (animal-daysp animal-nu) 0);; NOT - setting (animal-sick animal) because should be the same as parent 
    (when (> (animal-cfact animal) 0)
      (setf (animal-energy animal-nu) (+ (round (* (animal-cfact animal) (animal-energy animal-nu))) 50 )) ;; Added 50 because sexeff is mostly low.
      ;; (princ (animal-energy animal-nu))(fresh-line)
      ;; (princ (animal-energy animal))
      ;; (fresh-line)
    )
    (setf (animal-preg animal) 0)
    (setf (animal-cfact animal) 0)
    (setf (animal-ctyp animal) 'none)
    (setf (animal-daysp animal) 0) ;; Resets the mother.
    ;;(princ "animal was born")
    (fresh-line)
    (push animal-nu *animals*)
    (incf *animals-born*) (incf *tmp-animals-born*)))))
