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


(defun givesex (animal)
   "Giving a sex to the animals based on the sum of genes decides the sex of the animal"
  (if (oddp (apply '+ (animal-genes animal)))
    (setf (animal-sex animal) 'f)
    (setf (animal-sex animal) 'm)))


(defun havesex (animal)
  "Same types will reproduce of course
   When there is any omnivore, the child-typ will be random.
   Carnivores & herbivores can't have sex."
  (let ((child-typ 'none) (success nil)) 
  (let ((pos (cons (animal-x animal) (animal-y animal))) (gena (animal-genes animal)) (agea (animal-age animal)) (typa (animal-typ animal)) (sexa (animal-sex animal)))
      (mapc (lambda (animal)
              (let ((posa (cons (animal-x animal) (animal-y animal))) (ageb (animal-age animal)) (genb (animal-genes animal)) (typb (animal-typ animal)) (sexb (animal-sex animal)))
                (when (and (not (equal sexa sexb))(equal pos posa) (> agea  5) (> ageb 5) (not (equal gena genb)) (evenp (+ (car gena) (car genb))))
                  (when (and (not (equal typa 'omnivore)) (equal typa typb) (not (equal typb 'omnivore)))
                    (setf child-typ typa)
                    (setf success t)
                    (sexeff gena genb))
                  (when (or (and (equal typa 'carnivore) (equal typb 'herbivore)) (and (equal typa 'herbivore) (equal typb 'carnivore)))
                    (setf success nil))
                  (when (or (equal typa 'omnivore) (equal typb 'omnivore))
                    (setf child-typ 'none)
                    (setf success t)
                    (sexeff gena genb))
                  (when (and success (equal sexb 'f))
                    (setf (animal-ctyp animal) child-typ)
                    (setf (animal-preg animal) 1)
                    (setf (animal-cfact animal) *efficiency*)
                    (setf *efficiency* 0)))))
            *animals*))
  (when (and success (equal (animal-sex animal) 'f))
    (dbg :ael "Sex was successful and a new ~a will be born soon.~%" child-typ)
    (setf (animal-ctyp animal) child-typ)
    (setf (animal-cfact animal) *efficiency*)
    (setf *efficiency* 0)
    (setf (animal-preg animal) 1))))


(defparameter *efficiency* 0)
(defun sexeff (gena genb)
  "Let the genes tell how efficient sex is"
  (let ((gendiff 0))
    (loop for i below (length gena) do 
          (when (not (equal (nth i gena) (nth i genb)))
            (incf gendiff))
          (setf *efficiency* (/ (1+ gendiff) (1+ (length gena)))))))

