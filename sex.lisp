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

;; Giving a sex to the animals | Version 1.2.1
(defun givesex (animal)
  (if (oddp (apply '+ (animal-genes animal))) ;; The sum of genes decides the sex of the animal.
    (setf (animal-sex animal) 'f)
    (setf (animal-sex animal) 'm)))


;; Having sex here | Version 1.2.2
;; Same types will reproduce of cource
;; When there is any omnivore, the childi-typ will be randomed.
;; Carnivores & herbivores can't have sex.
(defun havesex (animal)
  (let ((child-typ 'none) (success 0)) 
  (let ((pos (cons (animal-x animal) (animal-y animal))) (gena (animal-genes animal)) (agea (animal-age animal)) (typa (animal-typ animal)) (sexa (animal-sex animal)))
      (mapc (lambda (animal)
              (let ((posa (cons (animal-x animal) (animal-y animal))) (ageb (animal-age animal)) (genb (animal-genes animal)) (typb (animal-typ animal)) (sexb (animal-sex animal)))
                (when (and (not (equal sexa sexb))(equal pos posa) (> agea  5) (> ageb 5) (not (equal gena genb)) (evenp (+ (car gena) (car genb))))
                  (when (and (not (equal typa 'omnivore)) (equal typa typb) (not (equal typb 'omnivore)))
                    ;;(prin1 "Having sex.")
                    (setf child-typ typa)
                    (setf success 1)
                    (sexeff gena genb)
                    )
                  (when (or (and (equal typa 'carnivore) (equal typb 'herbivore)) (and (equal typa 'herbivore) (equal typb 'carnivore)))
                    ;;(prin1 "no sex happend.")
                    (setf success 0))
                  (when (or (equal typa 'omnivore) (equal typb 'omnivore))
                    ;;(prin1 "Having sex.")
                    (setf child-typ 'none)
                    (setf success 1)
                    (sexeff gena genb))
                  (when (and (= success 1)(equal sexb 'f))
                    (setf (animal-ctyp animal) child-typ)
                    (setf (animal-preg animal) 1)
                    (setf (animal-cfact animal) *efficiency*)
                    (setf *efficiency* 0))
      ))) *animals*))
  (when (and (= success 1) (equal (animal-sex animal) 'f))
    (setf (animal-ctyp animal) child-typ)
    (setf (animal-cfact animal) *efficiency*)
    (setf *efficiency* 0)
    (setf (animal-preg animal) 1))
 (fresh-line)))


;; Let the genes tell how efficient sex is! | Version 1.2.6
(defparameter *efficiency* 0)
(defun sexeff (gena genb)
  (let ((gendiff 0))
    (loop for i below (length gena)
          do 
              (when(not (equal (nth i gena) (nth i genb)))
                (incf gendiff)
                )
    (setf *efficiency* (/ (1+ gendiff) (1+ (length gena))))
   ;; (princ *efficiency*) ;; Debug-line
    (fresh-line))))
