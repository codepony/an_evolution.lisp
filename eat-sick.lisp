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


;; Food for omnivores and herbivores (or checking if there is a plant):
(defun eatp (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      ;; The older they get, the less energy they will get. It's that easy:
      (incf (animal-energy animal) (round (- *plant-energy* (* 0.1 (animal-age animal)))))
      (incf *tmp-eaten-plants*)
      (incf *plants-eaten*)
      ;; Chance of 1/5 to change the sick-status by eating plants:
      (if (= (random 5) 4)
        (setf (animal-sick animal) (not (animal-sick animal))))
      (remhash pos *plants*))))
;; No need to do this for carnivores


;; Let the carnivores eat  
;; Functions should be logical to understand
;; Same pos = eat it.
;; Age of the victim must be > 5 to be eaten. Otherwise Parents would kill their children.
;; Added genes here, because self-eating got a problem.
(defun eat-new (animal)
 (let ((success 0) (energy-loose 0) (damage 0) (sick-victim 0))
  (let ((posa (cons (animal-x animal) (animal-y animal)))(gena (animal-genes animal)) (agea (animal-age animal)) (energya (animal-energy animal)))
    (mapc (lambda (animal)
            (let ((posb (cons (animal-x animal) (animal-y animal))) (ageb (animal-age animal)) (genb (animal-genes animal)) (energyb (animal-energy animal)))
              (when (and (equal posa posb) (or (equal (animal-typ animal) 'omnivore) (equal (animal-typ animal) 'herbivore)) (> agea  5) (not (equal gena genb)) (oddp (+ (car gena) (car genb))))
                  (when (equal (animal-typ animal) 'herbivore)
                   (setf energy-loose (+ ageb (round (* 0.5 energya)) (round (* 0.2 energyb))))
                   (setf damage (+ agea (round (* 1/3 energyb)) (round (* 0.1 energya)))))
                  (when (equal (animal-typ animal) 'omnivore)
                   (setf energy-loose (+ ageb (round (* 0.75 energya)) (round (* 0.2 energyb))))
                   (setf damage (+ agea (round (* 2/3 energyb)) (round (* 0.1 energya)))))
                (when (>= energy-loose energyb)
                  (incf *tmp-eaten-animals*)
                  (incf *animals-eaten*)
                  (setf success 1)
                  (when (equal (animal-sick animal) T)
                    (setf sick-victim 1))
                  (setf (animal-energy animal) 0))
                (when (not (>= energy-loose energyb))
                  (setf success 0)
                  (decf (animal-energy animal) energy-loose))))) 
          *animals*))
  (decf (animal-energy animal) damage)
  (when (= success 1)
    ;; Same as we do with the plants. higher age => less energy:
    (incf (animal-energy animal) (round (- *animal-energy* (* 0.1 (animal-age animal)))))
    (when (= sick-victim 1)
      (setf (animal-sick animal) T)))))


;; Testing for illness of animals | Version 1.2.8-2
(defun issick (animal)
  (when (equal (animal-sick animal) T)
    (setf (animal-energy animal) (round (* (animal-energy animal) 0.95)))))

