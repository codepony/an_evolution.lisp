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


;; define world:
(defparameter *width-org* (- 45 20)) 
(defparameter *height-org* (- 15 5)) 

;; Resetting this because of the dynamic map-size:
(defparameter *width* *width-org*) 
(defparameter *height* *height-org*) 
 
;; Coordinates of the Jungle (Middle of the map) '(x y width height):
(defparameter *jungle* '(22 5 10 10))
;; One plant gives an animal 80 days of energy: 
(defparameter *plant-energy* 80) 

;; Global Statistics:
(defparameter *animals-eaten* 0)
(defparameter *plants-eaten* 0)
;; Because 1st animal won't be counted else:
(defparameter *animals-born* 1)
(defparameter *tmp-animals-born* 0)
(defparameter *tmp-eaten-plants* 0)
(defparameter *tmp-eaten-animals* 0)
(defparameter *tmp-logic-moves* 0)
(defparameter *logic-moves* 0)

;; Growing plants:
(defparameter *plants* (make-hash-table :test #'equal))


(defun random-plant (left top width height)
  ;; Random Coordinates of the new plant:
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    ;; Shows the plant in the hash-table:
    (setf (gethash pos *plants*) t)))


;; Every day this function creates 2 plants => one in the jungle and one out:
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
;; The jungle will be higher populated as the rest of the world because it's smaller.


;; Define the fire here | Version 1.2.8-1
(defstruct fire x y lifet)
;; empty list, because no fire on startup:
(defparameter *fires* (list))

;; One animal gives an animal 100 days of energy:
(defparameter *animal-energy* 100)


;; Creating animals:
(defstruct animal x y energy dir typ sex preg daysp ctyp cfact age sick genes)

(defparameter *animals*
  ;; Get Coordinates of the animal here:
  (list (make-animal
        :x      (ash *width* -1)
        :y      (ash *height* -1)
        :energy 900
        :dir    0
        :typ    'omnivore
        :sex    'n
        ;; First animal is pregnant to reproduce on startup:
        :preg   1
        ;; Days pregnant: 
        :daysp  0 
        ;; child-typ: 
        :ctyp   'omnivore 
        ;; For first run it's 100% - energy-factor based on genes cal. in (sexeff): 
        :cfact  1 
        :age    0
        :sick   nil
        :genes  (loop repeat 8
                  collecting (1+ (random 10)))
        )))

