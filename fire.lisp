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


;; Let fire happen | Version 1.2.8-1
(defun setfire ()
  (format t "~%A lightning hits a plant~%")
  ;; Loops are great:
  (let ((key-list (loop for key being the hash-keys of *plants* collect key)))
    ;; getting a random plant:
    (let ((choosen-plant (nth (random (hash-table-count *plants*)) key-list)))
      (let ((xfire (car choosen-plant)) (yfire (cdr choosen-plant)))
        (remhash choosen-plant *plants*)
        (let ((fire-nu (make-fire :x xfire :y yfire :lifet 0)))
          (push fire-nu *fires*))))))


;; Spreading the fire on other plants | Version 1.2.8-1
;; It's still not perfect - sorry but I really hope it works.
(defun spreadfire (fire)
 ;; Test every field around the fire if there is a plant. ( 8 - cases):
 (let ((xfire (+ (fire-x fire) 1)) (yfire (fire-y fire)))
   (testfire xfire yfire) 
  (when (> xfire *width*)
    (setf xfire 0) 
    (testfire xfire yfire)))
 (let ((xfire (+ (fire-x fire) 1)) (yfire (+ (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (> xfire *width*)
    (setf xfire 0)
    (testfire xfire yfire)) 
  (when (> yfire *height*)
    (setf yfire 0) 
    (testfire xfire yfire)))
 (let ((xfire (fire-x fire)) (yfire (+ (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (> yfire *height*)
    (setf yfire 0) 
    (testfire xfire yfire)))
 (let ((xfire (- (fire-x fire) 1)) (yfire (+ (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (< xfire 0)
    (setf xfire *width*)
    (testfire xfire yfire)) 
  (when (> yfire *height*)
    (setf yfire 0) 
    (testfire xfire yfire)))
 (let ((xfire (- (fire-x fire) 1)) (yfire (fire-y fire)))
   (testfire xfire yfire) 
  (when (< xfire 0)
    (setf xfire *width*)
    (testfire xfire yfire))) 
 (let ((xfire (- (fire-x fire) 1)) (yfire (- (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (< xfire 0)
    (setf xfire *width*)
    (testfire xfire yfire)) 
  (when (< yfire 0)
    (setf yfire *height*)
    (testfire xfire yfire)))
 (let ((xfire (+ (fire-x fire) 1)) (yfire (- (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (> xfire *width*)
    (setf xfire 0) 
    (testfire xfire yfire)) 
  (when (< yfire 0)
    (setf yfire *height*)
    (testfire xfire yfire)))
 (let ((xfire (fire-x fire)) (yfire (- (fire-y fire) 1)))
   (testfire xfire yfire) 
  (when (< yfire 0)
    (setf yfire *height*)
    (testfire xfire yfire))))


;; Testing if fire spreads | Version 1.2.8-1
(defun testfire (xfire yfire)
  (when (gethash (cons xfire yfire) *plants*)
    (let ((newfire (make-fire :x xfire :y yfire :lifet 0)))
      (push newfire *fires*)
      (remhash (cons xfire yfire) *plants*))))


;; Killing an animal, if it touches the fire | Version 1.2.8-1
(defun burn (animal)
  (mapc (lambda (fire)
          (when (and (equal (animal-x animal) (fire-x fire)) (equal (animal-y animal) (fire-y fire)))
            (setf (animal-energy animal) 0)))
        *fires*))

