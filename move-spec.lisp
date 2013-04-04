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

;; Logical thinking for moving | Version 1.3.2
;; x == width | y == height (just to remember)
;; Plants are saved like (x . y)

(defparameter *isfire* (make-array 8))
(defparameter *isplant* (make-array 8))
(defparameter *isanimal* (make-array 8))

(defun move-logic (animal)
  (let ((ismoved nil) (posx (animal-x animal)) (posy (animal-y animal))) ;; Coordinates we need to find direct neighbours
    (lookaround posx posy)
    
    (let ((firelist (list)) (animallist (list)) (plantlist (list))) ;; Makes 3 lists with pos, where something is.
      
     (loop for x from 0 to 7 do
           (if (not (equal (aref *isfire* x) nil))
             (setf firelist (cons firelist (cons (aref *isfire* x) ())))
             )
      ) 
     (setf firelist (cdr firelist)) ;; because otherwise it would be (NIL (x y)..)
     
     (loop for x from 0 to 7 do
           (if (not (equal (aref *isplant* x) nil))
             (setf plantlist (cons plantlist (cons (aref *isplant* x) ())))
             )
      ) 
     (setf plantlist (cdr plantlist))
     
     (loop for x from 0 to 7 do
           (if (not (equal (aref *isanimal* x) nil))
             (setf animallist (cons animallist (cons (aref *isanimal* x) ())))
             )
      ) 
     (setf animallist (cdr animallist))
;;   (format t "animals: ~a plants: ~a fires: ~a ~%" animallist plantlist firelist)
     
     (cond 
       ((> (length firelist) 0)
        (let ((workwith (nth (random  (length firelist)) firelist))) 
;;          (format t "testing fire with ~a ~%" workwith)
          (if (> (car workwith) posx)
            (setf (animal-x animal) (- (animal-x animal) 1))
            )
          (if (< (car workwith) posx)
            (setf (animal-x animal) (+ (animal-x animal) 1))
            )
          (if (< (cadr workwith) posy)
            (setf (animal-y animal) (+ (animal-y animal) 1))
            )
          (if (> (cadr workwith) posy)
            (setf (animal-y animal) (- (animal-y animal) 1))
            )
          ) (setf ismoved t)(incf *tmp-logic-moves*) (incf *logic-moves*))
      
       ((and (> (length plantlist) 0) (or (equal (animal-typ animal) 'herbivore) (equal (animal-typ animal) 'omnivore)) (equal ismoved nil))
        (let ((workwith (nth (random (length plantlist)) plantlist)))
;;          (format t "testing plants with ~a ~%" workwith)
          (setf (animal-x animal) (car workwith))
          (setf (animal-y animal) (cadr workwith))
          ) (setf ismoved t) (incf *tmp-logic-moves*) (incf *logic-moves*))
       
       ((and (> (length animallist) 0) (equal ismoved nil))
        (let ((workwith (nth (random (length animallist)) animallist)))
;;          (format t "~a testing animals with ~a  length= ~a ~%" animallist workwith (length animallist))
         
          (if (and (equal (animal-typ animal) "herbivore") (not (equal (caddr workwith) "herbivore")))
           (progn
           (if (> (car workwith) posx)
             (setf (animal-x animal) (- (animal-x animal) 1))
             )
           (if (< (car workwith) posx)
             (setf (animal-x animal) (+ (animal-x animal) 1))
             )
           (if (< (cadr workwith) posy)
             (setf (animal-y animal) (+ (animal-y animal) 1))
             )
           (if (> (cadr workwith) posy)
             (setf (animal-y animal) (- (animal-y animal) 1))
             )
           (setf ismoved t)(incf *tmp-logic-moves*) (incf *logic-moves*)
           )
           (if (not (equal (animal-typ animal) (caddr workwith)))
             (progn
               (setf (animal-x animal) (car workwith))
               (setf (animal-y animal) (cadr workwith))
               (setf ismoved t)(incf *tmp-logic-moves*) (incf *logic-moves*)
             )
             (progn
;;               (format t "We got this case!~%")
               (move animal)
               )
             ))
        ))
       
       ((equal ismoved nil)
        (move animal)
        )))))

;; Simple function that calls checkfortarget(x y) with any coordinate around an animal.
(defun lookaround (x y)
;;  (format t "We got here with ~a and ~a ~%" x y)
  (let ((xlook (+ x 1)) (ylook y) (check 0))
    (checkfortarget xlook ylook check) 
   (when (> xlook *width*)
     (setf xlook 0) 
     (checkfortarget xlook ylook check)))
 
  (let ((xlook (+ x 1)) (ylook (+ y 1)) (check 1))
    (checkfortarget xlook ylook check)
   (when (> xlook *width*)
     (setf xlook 0)
     (checkfortarget xlook ylook check)) 
   (when (> ylook *height*)
     (setf ylook 0) 
     (checkfortarget xlook ylook check)))
 
  (let ((xlook x) (ylook (+ y 1)) (check 2))
    (checkfortarget xlook ylook check) 
   (when (> ylook *height*)
     (setf ylook 0) 
     (checkfortarget xlook ylook check)))
 
  (let ((xlook (- x 1)) (ylook (+ y 1)) (check 3))
    (checkfortarget xlook ylook check) 
   (when (< xlook 0)
     (setf xlook *width*)
     (checkfortarget xlook ylook check)) 
   (when (> ylook *height*)
     (setf ylook 0) 
     (checkfortarget xlook ylook check)))
 
  (let ((xlook (- x 1)) (ylook y) (check 4))
    (checkfortarget xlook ylook check) 
   (when (< xlook 0)
     (setf xlook *width*)
     (checkfortarget xlook ylook check))) 
 
  (let ((xlook (- x 1)) (ylook (- y 1)) (check 5))
    (checkfortarget xlook ylook check) 
   (when (< xlook 0)
     (setf xlook *width*)
     (checkfortarget xlook ylook check)) 
   (when (< ylook 0)
     (setf ylook *height*)
     (checkfortarget xlook ylook check)))
 
  (let ((xlook (+ x 1)) (ylook (- y 1)) (check 6))
    (checkfortarget xlook ylook check)
   (when (> xlook *width*)
     (setf xlook 0) 
     (checkfortarget xlook ylook check))
   (when (< ylook 0)
     (setf ylook *height*)
     (checkfortarget xlook ylook check))) 
  
 (let ((xlook x) (ylook (- y 1)) (check 7))
   (checkfortarget xlook ylook check)
  (when (< ylook 0)
    (setf ylook *height*)
    (checkfortarget xlook ylook check))) 
  )

;; Function to check for targets around animals
(defun checkfortarget (xlook ylook check)
;;  (format t "We got here with ~a and ~a case: ~a ~%" xlook ylook check)
  (if (gethash (cons xlook ylook) *plants*)
    (setf (aref *isplant* check) (cons xlook (cons ylook ())))
    (setf (aref *isplant* check) nil)
    )
;;  (format t "isplant: ~a ~%" *isplant*)
  
  (let ((ischecked nil))
    (mapc (lambda (animal)
            (if (and (= (animal-x animal) xlook) (= (animal-y animal) ylook))
              (progn
                ;;(princ "was here")
                (setf (aref *isanimal* check) (cons xlook (cons ylook (cons (animal-typ animal) ()))))
                (setf ischecked check)
                ;;(princ ischecked)
                ))) *animals*)
    (if (equal ischecked nil)
      (setf (aref *isanimal* check) nil)
      ))
  
  (let ((ischecked nil))
    (mapc (lambda (fire)
            (if (and (= (fire-x fire) xlook) (= (fire-y fire) ylook))
              (progn
                (setf (aref *isfire* check) (cons xlook (cons ylook ())))
                (setf ischecked check)
                ))) *fires*)
    (if (equal ischecked nil)
      (setf (aref *isfire* check) nil)
      )))

