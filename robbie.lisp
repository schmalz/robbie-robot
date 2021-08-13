(setf rooms '((living-room (north front-stairs)
			   (south dining-room)
			   (east kitchen))
	      (upstairs-bedroom (west library)
				(south front-stairs))
	      (dining-room (north living-room)
			   (east pantry)
			   (west downstairs-bedroom))
	      (kitchen (west living-room)
		       (south pantry))
	      (pantry (north kitchen)
		      (west dining-room))
	      (downstairs-bedroom (north back-stairs)
				  (east dining-room))
	      (back-stairs (south downstairs-bedroom)
			   (east dining-room))
	      (front-stairs (north upstairs-bedroom)
			    (south living-room))
	      (library (east upstairs-bedroom)
		       (south back-stairs))))

(setf loc 'pantry)

(defun choices (place)
  "The permissible directions from PLACE."
  (mapcar #'car
	  (cdr (assoc place rooms))))

(defun look (direction place)
  "The place that is in DIRECTION from PLACE."
  (cadr (assoc direction
	       (cdr (assoc place rooms)))))

(defun set-location (place)
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairs-p (place)
  (member place '(library upstairs-bedroom)))

(defun onstairs-p (place)
  (member place '(back-stairs front-stairs)))

(defun where ()
  (cond ((onstairs-p loc) `(robbie is on the ,loc))
	(t (let ((height (if (upstairs-p loc)
			     'upstairs
			   'downstairs)))
	     `(robbie is ,height in the ,loc)))))

(defun move (direction)
  (if (member direction
	      (choices loc))
      (progn (set-location (look direction loc))
	     (where))
    '(ouch! robbie hit a wall)))
