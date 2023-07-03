(in-package #:org.shirakumo.farey-numbers)

(defun generate-farey-grid (n)
  (sort (coerce (remove-duplicates
                 (list* 0
                        (loop for j from 1 to n
                              append (loop for i from 1 to (- j 1)
                                           collect (/ i j)))))
                'vector)
        #'<))

(defvar *farey-grid* (generate-farey-grid 10))

(defun new-farey-grid (n)
  (setf *farey-grid* (generate-farey-grid n)))

(defun fareyref (p)
  (aref *farey-grid* p))

(defstruct (farey (:conc-name %)
                  (:constructor make-farey (multiplier grid-index)))
  (multiplier 0 :type integer)
  (grid-index 0 :type integer))

(defmethod print-object ((farey farey) stream)
  (print-unreadable-object (farey stream :type T)
    (format stream "~s ~s" (multiplier farey) (fraction farey))))

(defmethod make-load-form ((farey farey) &optional env)
  (declare (ignore env))
  `(make-farey ,(multiplier farey) ,(grid-index farey)))

(defun snap-to-farey-grid (x)
  (multiple-value-bind (multiplier residual) (floor x)
    (loop for grid-index from 1 below (length *farey-grid*)
          do (when (<= residual (fareyref grid-index))
               (return (if (< (- (fareyref grid-index) residual)
                              (- residual (fareyref (1- grid-index))))
                           (values multiplier grid-index)
                           (values multiplier (1- grid-index)))))
          finally (return (values multiplier (1- grid-index))))))

(defun ensure-farey (x)
  (etypecase x
    (farey x)
    (real
     (multiple-value-bind (multiplier grid-index) (snap-to-farey-grid x)
       (make-farey multiplier grid-index)))))

(defun multiplier (farey)
  (%multiplier (ensure-farey farey)))

(defun (setf multiplier) (multiplier farey)
  (setf (%multiplier farey) multiplier))

(defun grid-index (farey)
  (%grid-index (ensure-farey farey)))

(defun (setf grid-index) (grid-index farey)
  (multiple-value-bind (multiplier grid-index) (floor grid-index (length *farey-grid*))
    (setf (%grid-index farey) grid-index)
    (incf (%multiplier farey) multiplier)
    grid-index))

(defun fraction (farey)
  (fareyref (%grid-index (ensure-farey farey))))

(defun (setf fraction) (number farey)
  (let ((grid-index (position number *farey-grid*)))
    (unless grid-index
      (error "~s is not a valid fraction as it could not be found in the grid."
             number))
    (setf (%grid-index farey) grid-index)))

(defun farey->number (farey)
  (+ (multiplier farey) (fraction farey)))

(defun combine (a b operation)
  (let ((a (ensure-farey a))
        (b (ensure-farey b)))
    (multiple-value-bind (multiplier grid-index)
        (snap-to-farey-grid (funcall operation (farey->number a) (farey->number b)))
      (make-farey multiplier grid-index))))

(defun f+ (&rest args)
  (cond ((cddr args)
         (reduce #'f+ args))
        ((cdr args)
         (combine (first args) (second args) '+))
        ((car args)
         (ensure-farey (first args)))
        (T
         (ensure-farey 0))))

(defun f* (&rest args)
  (cond ((cddr args)
         (reduce #'f* args))
        ((cdr args)
         (combine (first args) (second args) '*))
        ((car args)
         (ensure-farey (first args)))
        (T
         (ensure-farey 1))))

(defun f- (a &rest args)
  (cond ((cdr args)
         (reduce #'f- args))
        ((car args)
         (combine a (first args) '-))
        (T
         (make-farey (1- (multiplier a))
                     (- (length *farey-grid*)
                        (grid-index a) 1)))))

(defun f/ (a &rest args)
  (cond ((cdr args)
         (reduce #'f- args))
        ((car args)
         (combine a (first args) '/))
        (T
         (f/ 1 a))))

(defun fshift (a spaces)
  (let ((a (copy-farey (ensure-farey a))))
    (incf (grid-index a) spaces)
    a))
