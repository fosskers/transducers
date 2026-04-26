(defpackage transducers/fset
  (:use :cl)
  (:shadow #:set #:map)
  (:local-nicknames (#:t #:transducers)
                    (#:s #:fset2))
  (:export #:set #:map #:seq #:bag)
  (:documentation "Fset extensions for Transducers."))

(in-package :transducers/fset)

;; --- Transducers --- ;;

(defmethod t:transduce (xform f (source s:set))
  (set-transduce xform f source))

(defmethod t:transduce (xform f (source s:map))
  (map-transduce xform f source))

(defmethod t:transduce (xform f (source s:seq))
  (seq-transduce xform f source))

(defmethod t:transduce (xform f (source s:bag))
  (bag-transduce xform f source))

(defun set-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (set-reduce xf init coll)))
    (funcall xf result)))

(defun set-reduce (f identity set)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((iter (s:iterator set)))
    (labels ((recurse (acc)
               (if (funcall iter :done?)
                   acc
                   (let ((acc (funcall f acc (funcall iter :get))))
                     (if (t:reduced? acc)
                         (t:reduced-val acc)
                         (recurse acc))))))
      (recurse identity))))

#+nil
(t:transduce #'t:pass #'t:cons (s:set 1 2 3 1))

(defun map-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (map-reduce xf init coll)))
    (funcall xf result)))

(defun map-reduce (f identity set)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((iter (s:iterator set)))
    (labels ((recurse (acc)
               (if (funcall iter :done?)
                   acc
                   (multiple-value-bind (key val) (funcall iter :get)
                     (let ((acc (funcall f acc (cl:cons key val))))
                       (if (t:reduced? acc)
                           (t:reduced-val acc)
                           (recurse acc)))))))
      (recurse identity))))

#+nil
(t:transduce (t:map #'car) #'t:cons (s:map (:a 1) (:b 2) (:c 3)))

(defun seq-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (seq-reduce xf init coll)))
    (funcall xf result)))

(defun seq-reduce (f identity set)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((iter (s:iterator set)))
    (labels ((recurse (acc)
               (if (funcall iter :done?)
                   acc
                   (let ((acc (funcall f acc (funcall iter :get))))
                     (if (t:reduced? acc)
                         (t:reduced-val acc)
                         (recurse acc))))))
      (recurse identity))))

#+nil
(t:transduce #'t:pass #'t:cons (s:seq 1 2 3 1))

(defun bag-transduce (xform f coll)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (bag-reduce xf init coll)))
    (funcall xf result)))

(defun bag-reduce (f identity set)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((iter (s:iterator set)))
    (labels ((recurse (acc)
               (if (funcall iter :done?)
                   acc
                   (let ((acc (funcall f acc (funcall iter :get))))
                     (if (t:reduced? acc)
                         (t:reduced-val acc)
                         (recurse acc))))))
      (recurse identity))))

#+nil
(t:transduce #'t:pass #'t:cons (s:bag 1 2 3 1))

;; --- Reducers --- ;;

(defun set (&optional (acc (s:empty-set) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset set."
  (cond ((and a-p i-p) (s:with acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-set))))

#+nil
(t:transduce #'t:pass #'set '(1 2 3 1))

(defun map (&optional (acc (s:empty-map) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset map."
  (cond ((and a-p i-p) (destructuring-bind (key . val) input
                         (s:with acc key val)))
        ((and a-p (not i-p)) acc)
        (t (s:empty-map))))

#+nil
(t:transduce #'t:enumerate #'map "hello")

(defun seq (&optional (acc (s:empty-seq) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset seq."
  (cond ((and a-p i-p) (s:with-last acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-seq))))

#+nil
(t:transduce #'t:pass #'seq '(1 2 3 1))

(defun bag (&optional (acc (s:empty-bag) a-p) (input nil i-p))
  "Reducer: Collect all results as an immutable Fset bag."
  (cond ((and a-p i-p) (s:with acc input))
        ((and a-p (not i-p)) acc)
        (t (s:empty-bag))))

#+nil
(t:transduce #'t:pass #'bag '(1 2 1 3 1))
