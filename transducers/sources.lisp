(in-package :transducers)

(defstruct (generator (:copier nil) (:predicate nil))
  "A wrapper around a function that can potentially yield endless values."
  (func nil :read-only t :type (function () *)))

(defvar *done* 'done
  "A value to signal the end of an unfolding process.")

(defstruct (plist (:copier nil) (:predicate nil))
  (list nil :read-only t :type list))

(declaim (ftype (function (list) plist) plist))
(defun plist (plist)
  "Source: Yield key-value pairs from a Property List, usually known as a 'plist'.
The pairs are passed as a cons cell."
  (make-plist :list plist))

(defstruct (reversed (:copier nil) (:predicate nil))
  (vector #() :read-only t :type cl:vector))

(declaim (ftype (function (cl:vector) reversed) reversed))
(defun reversed (vector)
  "Source: Yield a VECTOR's elements in reverse order."
  (make-reversed :vector vector))

;; FIXME: type signature, expecting `values' to be called within the given
;; function.
(declaim (ftype (function ((function (t) *) t) generator) unfold))
(defun unfold (f seed)
  (let* ((curr seed)
         (func (lambda ()
                 (multiple-value-bind (acc next) (funcall f curr)
                   (cond ((eq *done* next) *done*)
                         (t (setf curr acc)
                            next))))))
    (make-generator :func func)))

(declaim (ftype (function (t) generator) repeat))
(defun repeat (item)
  "Source: Endlessly yield a given ITEM."
  (make-generator :func (constantly item)))

#+nil
(transduce (take 4) #'cons (repeat 9))

(declaim (ftype (function (integer &key (:step fixnum)) generator) ints))
(defun ints (start &key (step 1))
  "Source: Yield all integers, beginning with START and advancing by an optional
STEP value which can be positive or negative. If you only want a specific range
within the transduction, then use `take-while' within your transducer chain."
  (let* ((curr start)
         (func (lambda ()
                 (let ((old curr))
                   (setf curr (+ curr step))
                   old))))
    (make-generator :func func)))

#+nil
(transduce (take 10) #'cons (ints 0 :step 2))

(declaim (ftype (function ((or single-float double-float integer)) generator) random))
(defun random (limit)
  "Source: Yield an endless stream of random numbers, based on a given LIMIT."
  (make-generator :func (lambda () (cl:random limit))))

#+nil
(transduce (take 20) #'cons (random 10))

(declaim (ftype (function (cl:vector) generator) shuffle))
(defun shuffle (vec)
  "Source: Endlessly yield random elements from a given vector. Recall also that
strings are vectors too, so:

(transduce (take 5) #'string (shuffle \"Númenor\"))
=> \"mNNrú\"
"
  (let* ((len (length vec))
         (func (lambda () (aref vec (cl:random len)))))
    (make-generator :func func)))

#+nil
(transduce (take 5) #'cons (shuffle #("Colin" "Jack" "Natsume")))

(defgeneric cycle (seq)
  (:documentation "Source: Yield the values of a given SEQ endlessly."))

(defmethod cycle ((seq list))
  (if (null seq)
      (make-generator :func (lambda () *done*))
      (let* ((curr seq)
             (func (lambda ()
                     (cond ((null curr)
                            (setf curr (cdr seq))
                            (car seq))
                           (t (let ((next (car curr)))
                                (setf curr (cdr curr))
                                next))))))
        (make-generator :func func))))

(defmethod cycle ((seq cl:vector))
  "This works for strings as well."
  (if (zerop (length seq))
      (make-generator :func (lambda () *done*))
      (let* ((ix 0)
             (len (length seq))
             (func (lambda ()
                     (cond ((>= ix len)
                            (setf ix 1)
                            (aref seq 0))
                           (t (let ((next (aref seq ix)))
                                (setf ix (1+ ix))
                                next))))))
        (make-generator :func func))))

#+nil
(transduce (take 10) #'cons (cycle '(1 2 3)))

;; TODO: 2025-12-14 Extend this to chain any type of source, not just
;; generators. Would probably require that the various `*-reduce' functions are
;; made more generic so that proper dispatch can be done on each sub source,
;; while maintaining the current reduction state.
#+nil
(defun chain (&rest gens)
  "Source: Chain any number of generators together. When the current one exhausts
itself, the next one begins."
  (cond ((null gens) (lambda () *done*))
        (t (let ((head (generator-func (car gens)))
                 (tail (cdr gens)))
             (make-generator
              :func
              (lambda ()
                (let ((curr (funcall head)))
                  (cond ((and (eq curr *done*)
                              (null tail))
                         *done*)
                        ((eq curr *done*)
                         (setf head (generator-func (car tail)))
                         (setf tail (cdr tail))
                         (funcall head))
                        (t curr)))))))))

(defun chain (&rest gens)
  "Source: Chain any number of generators together. When the current one exhausts
itself, the next one begins."
  (cond ((null gens) (lambda () *done*))
        (t (let ((head (generator-func (car gens)))
                 (tail (cdr gens)))
             (labels ((recur ()
                        (let ((curr (funcall head)))
                          (cond ((and (eq curr *done*)
                                      (null tail))
                                 *done*)
                                ((eq curr *done*)
                                 (setf head (generator-func (car tail)))
                                 (setf tail (cdr tail))
                                 (recur))
                                (t curr)))))
               (make-generator :func #'recur))))))

#+nil
(defun silly ()
  (let ((curr 0))
    (make-generator :func (lambda ()
                            (cond ((> curr 10) *done*)
                                  (t (let ((prev curr))
                                       (incf curr)
                                       prev)))))))

#+nil
(defun dead ()
  (make-generator :func (lambda () *done*)))

#+nil
(transduce #'pass #'cons (chain (silly) (dead) (silly)))
