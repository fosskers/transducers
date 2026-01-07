(in-package :transducers)

(declaim (ftype (function (&optional list t) list) cons))
(defun cons (&optional (acc nil a?) (input nil i?))
  "Reducer: Collect all results as a list."
  (cond ((and a? i?) (cl:cons input acc))
        ((and a? (not i?)) (nreverse acc))
        (t '())))

(declaim (ftype (function (&optional list t) list) snoc))
(defun snoc (&optional (acc nil a?) (input nil i?))
  "Reducer: Collect all results as a list, but results are reversed.
In theory, slightly more performant than `cons' since it performs no final
reversal."
  (cond ((and a? i?) (cl:cons input acc))
        ((and a? (not i?)) acc)
        (t '())))

(defun string (&optional (acc nil a?) (input #\z i?))
  "Reducer: Collect a stream of characters into to a `simple-string'."
  (cond ((and a? i?)
         (write-char input acc)
         acc)
        ((and a? (not i?)) (get-output-stream-string acc))
        (t (make-string-output-stream :element-type 'character))))

#+nil
(transduce (map #'char-upcase) #'string "hello")

(defun base-string (&optional (acc nil a?) (input #\z i?))
  "Reducer: Collect a stream of characters into to a single string of `base-char'."
  (cond ((and a? i?)
         (write-char input acc)
         acc)
        ((and a? (not i?)) (get-output-stream-string acc))
        (t (make-string-output-stream :element-type 'base-char))))

#+nil
(transduce (map #'char-upcase) #'base-string "hello")

(declaim (ftype (function (&optional (or cl:vector null) t) cl:vector) vector))
(defun vector (&optional (acc nil a?) (input nil i?))
  "Reducer: Collect a stream of values into a vector."
  (cond ((and a? i?) (vector-push-extend input acc) acc)
        ((and a? (not i?)) acc)
        (t (make-array 16 :adjustable t :fill-pointer 0))))

#+nil
(vector-transduce (map #'1+) #'vector #(1 2 3))

(defun bit-vector (&optional (acc nil a?) (input 0 i?))
  "Reducer: Collect a stream of `bit' values into a `bit-vector'."
  (cond ((and a? i?) (vector-push-extend input acc) acc)
        ((and a? (not i?)) acc)
        (t (make-array 16 :element-type 'bit :adjustable t :fill-pointer 0))))

#+nil
(transduce (take 20) #'bit-vector (cycle #(0 1)))

(declaim (ftype (function (&optional (or cl:hash-table null) t) cl:hash-table) hash-table))
(defun hash-table (&optional (acc nil a?) (input nil i?))
  "Reducer: Collect a stream of key-value cons pairs into a hash table."
  (cond ((and a? i?) (destructuring-bind (key . val) input
                       (setf (gethash key acc) val)
                       acc))
        ((and a? (not i?)) acc)
        (t (make-hash-table :test #'equal))))

#+nil
(transduce #'enumerate #'hash-table '("a" "b" "c"))

(declaim (ftype (function (&optional fixnum t) fixnum) count))
(defun count (&optional (acc 0 a?) (input nil i?))
  "Reducer: Count the number of elements that made it through the transduction."
  (declare (ignore input))
  (cond ((and a? i?) (1+ acc))
        ((and a? (not i?)) acc)
        (t 0)))

#+nil
(transduce #'pass #'count '(1 2 3 4 5))

(defun median (&optional (acc nil a?) (input nil i?))
  "Reducer: Calculate the median value of all orderable elements (numbers,
characters, strings) in a transduction. The elements are sorted once before the
median is extracted.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (cond ((and a? i?) (cl:cons input acc))
        ((and a? (not i?))
         (if (null acc)
             (error 'empty-transduction :msg "`median' called on an empty transduction.")
             ;; HACK 2024-08-22 More robust comparison.
             ;;
             ;; It would be nice if there were a generic way to compare, or if
             ;; the user could pass in a function for this.
             (let* ((cmp    (typecase (car acc)
                              (cl:string #'string<)
                              (cl:character #'char<)
                              (otherwise #'<)))
                    (len    (length acc))
                    (ix     (floor (/ len 2)))
                    (sorted (sort acc cmp)))
               (nth ix sorted))))
        (t '())))

#+nil
(transduce #'pass #'median '(0 1 2 3 4))

(defstruct avg
  "A helper struct for the `average' reducer."
  ;; The number of items seen thusfar.
  (count 0 :type fixnum)
  ;; Some running total of summed values, etc.
  (total 0 :type real))

(defun average (&optional (acc nil a?) (input nil i?))
  "Reducer: Calculate the average value of all numeric elements in a transduction.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (cond ((and a? i?)
         (incf (avg-count acc))
         (incf (avg-total acc) input)
         acc)
        ((and a? (not i?))
         (if (zerop (avg-count acc))
             (error 'empty-transduction :msg "`average' called on an empty transduction.")
             (/ (avg-total acc) (avg-count acc))))
        (t (make-avg :count 0 :total 0))))

#+nil
(transduce #'pass #'average '(1 2 3 4 5 6))
#+nil
(transduce (filter #'evenp) #'average '(1 3 5))

(defun variance (mean)
  "Reducer: Calculate the variance of a stream of values, given their mean.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (incf (avg-count acc))
           (incf (avg-total acc)
                 (expt (- input mean) 2))
           acc)
          ((and a? (not i?))
           (if (zerop (avg-count acc))
               (error 'empty-transduction :msg "`variance' called on an empty transduction.")
               (/ (avg-total acc) (avg-count acc))))
          (t (make-avg :count 0 :total 0)))))

#+nil
(let* ((nums '(1 2 3 4 5 6 7 8 9 10))
       (mean (transduce #'pass #'average nums)))
  (sqrt (transduce #'pass (variance mean) nums)))

(defun ratio (pred)
  "Reducer: The percentage of items that satisfied a predicate. The final value
will always be between 0 and 1.

# Conditions

- `empty-transduction': when no values made it through the transduction."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (incf (avg-count acc))
           (when (funcall pred input)
             (incf (avg-total acc)))
           acc)
          ((and a? (not i?))
           (if (zerop (avg-count acc))
               (error 'empty-transduction :msg "`ratio' called on an empty transduction.")
               (/ (avg-total acc) (avg-count acc))))
          (t (make-avg :count 0 :total 0)))))

#+nil
(transduce #'pass (ratio #'evenp) #(1 2 3 4 5 6))
#+nil
(transduce #'pass (ratio #'evenp) #())

(declaim (ftype (function ((function (t) *)) *) any?))
(defun any? (pred)
  "Reducer: Yield t if any element in the transduction satisfies PRED.
Short-circuits the transduction as soon as the condition is met."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (if (funcall pred input)
               (reduced t)
               nil))
          ((and a? (not i?)) acc)
          (t nil))))

#+nil
(transduce #'pass (any? #'evenp) '(1 3 5 7 9))
#+nil
(transduce #'pass (any? #'evenp) '(1 3 5 2 7 9))

(declaim (ftype (function ((function (t) *)) *) all?))
(defun all? (pred)
  "Reducer: Yield t if all elements of the transduction satisfy PRED.
Short-circuits with NIL if any element fails the test."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (let ((test (funcall pred input)))
             (if (and acc test)
                 t
                 (reduced nil))))
          ((and a? (not i?)) acc)
          (t t))))

#+nil
(transduce #'pass (all? #'oddp) '(1 3 5 7 9))
#+nil
(transduce #'pass (all? #'oddp) '(1 3 5 7 9 2))

(defun first (&optional (acc 'transducers-none a?) (input nil i?))
  "Reducer: Yield the first value of the transduction. As soon as this first value
is yielded, the entire transduction stops.

# Conditions

- `empty-transduction': when no values made it through the transduction.
"
  (cond ((and a? i?) (reduced input))
        ((and a? (not i?))
         (if (eq 'transducers-none acc)
             (restart-case (error 'empty-transduction :msg "first: the transduction was empty.")
               (use-value (value)
                 :report "Supply a fallback value and end the transduction."
                 :interactive (lambda () (prompt-new-value "Fallback: "))
                 value))
             acc))
        (t 'transducers-none)))

#+nil
(transduce (filter #'oddp) #'first '(2 4 6 7 10))
#+nil
(transduce (filter #'oddp) #'first '(2 4 6 10))

(defun last (&optional (acc 'transducers-none a?) (input nil i?))
  "Reducer: Yield the last value of the transduction.

# Conditions

- `empty-transduction': when no values made it through the transduction.
"
  (cond ((and a? i?) input)
        ((and a? (not i?))
         (if (eq 'transducers-none acc)
             (restart-case (error 'empty-transduction :msg "last: the transduction was empty.")
               (use-value (value)
                 :report "Supply a fallback value and end the transduction."
                 :interactive (lambda () (prompt-new-value "Fallback: "))
                 value))
             acc))
        (t 'transducers-none)))

#+nil
(transduce #'pass #'last '(2 4 6 7 10))
#+nil
(transduce #'pass #'last '())

(declaim (ftype (function ((function (t t) *) &optional t) *) fold))
(defun fold (f &optional (seed nil seed-p))
  "Reducer: The fundamental reducer. `fold' creates an ad-hoc reducer based on
a given 2-argument function. An optional SEED value can also be given as the
initial accumulator value, which also becomes the return value in case there
were no input left in the transduction.

Functions like `+' and `*' are automatically valid reducers, because they yield
sane values even when given 0 or 1 arguments. Other functions like `max' cannot
be used as-is as reducers since they can't be called without arguments. For
functions like this, `fold' is appropriate.

# Conditions

- `empty-transduction': if no SEED is given and the transduction is empty.
"
  (if seed-p
      (lambda (&optional (acc nil a?) (input nil i?))
        (cond ((and a? i?) (funcall f acc input))
              ((and a? (not i?)) acc)
              (t seed)))
      (lambda (&optional (acc nil a?) (input nil i?))
        (cond ((and a? i?)
               (if (eq acc 'transducers-none)
                   input
                   (funcall f acc input)))
              ((and a? (not i?))
               (if (eq acc 'transducers-none)
                   (restart-case (error 'empty-transduction :msg "fold was called without a seed, but the transduction was also empty.")
                     (use-value (value)
                       :report "Supply a default value and end the transduction."
                       :interactive (lambda () (prompt-new-value "Default value: "))
                       value))
                   acc))
              (t 'transducers-none)))))

#+nil
(transduce #'pass (fold #'cl:max) '())
#+nil
(transduce #'pass (fold #'cl:max 0) '(1 2 3 4 1000 5 6))
#+nil
(transduce #'pass (fold #'cl:max) '(1 2 3 4 1000 5 6))

(defun max (default)
  "Deprecated: Use `fold' and pass it `cl:max' instead."
  (warn "`max' is deprecated; use `fold' instead.")
  (fold #'cl:max default))

(defun min (default)
  "Deprecated: Use `fold' and pass it `cl:min' instead."
  (warn "`min' is deprecated; use `fold' instead.")
  (fold #'cl:min default))

(declaim (ftype (function ((function (t) *) &key (:default t)) *) find))
(defun find (pred &key default)
  "Reducer: Find the first element in the transduction that satisfies a given PRED.
Yields `nil' if no such element were found, unless a DEFAULT is provided."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (if (funcall pred input)
               (reduced input)
               default))
          ((and a? (not i?)) acc)
          (t default))))

#+nil
(transduce #'pass (find #'evenp) '(1 3 5 6 9))
#+nil
(transduce #'pass (find #'evenp :default 1000) '(1 3 5 9))

(defun for-each (&rest vargs)
  "Reducer: Deprecated. Use `for' instead."
  (declare (ignore vargs))
  t)

#+nil
(transduce (map (lambda (n) (format t "~a~%" n))) #'for-each #(1 2 3 4))

(defun for (f)
  "Reducer: Call some effectful function on every item to be reduced, and yield a
final T."
  (lambda (&optional (acc nil a?) (input nil i?))
    (declare (ignore acc))
    (cond ((and a? i?) (funcall f input))
          ((and a? (not i?)) t)
          (t nil))))

#++
(transduce #'pass (for (lambda (n) (format t "~a~%" n))) #(1 2 3 4))

(defun quantities (test)
  "Reducer: Count the occurrences of every item in the transduction, given some
equality predicate."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (let ((count (gethash input acc)))
             (cond (count (incf (gethash input acc))
                          acc)
                   (t (setf (gethash input acc) 1)
                      acc))))
          ((and a? (not i?)) acc)
          (t (make-hash-table :size 32 :test test)))))

#+nil
(transduce #'pass (quantities #'eql) '(1 1 2 1 3 4 5 4 3 2 1))

(defun partition (pred)
  "Reducer: Given some predicate function, split the stream and collect its values
into two lists, those items that passed the check, and those that failed."
  (lambda (&optional (acc nil a?) (input nil i?))
    (cond ((and a? i?)
           (destructuring-bind (passed . failed) acc
             (cond ((funcall pred input)
                    (setf (car acc) (cl:cons input passed))
                    acc)
                   (t (setf (cdr acc) (cl:cons input failed))
                      acc))))
          ((and a? (not i?))
           (destructuring-bind (passed . failed) acc
             (values (nreverse passed) (nreverse failed))))
          (t (cl:cons '() '())))))

#+nil
(transduce #'pass (partition #'evenp) '(1 2 3 4 5))
