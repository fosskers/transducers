(in-package :transducers)

(defun ensure-function (arg)
  (cond ((functionp arg) arg)
        ((symbolp arg) (ensure-function (symbol-function arg)))
        (t (error "Argument is not a function: ~a" arg))))

(defmacro comp (function &rest functions)
  "Function composition.

(funcall (comp #'1+ #'length) \"foo\") == (1+ (length \"foo\"))"
  (let ((args (gensym "COMP-ARGS-"))
        (reversed (reverse (cl:cons function functions))))
    `(lambda (&rest ,args)
       ,(reduce (lambda (data fn)
                  `(funcall ,fn ,data))
                (cdr reversed)
                :initial-value `(apply ,(car reversed) ,args)))))

#+nil
(funcall (comp (const 1337) (lambda (n) (* 2 n)) #'1+) 1)

(defun const (item)
  "Return a function that ignores its argument and returns ITEM instead."
  (lambda (x)
    (declare (ignore x))
    item))

(declaim (ftype (function ((or t reduced)) reduced) ensure-reduced))
(defun ensure-reduced (x)
  "Ensure that X is reduced."
  (if (reduced? x)
      x
      (reduced x)))

(declaim (ftype (function ((or t reduced)) *) ensure-unreduced))
(defun ensure-unreduced (x)
  "Ensure that X is unreduced."
  (if (reduced? x)
      (reduced-val x)
      x))

(defun preserving-reduced (reducer)
  "A helper function that wraps a reduced value twice since reducing
functions (like list-reduce) unwraps them. `concatenate' is a good example: it
re-uses its reducer on its input using list-reduce. If that reduction finishes
early and returns a reduced value, list-reduce would 'unreduce' that value and
try to continue the transducing process."
  (lambda (a b)
    (let ((result (funcall reducer a b)))
      (if (reduced? result)
          (reduced result)
          result))))

(defun zipmap (keys vals)
  "Form a hashmap with the KEYS mapped to the corresponding VALS.

Borrowed from Clojure, thanks guys."
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (k v) (setf (gethash k table) v)) keys vals)
    table))

;; NOTE: 2025-11-08 This is no longer used within `transducers' itself, but I
;; leave it here as I know I use it secretly in other projects via `::'.
(declaim (ftype (function (cl:string &key (:separator character)) list) string-split))
(defun string-split (string &key (separator #\space))
  "Split a string into a list of substrings according to some configurable
separator character."
  (labels ((recurse (acc start end)
             (declare (type fixnum start end))
             (cond ((and (< start 0) (< end 0)) acc)
                   ;; The separator was found at the very start of the string.
                   ((and (zerop start) (eql separator (char string start)))
                    (cl:cons "" (cl:cons (subseq string (1+ start) (1+ end)) acc)))
                   ;; We got to the beginning without seeing another separator.
                   ((zerop start) (cl:cons (subseq string start (1+ end)) acc))
                   ;; Normal separator detection: collect the piece we've built.
                   ((eql separator (char string start))
                    (recurse (cl:cons (subseq string (1+ start) (1+ end)) acc)
                             (1- start)
                             (1- start)))
                   ;; Base case: just keep moving.
                   (t (recurse acc (1- start) end)))))
    ;; We start from the end of the string and go backwards, in order to neatly
    ;; build up the final list without needing to `reverse'.
    (let ((end (1- (length string))))
      (recurse '() end end))))

#++
(string-split ",Hello,my,name,is,Colin," :separator #\,)
