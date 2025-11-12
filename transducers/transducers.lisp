(in-package :transducers)

(defun pass (reducer)
  "Transducer: Just pass along each value of the transduction. Same in intent with
applying `map' to `identity', but this should be slightly more efficient. It is
at least shorter to type."
  (lambda (result &optional (input nil i?))
    (if i? (funcall reducer result input)
        (funcall reducer result))))

(declaim (ftype (function ((function (t) *)) *) map))
(defun map (f)
  "Transducer: Apply a function F to all elements of the transduction."
  (lambda (reducer)
    (lambda (result &optional (input nil i?))
      (if i? (funcall reducer result (funcall f input))
          (funcall reducer result)))))

#+nil
(transduce (map #'1+) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) filter))
(defun filter (pred)
  "Transducer: Only keep elements from the transduction that satisfy PRED."
  (lambda (reducer)
    (lambda (result &optional (input nil i?))
      (if i? (if (funcall pred input)
                 (funcall reducer result input)
                 result)
          (funcall reducer result)))))

#+nil
(transduce (filter #'evenp) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) filter-map))
(defun filter-map (f)
  "Transducer: Apply a function F to the elements of the transduction, but only
keep results that are non-nil.

(transduce (filter-map #'cl:first) #'cons '(() (2 3) () (5 6) () (8 9)))
=> (2 5 8)
"
  (lambda (reducer)
    (lambda (result &optional (input nil i?))
      (if i? (let ((x (funcall f input)))
               (if x
                   (funcall reducer result x)
                   result))
          (funcall reducer result)))))

#+nil
(transduce (filter-map #'cl:first) #'cons '(() (2 3) () (5 6) () (8 9)))

(declaim (ftype (function (fixnum) *) drop))
(defun drop (n)
  "Transducer: Drop the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n (1+ n)))
      (lambda (result &optional (input nil i?))
        (cond (i? (setf new-n (1- new-n))
                  (if (> new-n 0)
                      result
                      (funcall reducer result input)))
              (t (funcall reducer result)))))))

#+nil
(transduce (drop 3) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) drop-while))
(defun drop-while (pred)
  "Transducer: Drop elements from the front of the transduction that satisfy PRED."
  (lambda (reducer)
    (let ((drop? t))
      (lambda (result &optional (input nil i?))
        (if i? (cond ((and drop? (funcall pred input)) result)
                     (t (setf drop? nil)
                        (funcall reducer result input)))
            (funcall reducer result))))))

#+nil
(transduce (drop-while #'evenp) #'cons '(2 4 6 7 8 9))

(declaim (ftype (function (fixnum) *) take))
(defun take (n)
  "Transducer: Keep only the first N elements of the transduction."
  (lambda (reducer)
    (let ((new-n n))
      (lambda (result &optional (input nil i?))
        (declare (type fixnum new-n))
        (if i? (let ((result (if (> new-n 0)
                                 (funcall reducer result input)
                                 result)))
                 (setf new-n (1- new-n))
                 (if (<= new-n 0)
                     (ensure-reduced result)
                     result))
            (funcall reducer result))))))

#+nil
(transduce (take 3) #'cons '(1 2 3 4 5))
#+nil
(transduce (take 0) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) take-while))
(defun take-while (pred)
  "Transducer: Keep only elements which satisfy a given PRED, and stop the
transduction as soon as any element fails the test."
  (lambda (reducer)
    (lambda (result &optional (input nil i?))
      (if i? (if (not (funcall pred input))
                 (reduced result)
                 (funcall reducer result input))
          (funcall reducer result)))))

#+nil
(transduce (take-while #'evenp) #'cons '(2 4 6 8 9 2))

(defun uncons (reducer)
  "Transducer: Split up a transduction of cons cells."
  (lambda (result &optional (input nil i?))
    (if i? (let ((res (funcall reducer result (car input))))
             (if (reduced? res)
                 res
                 (funcall reducer res (cdr input))))
        (funcall reducer result))))

#+nil
(transduce #'uncons #'cons '((:a . 1) (:b . 2) (:c . 3)))
#+nil
(transduce #'uncons #'cons (plist '(:a 1 :b 2 :c 3)))
#+nil
(transduce (comp (map (lambda (pair) (cl:cons (car pair) (1+ (cdr pair)))))
                 #'uncons)
           #'cons (plist '(:a 1 :b 2 :c 3)))

(defun concatenate (reducer)
  "Transducer: Concatenate all the sublists and subvectors in the transduction."
  (let ((preserving-reducer (preserving-reduced reducer)))
    (lambda (result &optional (input nil i?))
      (if i? (typecase input
               (cl:list   (list-reduce preserving-reducer result input))
               (cl:vector (vector-reduce preserving-reducer result input))
               (otherwise (error 'unusable-type :type (type-of input))))
          (funcall reducer result)))))

#+nil
(transduce #'concatenate #'cons '((1 2 3) (4 5 6) (7 8 9)))
#+nil
(transduce #'concatenate
           #'cons (list (cl:vector 1 2 3) (cl:list 4 5 6) (cl:vector 7 8 9)))
#+nil
(transduce (comp #'concatenate (intersperse #\!))
           #'string '("hello" "there"))
#+nil
(transduce #'concatenate #'cons '(1 2 3))

(defun flatten (reducer)
  "Transducer: Entirely flatten all lists and vectors in the transduction,
regardless of nesting."
  (lambda (result &optional (input nil i?))
    (if i? (typecase input
             (cl:list (list-reduce (preserving-reduced (flatten reducer)) result input))
             (cl:vector (vector-reduce (preserving-reduced (flatten reducer)) result input))
             (otherwise (funcall reducer result input)))
        (funcall reducer result))))

#+nil
(transduce #'flatten #'cons '((1 2 3) 0 (4 (5) 6 "hi") 0 (7 #(8) 9) 0))

(declaim (ftype (function (fixnum) *) segment))
(defun segment (n)
  "Transducer: Partition the input into lists of N items. If the input stops, flush
any accumulated state, which may be shorter than N.

# Conditions

- `non-positive-integer': when a non-positive integer N is given.
"
  (if (< n 1)
      (restart-case (error 'non-positive-integer :fn "segment" :n n)
        (use-value (value)
          :report "Supply a new value and reattempt the transduction."
          :interactive (lambda () (prompt-new-value "Positive Integer: "))
          (segment value)))
      (lambda (reducer)
        (let ((i 0)
              (collect '()))
          (lambda (result &optional (input nil i?))
            (cond (i?
                   (setf collect (cl:cons input collect))
                   (setf i (1+ i))
                   (if (< i n)
                       result
                       (let ((next-input (reverse collect)))
                         (setf i 0)
                         (setf collect '())
                         (funcall reducer result next-input))))
                  (t (let ((result (if (zerop i)
                                       result
                                       (funcall reducer result (reverse collect)))))
                       (setf i 0)
                       (if (reduced? result)
                           (funcall reducer (reduced-val result))
                           (funcall reducer result))))))))))

#+nil
(transduce (segment 3) #'cons '(1 2 3 4 5))
#+nil
(transduce (segment -1) #'cons '(1 2 3 4 5))

(declaim (ftype (function ((function (t) *)) *) group-by))
(defun group-by (f)
  "Transducer: Group the input stream into sublists via some function F. The cutoff
criterion is whether the return value of F changes between two consecutive elements of the
transduction.

(transduce (group-by #'evenp) #'cons '(2 4 6 7 9 1 2 4 6 3))
=> ((2 4 6) (7 9 1) (2 4 6) (3))
"
  (lambda (reducer)
    (let ((prev 'nothing)
          (collect '()))
      (lambda (result &optional (input nil i?))
        (if i? (let ((fout (funcall f input)))
                 (cond ((or (equal fout prev) (eq prev 'nothing))
                        (setf prev fout)
                        (setf collect (cl:cons input collect))
                        result)
                       (t (let ((next-input (reverse collect)))
                            (setf prev fout)
                            (setf collect (list input))
                            (funcall reducer result next-input)))))
            (let ((result (if (null collect)
                              result
                              (funcall reducer result (reverse collect)))))
              (setf collect '())
              (if (reduced? result)
                  (funcall reducer (reduced-val result))
                  (funcall reducer result))))))))

#+nil
(transduce (group-by #'evenp) #'cons '(2 4 6 7 9 1 2 4 6 3))

(defun intersperse (elem)
  "Transducer: Insert an ELEM between each value of the transduction."
  (lambda (reducer)
    (let ((send-elem? nil))
      (lambda (result &optional (input nil i?))
        (if i? (cond (send-elem? (let ((result (funcall reducer result elem)))
                                   (if (reduced? result)
                                       result
                                       (funcall reducer result input))))
                     (t (setf send-elem? t)
                        (funcall reducer result input)))
            (funcall reducer result))))))

#+nil
(transduce (intersperse 0) #'cons '(1 2 3))

(defun enumerate (reducer)
  "Transducer: Index every value passed through the transduction into a cons pair.
Starts at 0."
  (let ((n 0))
    (lambda (result &optional (input nil i?))
      (if i? (let ((input (cl:cons n input)))
               (setf n (1+ n))
               (funcall reducer result input))
          (funcall reducer result)))))

#+nil
(transduce #'enumerate #'cons '("a" "b" "c"))

(defun log (logger)
  "Transducer: Call some LOGGER function for each step of the transduction. The
LOGGER must accept the running results and the current element as input. The
original items of the transduction are passed through as-is."
  (lambda (reducer)
    (lambda (result &optional (input nil i?))
      (cond (i?
             (funcall logger result input)
             (funcall reducer result input))
            (t (funcall reducer result))))))

#+nil
(transduce (log (lambda (_ n) (format t "Got: ~a~%" n))) #'cons '(1 2 3 4 5))

(declaim (ftype (function (fixnum) *) window))
(defun window (n)
  "Transducer: Yield N-length windows of overlapping values. This is different from
`segment' which yields non-overlapping windows. If there were fewer items in the
input than N, then this yields nothing.

# Conditions

- `non-positive-integer': when a non-positive integer N is given.
"
  (if (< n 1)
      (restart-case (error 'non-positive-integer :fn "window" :n n)
        (use-value (value)
          :report "Supply a new value and reattempt the transduction."
          :interactive (lambda () (prompt-new-value "Positive Integer: "))
          (window value)))
      (lambda (reducer)
        (let ((i 0)
              (q '()))
          (lambda (result &optional (input nil i?))
            (cond (i?
                   (setf q (append q (list input)))
                   (setf i (1+ i))
                   (cond ((< i n) result)
                         ((= i n) (funcall reducer result q))
                         (t (setf q (cdr q))
                            (funcall reducer result q))))
                  (t (funcall reducer result))))))))

#+nil
(transduce (window 3) #'cons '(1 2 3 4 5))
#+nil
(transduce (window -1) #'cons '(1 2 3 4 5))

(defun unique (reducer)
  "Transducer: Only allow values to pass through the transduction once each.
Stateful; this uses a Hash Table internally so could get quite heavy if you're
not careful."
  (funcall (unique-by #'identity) reducer))

#+nil
(transduce #'unique #'cons '(1 2 1 3 2 1 2 "abc"))

(defun unique-by (f)
  "Transducer: Only allow values to pass through the transduction once each,
determined by some key-mapping function. The function is only used to map the
values to something they should be compared to; the original values themselves
are what is passed through.

Stateful; this uses a Hash Table internally so could get quite heavy if you're
not careful."
  (lambda (reducer)
    (let ((seen (make-hash-table :test #'equal)))
      (lambda (result &optional (input nil i?))
        (if i? (let ((mapped (funcall f input)))
                 (cond ((gethash mapped seen) result)
                       (t (setf (gethash mapped seen) t)
                          (funcall reducer result input))))
            (funcall reducer result))))))

#++
(transduce (unique-by #'identity) #'cons '(1 2 1 3 2 1 2 "abc"))
#++
(transduce (unique-by #'cdr) #'cons '(("a" . 1) ("b" . 2) ("c" . 1) ("d" . 3)))

(defun dedup (reducer)
  "Transducer: Remove adjacent duplicates from the transduction."
  (let ((prev 'nothing))
    (lambda (result &optional (input nil i?))
      (if i? (cond ((equal prev input) result)
                   (t (setf prev input)
                      (funcall reducer result input)))
          (funcall reducer result)))))

#+nil
(transduce #'dedup #'cons '(1 1 1 2 2 2 3 3 3 4 3 3))

(declaim (ftype (function (fixnum) *) step))
(defun step (n)
  "Transducer: Only yield every Nth element of the transduction. The first element
of the transduction is always included.

# Conditions

- `non-positive-integer': when a non-positive integer N is given.

# Examples

(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))
=> (1 3 5 7 9)
"
  (if (< n 1)
      (restart-case (error 'non-positive-integer :fn "step" :n n)
        (use-value (value)
          :report "Supply a new value and reattempt the transduction."
          :interactive (lambda () (prompt-new-value "Positive Integer: "))
          (step value)))
      (lambda (reducer)
        (let ((curr 1))
          (lambda (result &optional (input nil i?))
            (if i? (cond ((= 1 curr)
                          (setf curr n)
                          (funcall reducer result input))
                         (t (setf curr (1- curr))
                            result))
                (funcall reducer result)))))))

#+nil
(transduce (step 2) #'cons '(1 2 3 4 5 6 7 8 9))

(declaim (ftype (function ((function (t t) *) t) *) scan))
(defun scan (f seed)
  "Transducer: Build up successsive values from the results of previous
applications of a given function F.

(transduce (scan #'+ 0) #'cons '(1 2 3 4))
=> (0 1 3 6 10)"
  (lambda (reducer)
    (let ((prev seed))
      (lambda (result &optional (input nil i?))
        (if i? (let* ((old prev)
                      (result (funcall reducer result old)))
                 (cond ((reduced? result) result)
                       (t (let ((new (funcall f prev input)))
                            (setf prev new)
                            result))))
            (let ((result (funcall reducer result prev)))
              (cond ((reduced? result) (funcall reducer (reduced-val result)))
                    (t (funcall reducer result)))))))))

#+nil
(transduce (scan #'+ 0) #'cons '(1 2 3 4))
#+nil
(transduce (comp (scan #'+ 0) (take 2)) #'cons '(1 2 3 4))

(defun once (item)
  "Transducer: Inject some ITEM onto the front of the transduction."
  (lambda (reducer)
    (let ((unused? t))
      (lambda (result &optional (input nil i?))
        (cond ((and i? unused?)
               (let ((res (funcall reducer result item)))
                 (cond ((reduced? res) res)
                       (t (setf unused? nil)
                          (funcall reducer res input)))))
              (i? (funcall reducer result input))
              ;; A weird case where they specified `once', but the original
              ;; Source itself was empty.
              ((and (not i?) unused?)
               (let ((res (funcall reducer result item)))
                 (if (reduced? res)
                     (funcall reducer (reduced-val res))
                     (funcall reducer res))))
              (t (funcall reducer result)))))))

#+nil
(transduce (comp (filter (lambda (n) (> n 10)))
                 (once 'hi)
                 (take 3))
           #'cons (ints 1))

#++
(transduce (once nil) #'cons '(0))
#++
(transduce (once nil) #'cons '())

(defun sexp (reducer)
  "Transducer: Interpret the data stream as S-expressions, yielding one at a time.
The stream can consist of either individual characters or whole strings. The
former would occur when transducing over a string directly. The latter would
occur when transducing over a stream/file line-by-line."
  (let ((acc (make-string-output-stream :element-type 'character))
        (parens 0))
    (lambda (result &optional (input nil i?))
      (declare (type fixnum parens))
      (labels ((one-char (res c)
                 (case c
                   (#\(
                    (incf parens)
                    (write-char c acc)
                    res)
                   (#\)
                    (decf parens)
                    (write-char c acc)
                    (cond ((zerop parens)
                           (let ((curr (get-output-stream-string acc)))
                             (setf acc (make-string-output-stream :element-type 'character))
                             (funcall reducer res curr)))
                          ((< parens 0) (error 'unmatched-closing-paren))
                          (t res)))
                   (otherwise (cond ((zerop parens) res)
                                    (t (write-char c acc)
                                       res)))))
               (a-string (res i)
                 (declare (type fixnum i))
                 (cond ((= i (length input)) res)
                       (t (let ((res (one-char res (char input i))))
                            (cond ((reduced? res) res)
                                  (t (a-string res (1+ i)))))))))
        (cond (i? (etypecase input
                    (character (one-char result input))
                    (cl:string (a-string result 0))))
              (t (funcall reducer result)))))))

#+nil
(transduce #'sexp #'cons "(+ 1 1)")
#+nil
(transduce #'sexp #'cons "(+ 1 1) (+ 2 2) (+ 3 (* 4 5))")
#+nil
(transduce #'sexp #'cons '("(+ 1 1)" "(+ 2 2)"))

(defun short-string ()
  "Allocate an adjustable short string for filling up later."
  (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))

(defun from-csv (reducer)
  "Transducer: Interpret the data stream as CSV data.

The first item found is assumed to be the header list, and it will be used to
construct useable hashtables for all subsequent items.

Note: This function makes no attempt to convert types from the
original parsed strings. If you want numbers, you will need to
further parse them yourself.

This function is expected to be passed \"bare\" to `transduce', so there is no
need for the caller to manually pass a REDUCER."
  (let ((headers nil))
    (lambda (result &optional (input nil i?))
      (if i? (let ((items (split-csv-line input)))
               (cond (headers (funcall reducer result (zipmap headers items)))
                     (t (setf headers items)
                        result)))
          (funcall reducer result)))))

#+nil
(transduce (comp (once "Name,Age")
                 #'from-csv
                 (map (lambda (hm) (gethash "Name" hm))))
           #'cons '("Alice,35" "Bob,26"))

#+nil
(transduce #'from-csv #'first '("Name,Money,Age" "Colin,\"10,000\",37"))

(defun split-csv-line (line)
  "Split a LINE of CSV data in a sane way.

This removes any extra whitespace that might be hanging around between elements."
  (let ((mode :comma))
    (transduce (comp (group-by (lambda (c)
                                 (cond ((and (eq mode :comma) (char= c #\,))
                                        nil)
                                       ((and (eq mode :quote) (char= c #\"))
                                        (setf mode :comma)
                                        t)
                                       ((and (eq mode :comma) (char= c #\"))
                                        (setf mode :quote)
                                        t)
                                       (t t))))
                     (map (lambda (chars) (cl:concatenate 'cl:string chars)))
                     (map (lambda (s) (string-trim " \"" s)))
                     (filter (lambda (s) (not (string= s ",")))))
               #'cons line)))

#+nil
(split-csv-line "Colin,   \"10,000\"   ,37")

(defun into-csv (headers)
  "Transducer: Given a sequence of HEADERS, rerender each item in the data stream
into a CSV string. It's assumed that each item in the transduction is a hash
table whose keys are strings that match the values found in HEADERS.

# Conditions

- `empty-argument': when an empty HEADERS sequence is given.
"
  (if (etypecase headers
        (list      (null headers))
        (cl:vector (zerop (length headers))))
      (restart-case (error 'empty-argument :fn "headers")
        (use-value (value)
          :report "Supply a default value and reattempt the transduction."
          :interactive (lambda () (prompt-new-value "Headers (must be a list): "))
          (into-csv value)))
      (lambda (reducer)
        (let ((unsent t))
          (lambda (result &optional (input nil i?))
            (if i? (if unsent
                       (let ((res (funcall reducer result (recsv headers))))
                         (cond ((reduced? res) res)
                               (t (setf unsent nil)
                                  (funcall reducer res (table-vals->csv headers input)))))
                       (funcall reducer result (table-vals->csv headers input)))
                (funcall reducer result)))))))

#+nil
(transduce (comp #'from-csv (into-csv '("Name" "Age")))
           #'cons '("Name,Age,Hair" "Colin,35,Blond" "Jack,26,Black"))
#+nil
(transduce (comp #'from-csv (into-csv '()))
           #'cons '("Name,Age,Hair" "Colin,35,Blond" "Jack,26,Black"))

(defun table-vals->csv (headers table)
  "Given some HEADERS to compare to, convert a hash TABLE to a rendered CSV string
of its values."
  (recsv (transduce (filter-map (lambda (k) (gethash k table)))
                    #'cons headers)))

(declaim (ftype (function (list) cl:string) recsv))
(defun recsv (items)
  "Reconvert some ITEMS into a comma-separated string."
  (format nil "~{~a~^,~}" items))

;; --- Higher Order Transducers --- ;;

(defun safe (tr)
  "Transducer: Given a transducer that may error, wrap it in `restart-case'
to provide various ways of skipping or handling that error."
  (lambda (reducer)
    (let ((callable (funcall tr reducer)))
      (lambda (result &optional (input nil i?))
        (if (not i?)
            (funcall callable result)
            (labels ((recurse (item)
                       (restart-case (funcall callable result item)
                         (next-item ()
                           :report "Skip this item and continue the transduction."
                           result)
                         (retry-item ()
                           :report "Put this item through the transduction chain once more."
                           (recurse item))
                         (alter-item (alter)
                           :report "Transform this item via a given function, then try the transduction again."
                           :interactive (lambda () (prompt-for-function-name))
                           (recurse (funcall alter item)))
                         (use-value (value)
                           :report "Supply a different value and reattempt the transduction."
                           :interactive (lambda () (prompt-new-value "Value: "))
                           (recurse value)))))
              (recurse input)))))))

#+nil
(transduce (comp (drop 1)
                 (safe (map (lambda (item) (if (= item 1) (error "無念") item))))
                 (take 1))
           #'cons '(0 1 2 3))

(defun branch (pred ta tb)
  "Transducer: If a PRED yields non-NIL on a value, proceed with transducer chain
TA. Otherwise, follow chain TB. This produces a kind of diamond pattern of data
flow within the transduction:

     /4a-5a-6a\\
1-2-3          7-8-9
     \\4b-5b---/

Assuming that TA here is some composition of three transducer steps, and TB is a
composition of two. Naturally, if you have other steps beyond the fork (Step 7
above), you should make sure that they can handle the return values of both
sides!

(transduce (comp (map #'1+)
                 (branch #'evenp
                         (map (comp #'write-to-string #'1+))
                         (map (const \"Odd!\")))
                 (map #'length))
           #'cons (range 1 6))
=> (1 4 1 4 1)
"
  (lambda (reducer)
    (let ((fa (funcall ta reducer))
          (fb (funcall tb reducer)))
      (lambda (result &optional (input nil i?))
        (if i? (if (funcall pred input)
                   (funcall fa result input)
                   (funcall fb result input))
            ;; It _shouldn't_ matter that we're skipping the fork, since if
            ;; no input is left, we want to get access to the "real" reducer
            ;; at the bottom of the composed transducer stack. We know is at
            ;; least as deep as the original reducer we were passed.
            (funcall reducer result))))))

#+nil
(transduce (comp (take 5)
                 (map #'1+)
                 (branch #'evenp
                         (map (comp #'write-to-string #'1+))
                         (map (const "Odd!")))
                 (map #'length))
           #'cons (ints 1))

(defun split (ta ra)
  "Transducer: Split off a new transducer chain, feeding it each input as well. It
reduces on its own given RA reducer. The final result is a cons-cell where the
first value is the result of the original transduction, and the second is that
of the branch."
  (lambda (reducer)
    (let ((fa (funcall ta ra))
          (other-res (funcall ra)))
      (lambda (result &optional (input nil i?))
        (cond (i?
               (unless (reduced? other-res)
                 (setf other-res (funcall fa other-res input)))
               (funcall reducer result input))
              (t (cl:cons (funcall reducer result)
                          (funcall fa (ensure-unreduced other-res)))))))))

#+nil
(transduce (comp (take 9)
                 (map #'1+)
                 (split (comp (filter #'evenp) (take 3)) #'+)
                 (map #'1-))
           #'cons (ints 1))

(defun inject (f)
  "Transducer: For each value in the transduction that actually affects the final
result (tested with `EQ'), inject an extra transduction step into the chain
immediately after this point. Accumulates, such that each new injection appears
before the previous one."
  (lambda (reducer)
    (let ((reducer reducer))
      (lambda (result &optional (input nil i?))
        (if i? (let ((new-res (funcall reducer result input)))
                 (if (eq result new-res)
                     new-res
                     (let* ((xform (funcall f input))
                            (next  (funcall xform reducer)))
                       (setf reducer next)
                       new-res)))
            (funcall reducer result))))))

#+nil
(transduce (comp (inject (lambda (prime) (filter (lambda (n) (/= 0 (mod n prime))))))
                 (take 10))
           #'cons (ints 3 :step 2))

#+nil
(transduce (map #'length) #'+ #p"transducers.lisp")

(defun par (f ta tb)
  "Transducer: Traverse two transducer paths at the same time, combining the
results of each path with a given function F before moving on. This is similar
to the `zip' concept from other languages.

Given the following transducer chain:

     /4a-5a-6a\\
1-2-3          7-8-9
     \\4b-5b---/

The function F would be applied right before Step 7. In this case, the function
would have to expect the output types of Step 6a and 5b as its arguments.

Note 1: If either branch yields a 'reduced' value, then the entire chain
short-circuits and that is the value applied to the reducer one final time.

Note 2: This function has potentially non-intuitive behaviour with regards to
functions like `filter' that don't always contribute to the final result. The
function F will only be applied (and thus pass values on) if both branches
produced a new value. If either branch 'died' for a particular value, then so
too will the other branch. If this is undesirable, see the higher-order
transducer `tri' for an alternative.
"
  (lambda (reducer)
    (let ((fa (funcall ta #'last))
          (fb (funcall tb #'last)))
      (lambda (result &optional (input nil i?))
        (if i? (let ((ra (funcall fa result input))
                     (rb (funcall fb result input)))
                 (cond ((reduced? ra) ra)
                       ((reduced? rb) rb)
                       ((eq ra result) result)
                       ((eq rb result) result)
                       (t (funcall reducer result (funcall f ra rb)))))
            (funcall reducer result))))))

#+nil
(transduce (comp (take 2)
                 (map (lambda (n) (* 3 n)))
                 (par (lambda (a b) (+ a b))
                      (comp (map (lambda (a) (* 1000 a))))
                      (comp (map (lambda (b) (* 3 b)))))
                 (map (lambda (n) (* 2 n))))
           #'+ (ints 1))

;; FIXME There is a problem here. If one side reduces early (thus waiting for
;; the other), new input values are lost on the waiting branch. I must decide if
;; this is acceptable.
(defun tri (f ta ra tb rb)
  "The Trident."
  (lambda (reducer)
    (let* ((fa (funcall ta ra))
           (fb (funcall tb rb))
           (a-id (funcall ra))
           (b-id (funcall rb))
           (res-a a-id)
           (res-b b-id))
      (lambda (result &optional (input nil i?))
        (cond (i?
               (unless (reduced? res-a)
                 (setf res-a (funcall fa res-a input)))
               (unless (reduced? res-b)
                 (setf res-b (funcall fb res-b input)))
               (when (and (reduced? res-a)
                          (reduced? res-b))
                 (let* ((fused (funcall f
                                        (funcall fa (reduced-val res-a))
                                        (funcall fb (reduced-val res-b))))
                        (res (funcall reducer result fused)))
                   (setf res-a a-id)
                   (setf res-b b-id)
                   res)))
              (t (funcall reducer result)))))))

#+nil
(transduce (comp (take 19)
                 (map #'1+)
                 (tri #'*
                      (comp (filter #'evenp) (take 3)) #'+
                      (comp (filter #'oddp)  (take 4)) #'*)
                 (map #'1+))
           #'cons
           (ints 1))
