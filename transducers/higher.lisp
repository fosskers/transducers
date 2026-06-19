(in-package :transducers)

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
composition of two. Naturally, if you have other steps beyond the fork
point (Step 7 above), you should make sure that they can handle the return
values of both sides!

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
        (cond (i? (if (funcall pred input)
                      (funcall fa result input)
                      (funcall fb result input)))
              ;; It _shouldn't_ matter that we're skipping the fork, since if
              ;; no input is left, we want to get access to the "real" reducer
              ;; at the bottom of the composed transducer stack. We know is at
              ;; least as deep as the original reducer we were passed.
              (t (funcall reducer result)))))))

#+nil
(transduce (comp (map #'string-upcase)
                 (branch (lambda (word) (evenp (length word)))
                         (map #'reverse)
                         (map #'string-capitalize))
                 (map (lambda (s) (string-trim " " s))))
           #'cons
           '(" waiting " " hungry " " within " " the " " toronto " " airport "))

;; NOTE: 2026-06-20 I don't like this. It's already exported so I'm going to
;; leave it that way, but it's not advertised in the README.
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

;; NOTE: 2026-06-20 Similar to `split', this is already exported but I don't
;; think it's correct. It remains unadvertised.
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
