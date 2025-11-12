(in-package :transducers)

#+sbcl
(require :sb-sprof)

;; --- Function Composition --- ;;

(defun comp-orig (function &rest functions)
  "Function composition.

(funcall (comp #'1+ #'length) \"foo\") == (1+ (length \"foo\"))"
  (reduce (lambda (f g)
            (let ((f (ensure-function f))
                  (g (ensure-function g)))
              (lambda (&rest arguments)
                (funcall f (apply g arguments)))))
          functions
          :initial-value function))

#+nil
(progn
  (format t "--- ORIG ---~%")
  (time (dotimes (n 100000)
          (transduce
           (comp-orig (drop 1)
                      (filter #'evenp)
                      (map #'1+)
                      (once 37)
                      (intersperse 0)
                      (take 100))
           #'+ (ints 0))))
  (format t "--- MACRO ---~%")
  (time (dotimes (n 100000)
          (transduce
           (comp (drop 1)
                 (filter #'evenp)
                 (map #'1+)
                 (once 37)
                 (intersperse 0)
                 (take 100))
           #'+ (ints 0)))))

;; --- FILE READING --- ;;

;; Hmm, this one is dominated memory-wise by reading the strings from the file.
;; 99% of the allocation is "not me".

(defun file-reading ()
  (transduce
   (comp (step 2) (map #'length) (filter #'evenp))
   #'+
   #p"README.org"))

#+nil
(file-reading)

#+nil
(time (dotimes (n 10000)
        (file-reading)))

#+nil
(sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph :mode :alloc)
  (dotimes (n 10000)
    (file-reading)))

;; (0) Base: 2.1b, 2.0s

;; --- Summation --- ;;

;; Fast and allocates nothing if the result doesn't overflow fixnum.

(defun summation ()
  (transduce (comp (filter #'oddp)
                   (take 1000000)
                   (map (lambda (n) (* n n))))
             #'+
             (ints 1)))

#+nil
(time (summation))

#+nil
(sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.00001 :report :graph)
  (dotimes (n 10)
    (summation)))

;; (0) Base: 271m, 1.19s

;; --- STRINGS --- ;;

(defun string-cons (&optional (acc nil a?) (input #\z i?))
  "Reducer: Collect a stream of characters into to a single string."
  (cond ((and a? i?) (cl:cons input acc))
        ((and a? (not i?)) (cl:concatenate 'cl:string (nreverse acc)))
        (t '())))

(defun string-push (&optional (acc nil a?) (input #\z i?))
  "Reducer: Collect a stream of characters into to a single string."
  (cond ((and a? i?) (vector-push-extend input acc) acc)
        ((and a? (not i?)) acc)
        (t (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))))

(defun string-with-stream (&optional (acc nil a?) (input #\z i?))
  "Reducer: Collect a stream of characters into to a single string."
  (cond ((and a? i?)
         (write-char input acc)
         acc)
        ((and a? (not i?)) (get-output-stream-string acc))
        (t (make-string-output-stream :element-type 'character))))

#+nil
(progn
  (format t "--- CONS ---~%")
  (time (dotimes (n 100)
          (transduce (comp (take 10000) (map #'char-upcase)) #'string-cons (repeat #\a))))
  (format t "--- PUSH ---~%")
  (time (dotimes (n 100)
          (transduce (comp (take 10000) (map #'char-upcase)) #'string-push (repeat #\a))))
  (format t "--- STREAM ---~%")
  (time (dotimes (n 100)
          (transduce (comp (take 10000) (map #'char-upcase)) #'string-with-stream (repeat #\a)))))

;; OBSERVATIONS
;;
;; - SBCL: stream is fastest and uses the least memory beyond strings of length 1000 or so.
;; - ECL: About the same.
;; - Allegro: Tons of consing for CONS. PUSH slightly less memory than STREAM?

;; CONCLUSIONS
;;
;; I will adopt the stream approach overall as it is more efficient for large
;; strings and yields a better string type.

#+nil
(transduce (comp (take 10000) (map #'char-upcase)) #'string-cons (repeat #\a))

#+nil
(transduce (comp (take 10000) (map #'char-upcase)) #'string-with-stream (repeat #\a))

#+nil
(transduce (comp (take 10000) (map #'char-upcase)) #'string-push (repeat #\a))

#+nil
(progn
  (format t "--- string with base-char ---~%")
  (time (dotimes (n 1000)
          (transduce (comp (take 10000) (map #'char-upcase)) #'string (repeat #\a))))
  (format t "--- base-string with base-char ---~%")
  (time (dotimes (n 1000)
          (transduce (comp (take 10000) (map #'char-upcase)) #'base-string (repeat #\a))))
  (format t "--- string with unicode ---~%")
  (time (dotimes (n 1000)
          (transduce (comp (take 10000) (map #'char-upcase)) #'string (repeat #\天)))))

#+nil
(let ((s (transduce (take 100) #'string (repeat #\a))))
  (format t "--- string-transduce ---~%")
  (time (dotimes (n 10000)
          (string-transduce #'pass #'string s)))
  (format t "--- simple-string-transduce ---~%")
  (time (dotimes (n 10000)
          (simple-string-transduce #'pass #'string s))))

;; CONCLUSIONS
;;
;; Even for strings of length 100, the simple-string variant is faster.

;; --- SEXP --- ;;

(defun sexp-push (reducer)
  "Transducer: Interpret the data stream as S-expressions, yielding one at a time.
The stream can consist of either individual characters or whole strings. The
former would occur when transducing over a string directly. The latter would
occur when transducing over a stream/file line-by-line."
  (let ((acc (short-string))
        (parens 0))
    (lambda (result &optional (input nil i?))
      (declare (type fixnum parens))
      (labels ((one-char (res c)
                 (case c
                   (#\(
                    (incf parens)
                    (vector-push-extend c acc)
                    res)
                   (#\)
                    (decf parens)
                    (vector-push-extend c acc)
                    (cond ((zerop parens)
                           (let ((curr acc))
                             (setf acc (short-string))
                             (funcall reducer res curr)))
                          ((< parens 0) (error 'unmatched-closing-paren))
                          (t res)))
                   (otherwise (cond ((zerop parens) res)
                                    (t (vector-push-extend c acc)
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

(defun sexp-push-convert (reducer)
  "Transducer: Interpret the data stream as S-expressions, yielding one at a time.
The stream can consist of either individual characters or whole strings. The
former would occur when transducing over a string directly. The latter would
occur when transducing over a stream/file line-by-line."
  (let ((acc (short-string))
        (parens 0))
    (lambda (result &optional (input nil i?))
      (declare (type fixnum parens))
      (labels ((one-char (res c)
                 (case c
                   (#\(
                    (incf parens)
                    (vector-push-extend c acc)
                    res)
                   (#\)
                    (decf parens)
                    (vector-push-extend c acc)
                    (cond ((zerop parens)
                           (let ((curr (coerce acc '(simple-array character (*)))))
                             (setf acc (short-string))
                             (funcall reducer res curr)))
                          ((< parens 0) (error 'unmatched-closing-paren))
                          (t res)))
                   (otherwise (cond ((zerop parens) res)
                                    (t (vector-push-extend c acc)
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
(let ((s (uiop:read-file-string #p"tests/sexp.txt")))
  (format t "--- PUSH ---~%")
  (time (dotimes (n 1000)
          (transduce #'sexp-push #'cons s)))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp-push #'cons "(+ 1 1) (+ 2 2) (+ 3 (* 4 5)) (+ 1 1) (+ 1 1) (+ 1 2 3 4)")))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp-push #'cons "(+ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (+ 1 1)")))
  (format t "--- PUSH CONVERT ---~%")
  (time (dotimes (n 1000)
          (transduce #'sexp-push-convert #'cons s)))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp-push-convert #'cons "(+ 1 1) (+ 2 2) (+ 3 (* 4 5)) (+ 1 1) (+ 1 1) (+ 1 2 3 4)")))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp-push-convert #'cons "(+ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (+ 1 1)")))
  (format t "--- STREAM ---~%")
  (time (dotimes (n 1000)
          (transduce #'sexp #'cons s)))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp #'cons "(+ 1 1) (+ 2 2) (+ 3 (* 4 5)) (+ 1 1) (+ 1 1) (+ 1 2 3 4)")))
  #+nil
  (time (dotimes (n 100000)
          (transduce #'sexp #'cons "(+ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (+ 1 1)"))))

;; OBSERVATIONS
;;
;; SBCL: For only a few sexps, PUSH uses less memory, but STREAM is a bit
;; faster. Adding more sexps actually makes STREAM. use more memory. If the
;; sexps are few but long, STREAM is faster yet, although PUSH is still using
;; the least memory. STREAM the best for the "large file" scenario.
;;
;; ECL: For tiny sexps, PUSH is faster and uses far less memory.
;;
;; Allegro: STREAM is significantly slower and uses several times more memory.
;; PUSH CONVERT a happy medium? But for a file with multiple large function
;; definitions, STREAM is the winner again.
;;
;; CONCLUSION
;;
;; Memory usage depends on how big the sexps are and how many of them there are.
;; Overall, rapidly allocating many streams seems to take more memory. However,
;; for large files of large sexps, the STREAM approach basically always wins.
;; I will go with STREAM because:
;;
;; - For SBCL it performs the best.
;; - It is competitive with other compilers.
;; - It yields a favourable return type without extra conversions.

;; --- Looping --- ;;

(defun loop-foldl (function initial-value list)
  (unless list
    (return-from loop-foldl initial-value))
  (loop for x in list
        for result = (funcall function initial-value x)
          then (funcall function result x)
        finally (return result)))

#+nil
(let ((a (make-list 1000000 :initial-element 1)))
  (format t "--- TRANSDUCERS ---~%")
  (time (dotimes (n 100)
          (transduce #'pass #'+ a)))
  (format t "--- LOOP ---~%")
  (time (dotimes (n 100)
          (loop-foldl #'+ 0 a))))

;; OBSERVATIONS
;;
;; Transducers is allocating, likely due to the closure inside of `pass'.
