(defpackage transducers/tests
  (:use :cl :parachute)
  (:local-nicknames (#:t #:transducers)
                    (#:j #:transducers/jzon)
                    (#:s #:transducers/fset)))

(in-package :transducers/tests)

(define-test reduction)

(define-test "Collecting"
  :parent reduction
  (is equal '() (t:transduce #'t:pass #'t:cons '()))
  (is equalp #() (t:transduce #'t:pass #'t:vector #()))
  (is equal "hello" (t:transduce #'t:pass #'t:string "hello")))

(define-test "Counting"
  :parent reduction
  (is = 0 (t:transduce #'t:pass #'t:count '()))
  (is = 3 (t:transduce #'t:pass #'t:count '(1 2 3)))
  (is = 0 (t:transduce #'t:pass #'t:count #()))
  (is = 3 (t:transduce #'t:pass #'t:count #(1 2 3)))
  (let ((ht (make-hash-table :test #'eql)))
    (setf (gethash 1 ht) 4)
    (setf (gethash 2 ht) 2)
    (setf (gethash 3 ht) 2)
    (setf (gethash 4 ht) 2)
    (setf (gethash 5 ht) 1)
    (is equalp ht (t:transduce #'t:pass (t:quantities #'eql) '(1 1 2 1 3 4 5 4 3 2 1)))))

(define-test "Predicates"
  :parent reduction
  (false (t:transduce #'t:pass (t:anyp #'evenp) '(1 3 5 7 9)))
  (true  (t:transduce #'t:pass (t:anyp #'evenp) '(1 3 5 7 9 2)))
  (true  (t:transduce #'t:pass (t:allp #'oddp) '(1 3 5 7 9)))
  (false (t:transduce #'t:pass (t:allp #'oddp) '(1 3 5 7 9 2))))

(define-test "First and Last"
  :parent reduction
  (is = 7  (t:transduce (t:filter #'oddp) #'t:first '(2 4 6 7 10)))
  (fail    (t:transduce (t:filter #'oddp) #'t:first '(2 4 6 10)))
  (is = 10 (t:transduce #'t:pass #'t:last '(2 4 6 7 10)))
  (fail    (t:transduce #'t:pass #'t:last '())))

(define-test "Folding and Finding"
  :parent reduction
  (fail      (t:transduce #'t:pass (t:fold #'cl:max) '()))
  (is = 1000 (t:transduce #'t:pass (t:fold #'cl:max 0) '(1 2 3 4 1000 5 6)))
  (is = 1000 (t:transduce #'t:pass (t:fold #'cl:max) '(1 2 3 4 1000 5 6)))
  (is = 6    (t:transduce #'t:pass (t:find #'evenp) '(1 3 5 6 9)))
  (is = 1000 (t:transduce #'t:pass (t:find #'evenp :default 1000) '(1 3 5 9)))
  (is = 7/2  (t:transduce #'t:pass #'t:average '(1 2 3 4 5 6)))
  (fail      (t:transduce (t:filter #'evenp) #'t:average '(1 3 5)))
  (is = 4    (t:transduce #'t:pass #'t:median '(1 2 3 4 5 6)))
  (is string-equal "b" (t:transduce #'t:pass #'t:median '("a" "c" "b")))
  (fail      (t:transduce #'t:pass #'t:median '())))

(define-test "Partitions"
  :parent reduction
  (is-values (t:transduce #'t:pass (t:partition #'evenp) '(1 2 3 4 5))
    (equal '(2 4))
    (equal '(1 3 5)))
  (is-values (t:transduce #'t:pass (t:partition #'evenp) #(1 2 3 4 5))
    (equal '(2 4))
    (equal '(1 3 5)))
  (is-values (t:transduce #'t:pass (t:partition (lambda (c) (eql c #\l))) "hello")
    (equal '(#\l #\l))
    (equal '(#\h #\e #\o)))
  (is-values (t:transduce #'t:pass (t:partition #'evenp) '())
    (equal '())
    (equal '())))

(define-test transduction)

(define-test "Mapping"
  :parent transduction
  :depends-on (reduction)
  (is equal '() (t:transduce (t:map #'1+) #'t:cons '()))
  (is equal '(2 3 4) (t:transduce (t:map #'1+) #'t:cons '(1 2 3))))

(define-test "Filtering"
  :parent transduction
  :depends-on (reduction)
  (is equal '(2 4) (t:transduce (t:filter #'evenp) #'t:cons '(1 2 3 4 5)))
  (is equal '(2 5 8) (t:transduce (t:filter-map #'cl:first) #'t:cons '(() (2 3) () (5 6) () (8 9))))
  (is equal '(1 2 3 "abc") (t:transduce #'t:unique #'t:cons '(1 2 1 3 2 1 2 "abc")))
  (is equal '((:a . 1) (:b . 2) (:d . 3))
      (t:transduce (t:unique-by #'cdr) #'t:cons '((:a . 1) (:b . 2) (:c . 1) (:d . 3))))
  (is equal '(1 2 3 4 3)
      (t:transduce #'t:dedup #'t:cons '(1 1 1 2 2 2 3 3 3 4 3 3))))

(define-test "Taking and Dropping"
  :parent transduction
  :depends-on (reduction)
  (is equal '() (t:transduce (t:drop 100) #'t:cons '(1 2 3 4 5)))
  (is equal '(4 5) (t:transduce (t:drop 3) #'t:cons '(1 2 3 4 5)))
  (is equal '(7 8 9) (t:transduce (t:drop-while #'evenp) #'t:cons '(2 4 6 7 8 9)))
  (is equal '() (t:transduce (t:take 0) #'t:cons '(1 2 3 4 5)))
  (is equal '(1 2 3) (t:transduce (t:take 3) #'t:cons '(1 2 3 4 5)))
  (is equal '() (t:transduce (t:take-while #'evenp) #'t:cons '(1)))
  (is equal '(2 4 6 8) (t:transduce (t:take-while #'evenp) #'t:cons '(2 4 6 8 9 2))))

(define-test "Flattening"
  :parent transduction
  :depends-on (reduction)
  (is equal '(1 2 3 4 5 6 7 8 9)
      (t:transduce #'t:concatenate #'t:cons '((1 2 3) (4 5 6) (7 8 9))))
  (is equal '(1 2 3 0 4 5 6 0 7 8 9 0)
      (t:transduce #'t:flatten #'t:cons '((1 2 3) 0 (4 (5) 6) 0 (7 8 9) 0)))
  (is equal '(:a 1 :b 2 :c 3)
      (t:transduce #'t:uncons #'t:cons (t:plist '(:a 1 :b 2 :c 3))))
  (is equal '(:a 2 :b 3 :c 4)
      (t:transduce (t:comp (t:map (lambda (pair) (cons (car pair) (1+ (cdr pair)))))
                           #'t:uncons)
                   #'t:cons (t:plist '(:a 1 :b 2 :c 3)))))

(define-test "Pairing"
  :parent transduction
  :depends-on (reduction)
  (is equal '((1 2 3) (4 5)) (t:transduce (t:segment 3) #'t:cons '(1 2 3 4 5)))
  (is equal '((1 2 3) (2 3 4) (3 4 5))
      (t:transduce (t:window 3) #'t:cons '(1 2 3 4 5)))
  (is equal '((2 4 6) (7 9 1) (2 4 6) (3))
      (t:transduce (t:group-by #'evenp) #'t:cons '(2 4 6 7 9 1 2 4 6 3))))

(define-test "Hash Tables"
  :parent transduction
  :depends-on (reduction)
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash :a table) 1)
    (setf (gethash :b table) 2)
    (setf (gethash :c table) 3)
    (is equal '(1 2 3)    (sort (t:transduce (t:map #'cdr) #'t:cons table) #'<))
    (is equal '(:a :b :c) (sort (t:transduce (t:map #'car) #'t:cons table) #'string<))))

(define-test "S-Expressions"
  :parent transduction
  :depends-on (reduction)
  (fail (t:transduce #'t:sexp #'t:cons "(+ 1 1))"))
  (is equal '() (t:transduce #'t:sexp #'t:cons ""))
  (is equal '("(+ 1 1)") (t:transduce #'t:sexp #'t:cons "(+ 1 1)"))
  (is equal '("(+ 1 1)" "(+ 2 2)") (t:transduce #'t:sexp #'t:cons "(+ 1 1) (+ 2 2)"))
  (is equal '("(+ 1 (* 1 1))") (t:transduce #'t:sexp #'t:cons "(+ 1 (* 1 1))"))
  (is equal '("(+ 1 1)" "(+ 2 2)") (t:transduce #'t:sexp #'t:cons '("(+ 1 " "1) (+ 2 2)"))))

(define-test "Other"
  :parent transduction
  :depends-on (reduction)
  (is equal '(1 3 5 7 9)
      (t:transduce (t:step 2) #'t:cons '(1 2 3 4 5 6 7 8 9)))
  (is equal '(1 0 2 0 3) (t:transduce (t:intersperse 0) #'t:cons '(1 2 3)))
  (is equal (list (cons 0 "a") (cons 1 "b") (cons 2 "c"))
      (t:transduce #'t:enumerate #'t:cons '("a" "b" "c")))
  (is equal '(0 1 3 6 10)
      (t:transduce (t:scan #'+ 0) #'t:cons '(1 2 3 4)))
  (is equal '(0 1)
      (t:transduce (t:comp (t:scan #'+ 0) (t:take 2))
                   #'t:cons '(1 2 3 4)))
  (is equal '(hi 11 12)
      (t:transduce (t:comp (t:filter (lambda (n) (> n 10)))
                           (t:once 'hi)
                           (t:take 3))
                   #'t:cons (t:ints 1)))
  (is equal '(nil 0) (t:transduce (t:once nil) #'t:cons '(0)))
  (is equal '(nil) (t:transduce (t:once nil) #'t:cons '())))

(define-test "Composition"
  :parent transduction
  :depends-on (reduction
               "Taking and Dropping"
               "Filtering"
               "Other")
  (is equal '(12 20 30)
      (t:transduce (t:comp
                    #'t:enumerate
                    (t:map (lambda (pair) (* (car pair) (cdr pair))))
                    (t:filter #'evenp)
                    (t:drop 3)
                    (t:take 3))
                   #'t:cons
                   '(1 2 3 4 5 6 7 8 9 10))))

#+nil
(t:transduce (t:comp (t:drop 3) (t:take 2)) #'t:cons '(1 2 3 4 5 6))
#+nil
(funcall (t:comp #'1+ (lambda (n) (* 2 n))) 7)

(define-test "Conditions"
  :parent transduction
  :depends-on (reduction)
  (is equal '(0 2 3)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (item) (if (= 1 item) (error "無念") item)))) #'t:cons '(0 1 2 3))))
  (is equal '(0 2 3)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (item) (if (= 1 item) (error "無念") item)))) #'t:cons #(0 1 2 3))))
  (is string-equal "Bithday"
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (item) (if (eq #\r item) (error "無念") item)))) #'t:string "Birthday")))
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash :a table) 1)
    (setf (gethash :b table) 2)
    (setf (gethash :c table) 3)
    (is equal '(1 3)
        (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
          (sort (t:transduce (t:safe (t:map (lambda (item) (if (= 2 (cdr item)) (error "無念") (cdr item))))) #'t:cons table)
                #'<))))
  (is = 4
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (line) (if (string= "" (string-trim '(#\space) line)) (error "Empty!") line)))) #'t:count #p"tests/file.txt")))
  (is equal '(0 2 3)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:comp (t:take 4)
                             (t:safe (t:map (lambda (item) (if (= 1 item) (error "無念") item)))))
                     #'t:cons (t:ints 0))))
  (is equal '(0 2 3 4)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:comp (t:safe (t:map (lambda (item) (if (= 1 item) (error "無念") item))))
                             (t:take 4))
                     #'t:cons (t:ints 0))))
  (is equal '(0 2 3)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (pair) (if (= 1 (cdr pair)) (error "無念") (cdr pair))))) #'t:cons (t:plist '(:a 0 :b 1 :c 2 :d 3)))))
  (is equal '(0 2 3)
      (handler-bind ((error #'(lambda (c) (declare (ignore c)) (invoke-restart 't:next-item))))
        (t:transduce (t:safe (t:map (lambda (item) (if (= 1 item) (error "無念") item)))) #'t:cons (j:read "[0,1,2,3]")))))

(define-test "Sources"
  :depends-on (reduction transduction)
  (is equal '() (t:transduce (t:take 0) #'t:cons (t:ints 0)))
  (is equal '(0 1 2 3) (t:transduce (t:take 4) #'t:cons (t:ints 0)))
  (is equal '(0 -1 -2 -3) (t:transduce (t:take 4) #'t:cons (t:ints 0 :step -1)))
  (is equal '(1 2 3 1 2 3 1) (t:transduce (t:take 7) #'t:cons (t:cycle '(1 2 3))))
  (is equal '(1 2 3 1 2 3 1) (t:transduce (t:take 7) #'t:cons (t:cycle #(1 2 3))))
  (is equal "hellohe" (t:transduce (t:take 7) #'t:string (t:cycle "hello")))
  (is equal '() (t:transduce (t:take 7) #'t:cons (t:cycle '())))
  (is equal (list (cons :a 1) (cons :b 2) (cons :c 3))
      (t:transduce #'t:pass #'t:cons (t:plist '(:a 1 :b 2 :c 3))))
  (fail (t:transduce #'t:pass #'t:cons 1))
  (is equal (list 3 2 1) (t:transduce #'t:pass #'t:cons (t:reversed #(1 2 3))))
  (is equal nil (t:transduce #'t:pass #'t:cons (t:reversed #())))
  (is equal #*0101 (t:transduce #'t:pass #'t:bit-vector #*0101)))

(define-test "Higher Order Transducers"
  :depends-on (reduction transduction)
  (is equal '(1 4 1 4 1)
      (t:transduce (t:comp (t:take 5)
                           (t:map #'1+)
                           (t:branch #'evenp
                                     (t:map (t:comp #'write-to-string #'1+))
                                     (t:map (t:const "Odd!")))
                           (t:map #'length))
                   #'t:cons (t:ints 1)))
  (is equal (cons '(1 2 3 4 5 6 7 8 9) 12)
      (t:transduce (t:comp (t:take 9)
                           (t:map #'1+)
                           (t:split (t:comp (t:filter #'evenp) (t:take 3)) #'+)
                           (t:map #'1-))
                   #'t:cons (t:ints 1))))

#-abcl
(define-test "Tail-recursion"
  :depends-on (reduction transduction)
  (let ((huge (t:transduce (t:take 1000000) #'t:cons (t:ints 1))))
    (is = 1000000 (t:transduce #'t:pass #'t:count huge)))
  #-allegro
  (is = 1000000
      (labels ((recurse (acc)
                 (if (= acc 1000000)
                     acc
                     (recurse (1+ acc)))))
        (recurse 1))))

(define-test "JSON: Reading and Writing"
  (is equal "[{\"name\":\"A\"},{\"name\":\"B\"}]"
      (with-output-to-string (stream)
        (t:transduce #'t:pass (j:write stream) (j:read "[{\"name\": \"A\"}, {\"name\": \"B\"}]"))))
  (is equal "[{\"name\":\"Colin\",\"age\":35},{\"name\":\"Jack\",\"age\":10}]"
      (with-output-to-string (stream)
        (t:transduce #'t:pass (j:write stream) '((:name "Colin" :age 35) (:name "Jack" :age 10))))))

(define-test "CSV: Reading and Writing"
  (is equal '("Name,Age" "Colin,35" "Jack,26")
      (t:transduce (t:comp #'t:from-csv (t:into-csv '("Name" "Age")))
                   #'t:cons '("Name,Age,Hair" "Colin,35,Blond" "Jack,26,Black")))
  (is equal "10,000" (gethash "Money" (t:transduce #'t:from-csv #'t:first '("Name,Money,Age" "Colin,\"10,000\",37")))))

(define-test "Fset: Immutable Collections"
  (let ((set (fset:set 1 2 3 1)))
    (is fset:equal? set (t:transduce #'t:pass #'s:set set)))
  (let ((map (fset:map (:a 1) (:b 2) (:c 3))))
    (is fset:equal? map (t:transduce #'t:pass #'s:map map)))
  (let ((seq (fset:seq 1 2 3)))
    (is fset:equal? seq (t:transduce #'t:pass #'s:seq seq)))
  (let ((bag (fset:bag 1 2 3 1)))
    (is fset:equal? bag (t:transduce #'t:pass #'s:bag bag))))

#+nil
(test 'transducers/tests)
#+nil
(test 'transducers/tests :report 'interactive)

(define-test utilities)

(define-test "String Splitting"
  :parent utilities
  (is equal '() (t::string-split "" :separator #\,))
  (is equal '("Hello") (t::string-split "Hello" :separator #\,))
  (is equal '("Hello" "there") (t::string-split "Hello,there" :separator #\,))
  (is equal '("" "Hello" "there") (t::string-split ",Hello,there" :separator #\,))
  (is equal '("" "Hello" "there" "") (t::string-split ",Hello,there," :separator #\,))
  (is equal '("" "") (t::string-split "," :separator #\,))
  (is equal '("" "" "") (t::string-split ",," :separator #\,))
  (is equal '("a" "bee") (t::string-split "a,bee" :separator #\,)))
