;;; quickcheck.el --- Quickcheck clone       -*- lexical-binding: t; -*-  
      (require 'seq)
      (require 'dash)
      (require 'ert)
      (require 'calc-comb)
      (require 'cl-lib)

(let ((parent-directory (file-name-directory (or load-file-name buffer-file-name))))
  (file-name-concat parent-directory "quickcheck"))
(require 'quickcheck)

(ert-deftest times ()    
  (let* ((test-n (random 255))
	 (test-constant (random))
	 (actual-result (times (cl-constantly test-constant) test-n))
	 (actual-constant-count (funcall #'seq-count (apply-partially #'eql test-constant) actual-result)))      
  (should (eql actual-constant-count test-n))))

(ert-deftest-n-times times-no-args 100
  (let* ((test-n (random 255))
	 (test-constant (random))
	 (actual-result (times-no-args (lambda () test-constant) test-n))
	 (actual-constant-count (funcall #'seq-count (apply-partially #'eql test-constant) actual-result)))      
  (should (eql actual-constant-count test-n))))

(ert-deftest-n-times non-zero-bounded-modular-addition-max-test 100
    (let* ((range-max (random 100000000))
  	 (range-min (- range-max (random range-max) 2))
  	 (increase 1)
  	 (expected-result range-min)
  	 (current-number (1- range-max))
  	 (actual-result (non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			    (should (eql actual-result expected-result))))


  (ert-deftest-n-times non-zero-bounded-modular-addition-min-test 100
    (let* ((range-max (random 10000000))
	 (range-min (- range-max (random range-max) 2))
	 (increase 1)
	 (expected-result (1+ range-min))
	 (current-number range-min)
	 (actual-result (non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			    (should (eql actual-result expected-result))))

(ert-deftest-n-times non-zero-bounded-modular-addition-basic-integer-test 100
  (let* ((range-max (random 10000000))
	 (range-min (- range-max (random range-max) 2))
	 (increase (random range-max))
	 (current-number (random range-max))
	 (actual-result (non-zero-bounded-modular-addition (list range-min range-max) increase current-number)))
			 (should (eql (and (greater-than-or-equal actual-result range-min) (less-than actual-result range-max)) t))))

(ert-deftest-n-times random-float-between-0-and-1 100
  (should (floatp (random-float-between-0-and-1))))

(ert-deftest-n-times scale-float-to-range 100    
    (let* ((test-max (random 10000000))
	   (test-min (- test-max (random test-max) 2))
	   (test-float-to-scale (convert-calc-value-into-lisp (math-random-float)))
	   (actual-float (scale-float-to-range (list test-min test-max) test-float-to-scale)))
      (should (greater-than-or-equal actual-float test-min))
      (should (less-than actual-float test-max))))

(ert-deftest-n-times convert-calc-value-into-lisp 100
  (should (floatp (convert-calc-value-into-lisp (math-gaussian-float)))))

(ert-deftest-n-times shuffle 100    
  (-let* (((actual-shuffled-list test-list) (funcall (-juxt #'shuffle #'identity) (-iterate-plus-one (math-random-base) (calcFunc-random-255))))
	      ((actual-shuffled-list-length test-list-length) (seq-map #'seq-length (list actual-shuffled-list test-list))))
	(should (= actual-shuffled-list-length test-list-length))
	(should-not (seq-difference actual-shuffled-list test-list))))

(ert-deftest-n-times n-random-values-from-array 100
  (-let* (((test-count test-list) (funcall (-compose (-juxt (-compose #'random-integer-in-range (apply-partially #'list 0)  #'seq-length) #'identity)
						   #'random-integer-list-in-range-255)))
	 ((actual-random-values actual-length) (funcall (-compose (-juxt #'identity #'seq-length) #'n-random-values-from-array) test-count test-list)))
    (should (eql actual-length test-count))))

(ert-deftest-n-times random-array-value 100
  (-let* (((actual-value test-array) (funcall (-compose (-juxt #'random-array-value #'identity) #'random-integer-list-in-range-255))))
    (should (memq actual-value test-array))))

(ert-deftest-n-times random-integer-in-range 100    
      (let* ((test-max (random 10000000))
  	   (test-min (- test-max (random test-max) 2))  	   
  	   (actual-integer (random-integer-in-range (list test-min test-max))))
	(should (integerp actual-integer))
        (should (greater-than-or-equal actual-integer test-min))
        (should (less-than actual-integer test-max))))

(ert-deftest-n-times random-integer-list 100
  (-let* (((actual-list expected-list-length) (funcall (-juxt #'random-integer-list #'identity) (calcFunc-random-255))))
  (should (eql (seq-count-integers actual-list ) expected-list-length))))

(ert-deftest-n-times random-integer-range 100
  (-let* (((actual-range expected-range-length) (funcall (-juxt #'random-integer-range #'identity) (random-integer-in-range (list 1 10000)))))
  (should (eql (range-size actual-range) expected-range-length))))

(ert-deftest-n-times divide-array-values-by-max-array-value 100
  (-let* (((actual-list expected-list-length) (funcall (-juxt #'divide-array-values-by-max-array-value #'seq-length)  (random-integer-list-in-range-255))))
    (should (eql (seq-count-between-zero-and-one actual-list) expected-list-length))))

(ert-deftest-n-times divide-by-random-value 100
  (-let* (((actual-result actual-input-value) (funcall (-compose (-juxt #'divide-by-random-value #'identity) #'random-integer-in-range-255))))
    (should (floatp actual-result))
    (should (less-than-or-equal actual-result actual-input-value))))

(ert-deftest-n-times divide-array-values-by-random-array-value 100
  (-let* (((actual-list expected-list-length) (funcall (-juxt #'divide-array-values-by-random-value #'seq-length)  (random-integer-list-in-range-255))))
    (should (eql (seq-count-floats actual-list) expected-list-length))))

(ert-deftest-n-times generate-test-data-for-list-of-integers 100
    (-let* (((actual-integer-count actual-list-length actual-list)
	     (funcall (-compose (-juxt #'seq-count-integers #'seq-length #'identity) #'generate-test-data) :min-length 1 :max-length 255)))
      (should (eql actual-integer-count actual-list-length))
      (should (between-one-and-255 actual-integer-count))))

(ert-deftest-n-times generate-test-data-for-list-of-floats-1 100
  (-let* (((actual-floats-count actual-list-length actual-list)
	     (funcall (-compose (-juxt #'seq-count-floats #'seq-length #'identity) #'generate-test-list-of-floats-between-zero-and-one))))
      (should (eql actual-floats-count actual-list-length))
      (should (between-one-and-255 actual-floats-count))))

(ert-deftest-n-times generate-test-data-for-list-of-floats-2 100
    (-let* (((actual-floats-count test-list-length)
	     (funcall (-compose (-juxt #'seq-count-floats #'seq-length) #'generate-test-list-of-floats))))
      (should (eql actual-floats-count test-list-length))
      (should (between-one-and-255 actual-floats-count))))

(ert-deftest-n-times generate-test-data-for-list-of-strings 100
    (-let* (((actual-strings-count test-list-length)
	     (funcall (-compose (-juxt #'seq-count-strings #'seq-length) #'generate-test-list-of-strings))))
      (should (eql actual-strings-count test-list-length))
      (should (between-one-and-255 actual-strings-count))))

(ert-deftest-n-times generate-test-data-for-single-string 100
  (let* ((actual-string (generate-test-string))
	   (actual-string-length (seq-length actual-string)))
    (should (stringp actual-string))
    (should (between-one-and-255 actual-string-length))))

(ert-deftest-n-times generate-test-data-for-vector-of-integers 100
  (-let* (((actual-integers-count test-vector-length actual-vector)
	     (funcall (-compose (-juxt #'seq-count-integers #'seq-length #'identity) #'generate-test-vector-of-integers))))
    (should (vectorp actual-vector))
    (should (eql actual-integers-count test-vector-length))
    (should (between-one-and-255 actual-integers-count))))

(ert-deftest-n-times generate-test-data-for-alist 100
    (-let* (((actual-cons-count actual-alist-length actual-alist) (funcall (-compose (-juxt #'seq-count-cons #'seq-length #'identity) #'generate-test-alist-of-integers))))
      (should (equal actual-cons-count actual-alist-length))
      (should (between-one-and-255 actual-cons-count))))

(ert-deftest-n-times generate-test-data-for-con-0 100
  (-let (((actual-con actual-car actual-cdr)(funcall (-compose (-juxt #'identity #'car #'cdr) #'generate-test-con-of-integers))))
	 (should (consp actual-con))
	 (should (integerp actual-car))
	 (should (integerp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-con-1 100
  (-let (((actual-con actual-car actual-cdr)(funcall (-compose (-juxt #'identity #'car #'cdr) #'generate-test-con-of-floats))))
	 (should (consp actual-con))
	 (should (floatp actual-car))
	 (should (floatp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-con-2 100
  (-let (((actual-con actual-car actual-cdr)(funcall (-compose (-juxt #'identity #'car #'cdr) #'generate-test-con-of-strings))))
	 (should (consp actual-con))
	 (should (stringp actual-car))
	 (should (stringp actual-cdr))))

(ert-deftest-n-times concat-string 100
    (-let* ((test-string-one test-string-two) (generate-test-string))
      ()))

(ert-deftest-n-times fmap-for-list 0
  (let* ((test-list (generate-test-data))
	 (test-list-length (seq-length test-list))
	 (actual-list (fmap #'1+ test-list)))
    (should (listp actual-list))
    (should (eql (-sum actual-list) (+ (-sum test-list) test-list-length)))))

(ert-deftest-n-times fmap-for-vectors 0
  (let* ((test-vector (generate-test-vector-of-integers))
	 (test-vector-length (seq-length test-vector))
	 (actual-vector (fmap #'1+ test-vector)))
    (should (vectorp actual-vector))
    (should (eql (-sum actual-vector) (+ (-sum test-vector) test-vector-length)))))

(ert-deftest-n-times fmap-constantly-for-list 0
  (let* ((test-list (generate-test-data))
	 (expected-list-length (seq-length test-list))
	 (test-constant (math-random-base))
	 (actual-list (<$ test-constant test-list)))
    (should (listp actual-list))
    (should (eql expected-list-length expected-list-length))))

(ert-deftest-n-times fmap-constantly-for-vector 0
  (let* ((test-vector (generate-test-vector-of-integers))
	 (expected-vector-length (seq-length test-vector))
	 (test-constant (math-random-base))
	 (actual-vector (<$ test-constant test-vector)))
     (should (vectorp actual-vector))
    (should (eql (seq-count (apply-partially eql test-constant)) expected-list-length))))

(ert-deftest-n-times fmap-constantly-for-string 0
  (let* ((test-string (generate-test-string-of-integers))
	 (expected-string-length (seq-length test-string))
	 (test-constant (math-random-base))
	 (actual-string (<$ test-constant test-string)))
     (should (stringp actual-string))
    (should (eql (seq-count (apply-partially eql test-constant)) expected-list-length))))
