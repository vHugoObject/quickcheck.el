;;; quickcheck.el --- Quickcheck clone       -*- lexical-binding: t; -*-            
        (require 'ert)
        (require 'seq)
        (require 'calc-comb)
        (require 'cl-lib)
        (require 'dash)
        (require 's)

  (let ((parent-directory (file-name-directory (or load-file-name buffer-file-name))))
    (file-name-concat parent-directory "quickcheck"))
  (require 'quickcheck)

(ert-deftest-n-times convert-calc-value-into-lisp 100
  (should (floatp (convert-calc-value-into-lisp (math-gaussian-float)))))

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

(ert-deftest-n-times seq-take-right-for-lists 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-list-of-integers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-right) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (cl-subsetp actual-result test-list))))

(ert-deftest-n-times seq-take-right-for-vectors 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-vector-of-integers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-right) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (vectorp actual-result))))

(ert-deftest-n-times seq-take-right-for-strings 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-string)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-right) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (stringp actual-result))))

(ert-deftest-n-times seq-shuffle-list 100
  (-let* (((actual-shuffled-list test-list) (funcall (-compose (-juxt #'seq-shuffle #'identity) #'generate-test-list-of-strings) :min-length 2)))
	(should-not (seq-difference actual-shuffled-list test-list))))

(ert-deftest-n-times seq-shuffle-vector 100
  (-let* (((actual-shuffled-vector test-vector) (funcall (-compose (-juxt #'seq-shuffle #'identity) #'generate-test-vector-of-integers))))
        (should (vectorp actual-shuffled-vector))
	(should-not (seq-difference actual-shuffled-vector test-vector))))

(ert-deftest-n-times seq-shuffle-string 100
  (-let* (((actual-shuffled-string test-string) (funcall (-compose (-juxt #'seq-shuffle #'identity) #'generate-test-string))))
        (should (stringp actual-shuffled-string))
	(should-not (seq-difference actual-shuffled-string test-string))))

(ert-deftest-n-times seq-take-last-for-lists 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-list-of-integers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (cl-subsetp actual-result test-list))))

(ert-deftest-n-times seq-take-last-for-vectors 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-vector-of-integers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (vectorp actual-result))))

(ert-deftest-n-times seq-take-last-for-strings 100
    (-let* (((test-string test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-string)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-string)))
      (should (eql actual-result-length test-chunk-length))
      (should (stringp actual-result))))

(ert-deftest-n-times seq-random-chunk-length 100
    (-let* (((test-chunk-length test-list-length) (funcall (-compose (-juxt #'seq-random-chunk-length #'seq-length) #'generate-test-list-of-integers) :min-length 2)))
      (should (less-than test-chunk-length test-list-length))
      (should (greater-than-or-equal test-chunk-length 1))))

(ert-deftest-n-times seq-n-random-values-list 100
  (-let* (((test-count test-list) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-list-of-strings) :min-length 2))
	  (actual-length (funcall (-compose #'seq-length #'seq-n-random-values) test-count test-list)))
    (should (eql actual-length test-count))))

(ert-deftest-n-times seq-n-random-values-vector 100
  (-let* (((test-count test-vector) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-vector-of-integers)))
	 ((actual-vector actual-length) (funcall (-compose #'identity-and-seq-length #'seq-n-random-values) test-count test-vector)))
    (should (vectorp actual-vector))
    (should (eql actual-length test-count))))

(ert-deftest-n-times seq-n-random-values-string 100
  (-let* (((test-count test-string) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-string)))
	 ((actual-string actual-length) (funcall (-compose #'identity-and-seq-length #'seq-n-random-values) test-count test-string)))
    (should (stringp actual-string))
    (should (eql actual-length test-count))))

(ert-deftest-n-times seq-random-values-lists 100
  (-let* ((((actual-list actual-list-length) (test-list test-list-length))
	  (funcall (-compose (apply-partially #'seq-map #'identity-and-seq-length) (-juxt #'seq-random-values #'identity) #'generate-test-list-of-strings))))      
    (should (listp actual-list))
    (should (less-than-or-equal actual-list-length test-list-length))))

(ert-deftest-n-times seq-random-values-vectors 100
  (-let* ((((actual-vector actual-vector-length) (test-vector test-vector-length))
	  (funcall (-compose (apply-partially #'seq-map #'identity-and-seq-length) (-juxt #'seq-random-values #'identity) #'generate-test-vector-of-integers))))      
    (should (vectorp actual-vector))
    (should (less-than-or-equal actual-vector-length test-vector-length))))

(ert-deftest-n-times seq-random-values-strings 100
  (-let* ((((actual-string actual-string-length) (test-string test-string-length))
	  (funcall (-compose (apply-partially #'seq-map #'identity-and-seq-length) (-juxt #'seq-random-values #'identity) #'generate-test-string))))      
    (should (stringp actual-string))
    (should (less-than-or-equal actual-string-length test-string-length))))

(ert-deftest-n-times seq-random-iterate-from-max-lists 100
  (-let* (((actual-list test-list-max) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'seq-max) #'generate-test-list-of-floats))))
    (should (seq-every-p (-rpartial #'greater-than-or-equal test-list-max) actual-list))))

(ert-deftest-n-times seq-random-iterate-from-max-vectors 100
  (-let* (((actual-vector test-vector-max) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'seq-max) #'generate-test-vector-of-integers))))
    (should (vectorp actual-vector))
    (should (seq-every-p (-rpartial #'greater-than-or-equal test-vector-max) actual-vector))))

(ert-deftest-n-times seq-random-iterate-from-max-strings 100
  (-let* (((actual-string test-string-max) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'seq-max) #'generate-test-string))))
    (should (stringp actual-string))
    (should (seq-every-p (-rpartial #'greater-than-or-equal test-string-max) actual-string))))

(ert-deftest-n-times seq-subsetp-list-true 100
      (-let* (((test-subset test-list) (funcall (-compose (-juxt #'seq-random-values #'identity) #'generate-test-list-of-strings)))
        (should (seq-subsetp test-subset test-list)))))

(ert-deftest-n-times seq-subsetp-list-false 100
      (-let* (((test-subset test-list) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'identity) #'generate-test-list-of-integers)))
        (should-not (seq-subsetp test-subset test-list)))))

(ert-deftest-n-times seq-subsetp-vector-true 100
      (-let* (((test-subset test-vector) (funcall (-compose (-juxt #'seq-random-values #'identity) #'generate-test-vector-of-integers))))
        (should (seq-subsetp test-subset test-vector))))

(ert-deftest-n-times seq-subsetp-vector-false 100
      (-let* (((test-subset test-vector) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'identity) #'generate-test-vector-of-integers))))
        (should-not (seq-subsetp test-subset test-vector))))

(ert-deftest-n-times seq-subsetp-string-true 100
      (-let* (((test-subset test-string) (funcall (-compose (-juxt #'seq-random-chunk #'identity) #'generate-test-string))))
	(should (seq-subsetp test-subset test-string))))

(ert-deftest-n-times seq-subsetp-string-false 100
      (-let* (((test-subset test-string) (funcall (-compose (-juxt #'reverse #'identity) #'generate-test-string))))
	(should-not (seq-subsetp test-subset test-string))))

(ert-deftest-n-times seq-random-chunk-of-size-n-string 100
    (-let* (((test-chunk-length test-string) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-string)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'identity-and-seq-length #'seq-random-chunk-of-size-n) test-chunk-length test-string)))
      (should (stringp actual-chunk))
      (should (s-contains? actual-chunk test-string))))

(ert-deftest-n-times seq-random-chunk-of-size-n-list 100
    (-let* (((test-chunk-length test-list) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-list-of-integers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'identity-and-seq-length #'seq-random-chunk-of-size-n) test-chunk-length test-list)))
      (should (listp actual-chunk))
      (should (seq-subsetp actual-chunk test-list))))

(ert-deftest-n-times seq-random-chunk-of-size-n-vector 100
    (-let* (((test-chunk-length test-vector) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-vector-of-integers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'identity-and-seq-length #'seq-random-chunk-of-size-n) test-chunk-length test-vector)))
      (should (vectorp actual-chunk))
      (should (seq-subsetp actual-chunk test-vector))))

(ert-deftest-n-times between-one-and-?-true 100
  (-let* (((test-? test-integer) (funcall (-compose (-juxt #'1+ #'identity) #'random-integer-in-range-255))))
    (should (eq (funcall (between-one-and-? test-?) test-integer) t))))

(ert-deftest-n-times between-one-and-?-false 100
  (-let* (((test-integer test-?) (funcall (-compose (-juxt #'1+ #'identity) #'random-integer-in-range-255))))
    (should (eq (funcall (between-one-and-? test-?) test-integer) 'nil))))

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

(ert-deftest-n-times divide-array-values-by-max-array-value 100
  (-let* (((actual-list expected-list-length) (funcall (-juxt #'divide-array-values-by-max-array-value #'seq-length)  (random-integer-list-in-range-255))))
    (should (eql (seq-count-between-zero-and-one actual-list) expected-list-length))))

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

(ert-deftest-n-times divide-by-random-value 100
  (-let* (((actual-result actual-input-value) (funcall (-compose (-juxt #'divide-by-random-value #'identity) #'random-integer-in-range-255))))
    (should (floatp actual-result))
    (should (less-than-or-equal actual-result actual-input-value))))

(ert-deftest-n-times divide-array-values-by-random-seq-value 100
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

(ert-deftest-n-times semigroup-concat-strings 100
    (-let* ((((test-string-one test-string-two) expected-string-length) (funcall (-compose (-juxt #'identity #'seq-sum-map-length) (apply-partially #'times-no-args #'generate-test-string)) 2))
	    (((actual-string actual-string-length) result-startswith-string-one result-startswith-string-two)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'string-prefix-p test-string-one) (apply-partially #'string-suffix-p test-string-two)) #'semigroup-concat) test-string-one test-string-two)))
      (should (and result-startswith-string-one result-startswith-string-two))
      (should (equal actual-string-length expected-string-length))
      (should (stringp actual-string))))

(ert-deftest-n-times semigroup-concat-lists 100
    (-let* ((((test-list-one test-list-two) (test-list-one-length test-list-two-length) expected-list-length) (funcall (-compose (-juxt #'identity #'seq-map-length #'seq-sum-map-length) (apply-partially #'times-no-args #'generate-test-list-of-floats)) 2))
	    (((actual-list actual-list-length) actual-list-prefix actual-list-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'take test-list-one-length) (apply-partially #'-take-last test-list-two-length)) #'semigroup-concat) test-list-one test-list-two)))
      (should (equal actual-list-prefix test-list-one))
      (should (equal actual-list-suffix test-list-two))        
      (should (equal actual-list-length expected-list-length))
      (should (listp actual-list))))

(ert-deftest-n-times semigroup-concat-vectors 100
    (-let* ((((test-vector-one test-vector-two) (test-vector-one-length test-vector-two-length) expected-vector-length) (funcall (-compose (-juxt #'identity #'seq-map-length #'seq-sum-map-length) (apply-partially #'times-no-args #'generate-test-vector-of-integers)) 2))
	    (((actual-vector actual-vector-length) actual-vector-prefix actual-vector-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (-rpartial #'seq-take test-vector-one-length) (apply-partially #'seq-take-last test-vector-two-length)) #'semigroup-concat) test-vector-one test-vector-two)))
      (should (equal actual-vector-prefix test-vector-one))
      ;;;(should (equal actual-vector-suffix test-vector-two))        
      (should (equal actual-vector-length expected-vector-length))
      (should (vectorp actual-vector))))

(ert-deftest-n-times stimes-string 0
    (-let* ((((test-string test-string-length) test-string-as-list) (funcall (-compose (-juxt #'identity-and-seq-length #'list) #'generate-test-string)))
	    ((test-times expected-string-length) (funcall (-compose (-juxt #'identity (apply-partially #'*)) #'random-integer-in-range-255)))
	    ((actual-result actual-result-as-list) (funcall (-compose (-juxt #'identity (-compose )) #'stimes) test-times test-string)))
    (should (stringp actual-result))
    (should (eql test-string-as-list))))

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
