;;; quickcheck-generics-tests.el --- quickcheck: Generics tests  -*- lexical-binding: t; -*-  
        (require 'seq)
        (require 'cl-lib)
        (require 'dash)
        (require 'quickcheck)

(ert-deftest-n-times sum-success 100
  (let* ((test-value (random))
	 (actual-wrapped-value (Sum test-value)))
    (should (equal (car actual-wrapped-value) 'sum))
    (should (equal (get-sum actual-wrapped-value) test-value))))

(ert-deftest-n-times sum-error 100
  (let* ((test-value (generate-test-string)))
    (should-error (Sum "1"))))

(ert-deftest-n-times all-success 100
  (let* ((test-value (random-boolean))
	 (actual-wrapped-value (All test-value)))
    (should (equal (car actual-wrapped-value) 'all))
    (should (equal (get-all actual-wrapped-value) test-value))))

(ert-deftest-n-times all-error 100
  (let* ((test-value (random)))
    (should-error (actual-wrapped-value (All test-value)))))

(ert-deftest-n-times plist-success 100
  (let* ((test-value (generate-random-unwrapped-plist))
	 (actual-wrapped-value (Plist test-value)))
    (should (equal (car actual-wrapped-value) 'plist))
    (should (equal (get-plist actual-wrapped-value) test-value))))

(ert-deftest-n-times plist-error 100
  (let* ((test-value (random)))
    (should-error (Plist test-value))))

(ert-deftest-n-times wrap-maybe-just 100
  (let* ((test-value (generate-random-value))
	 (actual-maybe (wrap-maybe test-value)))
    (should (equal (car actual-maybe) 'maybe))
    (should (equal (caadr actual-maybe) 'just))))

(ert-deftest wrap-maybe-nothing ()
  (let* ((actual-maybe (wrap-maybe 'nil)))
    (should (equal (car actual-maybe) 'maybe))
    (should (equal (caadr actual-maybe) 'nothing))))

(ert-deftest-n-times get-maybe-just 100
  (-let* (((test-maybe actual-pred) (generate-one-random-maybe-just))
	  ((actual-car actual-cdr) (get-maybe test-maybe)))
    (should (equal actual-car 'just))
    (should (functionp actual-pred))))

(ert-deftest get-maybe-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (equal (get-maybe test-maybe) (list 'nothing)))))

(ert-deftest-n-times from-just-just 100
  (-let* (((test-maybe _ expected-type-pred org) (generate-one-random-maybe-just)))
    (should (funcall expected-type-pred (from-just test-maybe)))))

(ert-deftest from-just-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should-error (from-just test-maybe))))

(ert-deftest-n-times from-maybe-just 100
  (-let* (((test-maybe _ expected-type-pred) (generate-one-random-maybe-just))
	 (test-default (generate-random-value))
	 (actual-value (from-maybe test-default test-maybe)))
    (should (funcall expected-type-pred actual-value))))

(ert-deftest-n-times from-maybe-nothing 100
  (let* ((test-maybe (create-maybe-nothing))
	 (test-default (generate-random-value)))
    (should (equal (from-maybe test-default test-maybe) test-default))))

(ert-deftest-n-times is-just-just 100
  (-let (((test-maybe _) (generate-one-random-maybe-just)))
    (should (is-just test-maybe))))

(ert-deftest is-just-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should-not (is-just test-maybe))))

(ert-deftest-n-times is-nothing-just 100
  (-let (((test-maybe _) (generate-one-random-maybe-just)))
    (should-not (is-nothing test-maybe))))

(ert-deftest is-nothing-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (is-nothing test-maybe))))

(ert-deftest-n-times maybe-to-list-just 100
    (-let* (((test-maybe _ expected-type-pred) (generate-one-random-maybe-just))
	    (actual-value (maybe-to-list test-maybe)))
    (should (funcall (-compose expected-type-pred #'seq-first) actual-value))))

(ert-deftest maybe-to-list-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (equal (maybe-to-list test-maybe) '()))))

(ert-deftest-n-times list-to-maybe-just 100
  (-let* (((test-list) (generate-one-random-list))
	 (actual-maybe (list-to-maybe test-list)))
    (should (maybep actual-maybe))
    (should (is-just actual-maybe))))

(ert-deftest list-to-maybe-nothing ()
  (let ((actual-maybe (list-to-maybe '())))
    (should (maybep actual-maybe))
    (should (is-nothing actual-maybe))))

(ert-deftest-n-times map-maybes 100
  (-let* (((test-list _ __ expected-list) (generate-one-random-list-of-maybes))
	 ((actual-list actual-random-value) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) (-partial #'map-maybes #'cl-constantly)) test-list)))
    (should (length= actual-list (seq-length expected-list)))
    (should (member (funcall actual-random-value) expected-list))))

(ert-deftest-n-times cat-maybes 100
  (-let* (((test-list _ __ expected-list) (generate-one-random-list-of-maybes))
	 (actual-list (cat-maybes test-list)))
    (should (seq-equal actual-list expected-list))))

(ert-deftest-n-times apply-maybe-just 100
  (-let* (((test-maybe _ expected-type-pred) (generate-one-random-maybe-just))
	  (test-default (generate-random-value))
	  (actual-value (apply-maybe test-default #'cl-constantly test-maybe)))
    (should (funcall expected-type-pred (funcall actual-value)))))

(ert-deftest apply-maybe-nothing ()
  (let* ((test-maybe (create-maybe-nothing))
	 (test-default (generate-random-value))
	 (actual-value (apply-maybe test-default #'cl-constantly test-maybe)))
    (should (equal actual-value test-default))))

(ert-deftest-n-times semigroup-concat-lists 100
  (-let* ((((test-list-one test-list-two)) (generate-one-random-list-type-twice))
	  (actual-list (semigroup-concat test-list-one test-list-two)))
    (should (listp actual-list))
    (should (equal (seq-intersection test-list-one actual-list) test-list-one))
    (should (equal (seq-intersection test-list-two actual-list) test-list-two))))

(ert-deftest-n-times semigroup-concat-strings 100
    (-let* ((((test-string-one test-string-two)) (generate-one-random-string-type-twice))  	    
	    (actual-string (semigroup-concat test-string-one test-string-two)))
      (should (s-starts-with-p test-string-one actual-string))
      (should (s-ends-with-p test-string-two actual-string))))

(ert-deftest-n-times semigroup-concat-vectors 100
  (-let* ((((test-vector-one test-vector-two)) (generate-one-random-vector-type-twice))
	  (actual-vector (semigroup-concat test-vector-one test-vector-two)))
    (should (vectorp actual-vector))
    (should (seq-set-equal-p (seq-intersection test-vector-one actual-vector) test-vector-one))
    (should (seq-set-equal-p (seq-intersection test-vector-two actual-vector) test-vector-two))))

(ert-deftest-n-times semigroup-concat-hash-table 100
  (-let* ((((test-hash-table-one test-hash-table-two)) (generate-one-random-hash-table-type-twice))
	  ((expected-key-one expected-key-two) (seq-map #'map-one-random-key (list test-hash-table-one test-hash-table-two)))
	  (actual-hash-table (semigroup-concat test-hash-table-one test-hash-table-two)))
    (should (hash-table-p actual-hash-table))
    (should (map-elt actual-hash-table expected-key-one))
    (should (map-elt actual-hash-table expected-key-two))))

(ert-deftest-n-times semigroup-concat-alist 100
  (-let* ((((test-alist-one test-alist-two) expected-type-pred) (generate-one-random-alist-type-twice))
	  ((expected-key-one expected-key-two) (seq-map #'map-one-random-key (list test-alist-one test-alist-two)))
	  (actual-alist (semigroup-concat test-alist-one test-alist-two)))
    (should (funcall expected-type-pred actual-alist))
    (should (map-elt actual-alist expected-key-one))
    (should (map-elt actual-alist expected-key-two))))

(ert-deftest-n-times semigroup-concat-wrapped-plist 100
  (-let* ((((test-wrapped-plist-one test-wrapped-plist-two) expected-type-pred) (generate-one-random-wrapped-plist-type-twice))
	  ((expected-key-one expected-key-two) (seq-map #'map-one-random-key (list test-wrapped-plist-one test-wrapped-plist-two)))
	  (actual-wrapped-plist (semigroup-concat test-wrapped-plist-one test-wrapped-plist-two)))
    (should (funcall expected-type-pred actual-wrapped-plist))
    (should (map-elt actual-wrapped-plist expected-key-one))
    (should (map-elt actual-wrapped-plist expected-key-two))))

(ert-deftest-n-times semigroup-concat-wrapped-integer 100
  (-let* ((((test-wrapped-integer-one test-wrapped-integer-two) expected-type-pred) (generate-one-random-wrapped-integer-type-twice))
	  (actual-result (semigroup-concat test-wrapped-integer-one test-wrapped-integer-two)))
    (should (funcall expected-type-pred actual-result))))

(ert-deftest-n-times semigroup-concat-error-for-numbers 100
   (-let (((test-nat-one test-nat-two) (generate-test-list-of-nat-numbers :min-length 2 :max-length 2)))
    (should-error (semigroup-concat test-nat-one test-nat-two))))

(ert-deftest-n-times semigroup-concat-wrapped-const 100
  (-let* ((((test-const-one test-const-two) expected-wrapper-pred expected-wrapped-type-pred) (generate-one-random-wrapped-const-type-twice))
	  (actual-const (semigroup-concat test-const-one test-const-two)))
    (should (funcall expected-wrapper-pred actual-const))
    (should (funcall (-compose expected-wrapped-type-pred #'get-const) actual-const))))

(ert-deftest-n-times semigroup-concat-wrapped-boolean 100
  (-let* ((((first-boolean second-boolean) expected-type-pred) (generate-one-random-wrapped-boolean-type-twice))
	  (actual-boolean (semigroup-concat first-boolean second-boolean)))
    (should (funcall expected-type-pred actual-boolean))))

(ert-deftest-n-times semigroup-concat-maybe-test-one 100
  (-let* ((((first-maybe second-maybe) expected-wrapper-pred expected-wrapped-type-pred) (generate-one-random-wrapped-maybe-type-twice))
	  (actual-maybe (semigroup-concat first-maybe second-maybe)))
    (should (funcall expected-wrapper-pred actual-maybe))
    (should (funcall (-compose expected-wrapped-type-pred #'unwrap-maybe-just) actual-maybe))))

(ert-deftest-n-times semigroup-concat-maybe-test-two 100
  (-let* (((test-maybe) (generate-one-random-maybe-just))
	  (test-nothing (create-maybe-nothing)))
    (should (equal (semigroup-concat test-maybe test-nothing) test-maybe))))

(ert-deftest-n-times semigroup-concat-maybe-test-three 100
  (-let* (((test-maybe) (generate-one-random-maybe-just))
	  (test-nothing (create-maybe-nothing)))
    (should (equal (semigroup-concat test-nothing test-maybe) test-maybe))))

(ert-deftest-n-times semigroup-concat-maybe-test-four 100
  (-let (((test-nothing-a test-nothing-b) (-times-no-args 2 #'create-maybe-nothing)))
    (should (is-nothing (semigroup-concat test-nothing-a test-nothing-b)))))

(ert-deftest-n-times stimes-idempotent-greater-than-zero 100
  (let* ((test-value (generate-random-value))
	 (test-n (random-nat-number-in-range-255)))
    (should (equal (stimes-idempotent test-n test-value) test-value))))

(ert-deftest-n-times stimes-idempotent-zero 100
  (let* ((test-value (generate-random-value))
	 (test-n (random-number-less-than-or-equal-zero)))
    (should-error (stimes-idempotent test-n test-value))))

(ert-deftest-n-times stimes-list 100
    (-let* (((test-list) (generate-one-random-list))
	    (test-n (random-nat-number-in-range-50))
	    (expected-length (funcall (-compose (-partial #'* test-n) #'seq-length) test-list))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'stimes) test-n test-list)))
      (should (listp actual-result))
      (should (eql actual-result-length expected-length))))

(ert-deftest-n-times stimes-string 100
  (-let* (((test-string) (generate-one-random-string)) 
	    (test-n (random-nat-number-in-range-50))
	    (expected-length (funcall (-compose (-partial #'* test-n) #'seq-length) test-string))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'stimes) test-n test-string)))
      (should (stringp actual-result))
      (should (eql actual-result-length expected-length))))

(ert-deftest-n-times stimes-vector 100
      (-let* (((test-vector) (generate-one-random-vector)) 
	    (test-n (random-nat-number-in-range-50))
	    (expected-length (funcall (-compose (-partial #'* test-n) #'seq-length) test-vector))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'stimes) test-n test-vector)))
      (should (vectorp actual-result))
      (should (eql actual-result-length expected-length))))

(ert-deftest-n-times stimes-alist 100
    (-let* (((test-alist _) (generate-one-random-alist))
	    (test-n (random-nat-number-in-range-50))
	    (actual-result (stimes test-n test-alist)))
    (should (eql actual-result test-alist))))

(ert-deftest-n-times stimes-hash-table 100
    (-let* (((test-hash-table _) (generate-one-random-hash-table))
	    (test-n (random-nat-number-in-range-255)) 
	    (actual-result (stimes test-n test-hash-table)))
    (should (equal actual-result test-hash-table))))

(ert-deftest-n-times stimes-wrapped-integer 100
  (-let* (((test-wrapped-integer expected-type-pred) (generate-one-random-wrapped-integer))
	  (test-n (random-nat-number-in-range-255))
	  (actual-result (stimes test-n test-wrapped-integer)))
    (should (funcall expected-type-pred actual-result))))

(ert-deftest-n-times stimes-wrapped-const 100
  (-let* (((test-wrapped-const expected-wrapper-type-pred expected-wrapped-type-pred) (generate-one-random-wrapped-const))
	  (test-n (random-nat-number-in-range-255))
	  (actual-const (stimes test-n test-wrapped-const)))
    (should (funcall expected-wrapper-type-pred actual-const))
    (should (funcall (-compose expected-wrapped-type-pred #'get-const) actual-const))))

(ert-deftest-n-times stimes-wrapped-boolean 100
  (-let* (((test-wrapped-boolean expected-type-pred) (generate-one-random-wrapped-boolean))
	  (test-n (random-nat-number-in-range-50))
	  (actual-wrapped-boolean (stimes test-n test-wrapped-boolean)))
    (should (funcall expected-type-pred test-wrapped-boolean))))

(ert-deftest-n-times stimes-maybe-test-just-negative 100
  (-let* (((test-maybe) (generate-one-random-maybe-just))
	  (test-n (random-negative-number)))  	  
    (should-error (stimes test-n test-maybe))))

(ert-deftest-n-times stimes-maybe-test-just-zero 100
  (-let* (((test-maybe) (generate-one-random-maybe-just))
	  (actual-value (stimes 0 test-maybe)))
    (should (is-nothing actual-value))))

(ert-deftest-n-times stimes-maybe-test-just-positive 100
  (-let* (((test-maybe expected-wrapper-type-pred expected-wrapped-type-pred) (generate-one-random-maybe-just))
	  (test-n (random-nat-number-in-range-50))
	  (actual-value (stimes test-n test-maybe)))
    (should (funcall expected-wrapper-type-pred actual-value))
    (should (funcall (-compose expected-wrapped-type-pred #'unwrap-maybe-just) actual-value))))

(ert-deftest-n-times stimes-maybe-test-nothing 100
  (let ((test-maybe (create-maybe-nothing))
	(test-n (random-nat-number-in-range-255)))
    (should (is-nothing (stimes test-n test-maybe)))))

(ert-deftest-n-times mempty-wrapped-integer 0
  (-let* (((test-wrapped-integer _) (generate-one-random-wrapped-monoidal-integer-type))
	  (expected-type-pred (get-wrapped-integer-type-pred test-wrapped-integer))
	  (actual-result (semigroup-concat (mempty test-wrapped-integer) test-list)))
    (should (funcall expected-type-pred actual-result))))

(ert-deftest-n-times mconcat-lists-of-strings 100
  (let* ((test-list (generate-test-list-of-strings))
	 (expected-length (seq-sum-seq-lengths test-list))
	  (actual-result (mconcat test-list)))
    (should (equal (cl-type-of actual-result) 'string))
    (should (length= actual-result expected-length))))

(ert-deftest-n-times mconcat-lists-of-unwrapped-integers 100
  (let* ((test-list (generate-test-list-of-nat-numbers)))  	  
    (should-error (mconcat test-list))))

(ert-deftest-n-times mconcat-lists-of-wrapped-sums 0
  (-let* (((test-list) (generate-one-random-sum-type-n-random-times))
	  (actual-result (mconcat test-list)))      
    (should (homogenic-list-p test-list))))

(ert-deftest-n-times mconcat-lists-of-wrapped-integers 0
  (-let* (((test-list expected-type-pred) (generate-one-random-wrapped-integer-type-n-random-times))
	  (actual-result (mconcat test-list)))
    (should (funcall expected-type-pred actual-result))))

(ert-deftest-n-times mconcat-vectors 0
  (-let* (((test-vector) (generate-one-random-vector))
	  (expected-type (seq-subtype test-vector))
	  (actual-result (mconcat test-vector)))      
    (should (equal (cl-type-of actual-result) expected-type))))

(ert-deftest-n-times fmap-for-list 0
  (let* ((test-list (generate-test-data))
	 (test-list-length (seq-length test-list))
	 (actual-list (fmap #'1+ test-list)))
    (should (listp actual-list))
    (should (eql (-sum actual-list) (+ (-sum test-list) test-list-length)))))

(ert-deftest-n-times fmap-for-vectors 0
  (let* ((test-vector (generate-test-vector-of-nat-numbers))
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
  (let* ((test-vector (generate-test-vector-of-nat-numbers))
	 (expected-vector-length (seq-length test-vector))
	 (test-constant (math-random-base))
	 (actual-vector (<$ test-constant test-vector)))
     (should (vectorp actual-vector))
    (should (eql (seq-count (apply-partially eql test-constant)) expected-list-length))))

(ert-deftest-n-times fmap-constantly-for-string 0
  (let* ((test-string (generate-test-string-of-nat-numbers))
	 (expected-string-length (seq-length test-string))
	 (test-constant (math-random-base))
	 (actual-string (<$ test-constant test-string)))
     (should (stringp actual-string))
    (should (eql (seq-count (apply-partially eql test-constant)) expected-list-length))))

(provide 'quickcheck-generics-tests)
;;; quickcheck-generics-tests.el ends here
