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

(ert-deftest-n-times sum-error 0
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
  (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just))
	  ((actual-car actual-cdr) (get-maybe test-maybe)))
    (should (equal actual-car 'just))
    (should (equal (cl-type-of actual-cdr) expected-type))))

(ert-deftest get-maybe-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (equal (get-maybe test-maybe) (list 'nothing)))))

(ert-deftest-n-times from-just-just 100
  (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just)))
    (should (equal (cl-type-of (from-just test-maybe)) expected-type))))

(ert-deftest from-just-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should-error (from-just test-maybe))))

(ert-deftest-n-times from-maybe-just 100
  (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just))
	 (test-default (generate-random-value))
	 (actual-value (from-maybe test-default test-maybe)))
    (should (equal (cl-type-of actual-value) expected-type))))

(ert-deftest-n-times from-maybe-nothing 100
  (let* ((test-maybe (create-maybe-nothing))
	 (test-default (generate-random-value)))
    (should (equal (from-maybe test-default test-maybe) test-default))))

(ert-deftest-n-times is-just-just 100
  (-let ((((test-maybe) _) (generate-one-random-maybe-just)))
	 (should (is-just test-maybe))))

(ert-deftest is-just-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should-not (is-just test-maybe))))

(ert-deftest-n-times is-nothing-just 100
  (-let ((((test-maybe) _) (generate-one-random-maybe-just)))
    (should-not (is-nothing test-maybe))))

(ert-deftest is-nothing-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (is-nothing test-maybe))))

(ert-deftest-n-times maybe-to-list-just 100
    (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just))
	 (actual-value (maybe-to-list test-maybe)))
    (should (equal (funcall (-compose #'cl-type-of #'car) actual-value) expected-type))))

(ert-deftest maybe-to-list-nothing ()
  (let ((test-maybe (create-maybe-nothing)))
    (should (equal (maybe-to-list test-maybe) '()))))

(ert-deftest-n-times list-to-maybe-just 0
  (-let* (((test-list _) (generate-one-random-list-of-primitives))
	 (actual-maybe (list-to-maybe test-list)))
    (should (maybep actual-maybe))
    (should (is-just actual-maybe))))

;; (ert-deftest list-to-maybe-nothing ()
;;   (let ((actual-maybe (list-to-maybe '())))
;;     (should (maybep actual-maybe))
;;     (should (is-nothing actual-maybe))))

(ert-deftest-n-times map-maybes 100
  (-let* (((test-list expected-list expected-list-length &rest _) (generate-one-random-list-of-maybe-justs-and-nothings))
	 ((actual-list actual-random-value) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) (-partial #'map-maybes #'cl-constantly)) test-list)))
    (should (length= actual-list expected-list-length))
    (should (member (funcall actual-random-value) expected-list))))

(ert-deftest-n-times cat-maybes 100
  (-let* (((test-list expected-list &rest _) (generate-one-random-list-of-maybe-justs-and-nothings))
	 (actual-list (cat-maybes test-list)))
    (should (seq-equal actual-list expected-list))))

(ert-deftest-n-times apply-maybe-just 100
  (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just))
	 (test-default (generate-random-value))
	 (actual-value (apply-maybe test-default #'cl-constantly test-maybe)))
    (should (equal (cl-type-of (funcall actual-value)) expected-type))))

(ert-deftest apply-maybe-nothing ()
  (let* ((test-maybe (create-maybe-nothing))
	 (test-default (generate-random-value))
	 (actual-value (apply-maybe test-default #'cl-constantly test-maybe)))
    (should (equal actual-value test-default))))

(ert-deftest-n-times semigroup-concat-lists 0
    (-let* ((((test-list-one test-list-two) (test-list-one-length test-list-two-length) expected-list-length)
	     (funcall (-compose (-juxt #'identity #'seq-map-seq-length #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-list-of-floats))
	    (((actual-list actual-list-length) actual-list-prefix actual-list-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'take test-list-one-length) (apply-partially #'-take-last test-list-two-length)) #'semigroup-concat) test-list-one test-list-two)))
      (should (equal actual-list-prefix test-list-one))
      (should (equal actual-list-suffix test-list-two))        
      (should (equal actual-list-length expected-list-length))
      (should (listp actual-list))))

(ert-deftest-n-times semigroup-concat-strings 100
    (-let* ((((test-string-one test-string-two) expected-string-length) (funcall (-compose (-juxt #'identity #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-string))
	    (((actual-string actual-string-length) result-startswith-string-one result-startswith-string-two)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'string-prefix-p test-string-one) (apply-partially #'string-suffix-p test-string-two)) #'semigroup-concat) test-string-one test-string-two)))
      (should (and result-startswith-string-one result-startswith-string-two))
      (should (equal actual-string-length expected-string-length))
      (should (stringp actual-string))))

(ert-deftest-n-times semigroup-concat-vectors 100
    (-let* ((((test-vector-one test-vector-two) (test-vector-one-length test-vector-two-length) expected-vector-length)
	     (funcall (-compose (-juxt #'identity #'seq-map-seq-length #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-vector-of-nat-numbers))
	    (((actual-vector actual-vector-length) actual-vector-prefix actual-vector-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (-rpartial #'seq-take test-vector-one-length) (apply-partially #'seq-take-last test-vector-two-length)) #'semigroup-concat) test-vector-one test-vector-two)))
      (should (equal actual-vector-prefix test-vector-one))
      (should (equal actual-vector-suffix test-vector-two))        
      (should (equal actual-vector-length expected-vector-length))
      (should (vectorp actual-vector))))

(ert-deftest-n-times semigroup-concat-hash-table 0
  (-let* ((((test-hash-table-one test-hash-table-two) (expected-key-one expected-key-two)) (funcall (-compose (-juxt #'identity #'seq-map-one-random-map-key) #'call-one-random-hash-table-generator-twice)))
	  (actual-hash-table (semigroup-concat test-hash-table-one test-hash-table-two)))
    (should (hash-table-p actual-hash-table))
    (should (map-elt actual-hash-table expected-key-one))
    (should (map-elt actual-hash-table expected-key-two))))

(ert-deftest-n-times semigroup-concat-alist 100
  (-let* ((((test-alist-one test-alist-two) (expected-key-one expected-key-two)) (funcall (-compose (-juxt #'identity #'seq-map-one-random-map-key) #'call-one-random-alist-generator-twice)))
	  (actual-alist (semigroup-concat test-alist-one test-alist-two)))
    (should (alistp actual-alist))
    (should (map-elt actual-alist expected-key-one))
    (should (map-elt actual-alist expected-key-two))))

(ert-deftest-n-times semigroup-concat-plist 100
  (-let* ((((test-plist-one test-plist-two) (expected-key-one expected-key-two)) (funcall (-compose (-juxt #'identity #'seq-map-one-random-map-key) #'call-one-random-wrapped-plist-generator-twice)))
	  (actual-plist (semigroup-concat test-plist-one test-plist-two)))
    (should (plistp actual-plist))
    (should (map-elt actual-plist expected-key-one))
    (should (map-elt actual-plist expected-key-two))))

(ert-deftest-n-times semigroup-concat-wrapped-integer 100
  (-let* ((((first-wrapped-integer second-wrapped-integer) actual-wrapped-integer-type) (generate-one-random-wrapped-integer-type-twice))
	  ((actual-result &as actual-car actual-cdr) (semigroup-concat first-wrapped-integer second-wrapped-integer)))
    (should (length= actual-result 2))
    (should (equal actual-car actual-wrapped-integer-type))
    (should (numberp actual-cdr))))

(ert-deftest-n-times semigroup-concat-error-for-numbers 100
   (-let (((test-nat-one test-nat-two) (generate-test-list-of-nat-numbers :min-length 2 :max-length 2)))
    (should-error (semigroup-concat test-nat-one test-nat-two))))

(ert-deftest-n-times semigroup-concat-wrapped-const 100
  (-let* ((((first-const second-const) expected-type) (generate-one-random-wrapped-const-type-twice))
	  ((actual-const &as _ actual-unwrapped-val) (semigroup-concat first-const second-const)))
    (should (constp actual-const))
    (should (equal (cl-type-of actual-unwrapped-val) expected-type))))

(ert-deftest-n-times semigroup-concat-wrapped-boolean 100
  (-let* ((((first-wrapped-boolean second-wrapped-boolean) test-wrapped-type-predicate) (generate-one-random-wrapped-boolean-type-twice))
	  ((actual-wrapped-boolean &as _ actual-unwrapped-val) (semigroup-concat first-wrapped-boolean second-wrapped-boolean)))
    (should (funcall test-wrapped-type-predicate actual-wrapped-boolean))
    (should (booleanp actual-unwrapped-val))))

(ert-deftest-n-times semigroup-concat-maybe-test-one 100    
  (-let* ((((first-maybe second-maybe) expected-type) (generate-one-random-wrapped-maybe-type-twice))
	  ((actual-value actual-unwrapped-val) (funcall (-compose (-juxt #'identity #'unwrap-maybe-just) #'semigroup-concat) first-maybe second-maybe)))
    (should (maybep actual-value))
    (should (equal (cl-type-of actual-unwrapped-val) expected-type))))

(ert-deftest-n-times semigroup-concat-maybe-test-two 100
  (-let* ((((test-maybe) _) (generate-one-random-maybe-just))
	  (test-nothing (create-maybe-nothing)))
    (should (equal (semigroup-concat test-maybe test-nothing) test-maybe))))

(ert-deftest-n-times semigroup-concat-maybe-test-three 100
  (-let* ((((test-maybe) _) (generate-one-random-maybe-just))
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
    (-let* (((test-list test-list-length) (funcall (-compose #'identity-and-seq-length #'generate-test-list-of-floats)))
	    (test-n (random-nat-number-in-range-50))
	    (expected-length (* test-n test-list-length))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'stimes) test-n test-list)))
    (should (listp actual-result))
    (should (eql actual-result-length expected-length))))

(ert-deftest-n-times stimes-string 100
    (-let* (((test-string test-string-length) (funcall (-compose #'identity-and-seq-length #'generate-test-string)))
	    ((test-n expected-result-length) (funcall (-compose (-juxt #'identity (apply-partially #'* test-string-length)) #'random-nat-number-in-range-255)))
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'seq-random-chunk-of-size-n test-string-length)) #'stimes) test-n test-string)))
    (should (stringp actual-result))
    (should (eql actual-result-length expected-result-length))
    (should (equal actual-random-chunk test-string))))

(ert-deftest-n-times stimes-vector 100
    (-let* (((test-vector test-vector-length) (funcall (-compose #'identity-and-seq-length #'generate-test-vector-of-nat-numbers)))
	    (test-n (random-nat-number-in-range-50))
	    (expected-length (* test-n test-vector-length))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'stimes) test-n test-vector)))
    (should (vectorp actual-result))
    (should (eql actual-result-length expected-length))))

(ert-deftest-n-times stimes-alist 100
    (-let* (((test-alist test-list-length) (generate-random-alist))
	    (test-n (random-nat-number-in-range-50))
	    (actual-result (stimes test-n test-alist)))
    (should (alistp actual-result))
    (should (eql actual-result test-alist))))

(ert-deftest-n-times stimes-hash-table 0
    (-let* (((test-hash-table _) (generate-one-random-hash-table))
	    (test-n (random-nat-number-in-range-255)) 
	    (actual-result (stimes test-n test-hash-table)))
    (should (equal actual-result test-hash-table))))

(ert-deftest-n-times stimes-wrapped-integer 100
  (-let* (((test-wrapped-integer expected-type) (generate-one-random-wrapped-integer))
	  (test-n (random-nat-number-in-range-255))
	  ((actual-car actual-cdr) (stimes test-n test-wrapped-integer)))
    (should (equal actual-car expected-type))
    (should (numberp actual-cdr))))

(ert-deftest-n-times stimes-wrapped-const 100
  (-let* ((((test-wrapped-const) expected-type) (generate-one-random-wrapped-const))
	  (test-n (random-nat-number-in-range-50))
	  ((actual-const &as _ actual-unwrapped-val) (stimes test-n test-wrapped-const)))
    (should (constp actual-const))
    (should (equal (cl-type-of actual-unwrapped-val) expected-type))))

(ert-deftest-n-times stimes-wrapped-boolean 100
  (-let* ((((test-wrapped-boolean) test-wrapped-type-predicate) (generate-one-random-wrapped-boolean-type))
	  (test-n (random-nat-number-in-range-50))
	  (actual-wrapped-boolean (stimes test-n test-wrapped-boolean)))
    (should (equal actual-wrapped-boolean test-wrapped-boolean))))

(ert-deftest-n-times stimes-maybe-test-just-negative 100
  (-let* ((((test-maybe) _) (generate-one-random-maybe-just))
	  (test-n (random-negative-number)))  	  
    (should-error (stimes test-n test-maybe))))

(ert-deftest-n-times stimes-maybe-test-just-zero 100
  (-let* ((((test-maybe) _) (generate-one-random-maybe-just))
	  (actual-value (stimes 0 test-maybe)))
    (should (is-nothing actual-value))))

(ert-deftest-n-times stimes-maybe-test-just-positive 100
  (-let* ((((test-maybe) expected-type) (generate-one-random-maybe-just))
	  (test-n (random-nat-number-in-range-50))
	  ((actual-value actual-unwrapped-val) (funcall (-compose (-juxt #'identity #'unwrap-maybe-just) #'stimes) test-n test-maybe)))
    (should (maybep actual-value))
    (should (equal (cl-type-of actual-unwrapped-val) expected-type))))

(ert-deftest-n-times stimes-maybe-test-nothing 100
  (let ((test-maybe (create-maybe-nothing))
	(test-n (random-nat-number-in-range-255)))
    (should (is-nothing (stimes test-n test-maybe)))))

(ert-deftest-n-times mconcat-lists 100
  (-let* (((test-list test-subtype) (generate-random-list))
	  (actual-result (mconcat test-list)))      
    (should (equal (cl-type-of actual-result) test-subtype))))

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
