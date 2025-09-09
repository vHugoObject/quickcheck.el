;;; quickcheck-generics-tests.el --- quickcheck: Generics tests  -*- lexical-binding: t; -*-  
        (require 'seq)
        (require 'cl-lib)
        (require 'dash)
        (require 'quickcheck)

(ert-deftest-n-times sum-success 100
  (let* ((test-value (random))
	 (actual-wrapped-value (Sum test-value)))
    (should (equal (cl-type-of actual-wrapped-value) 'sum))
    (should (equal (get-sum actual-wrapped-value) test-value))))

(ert-deftest-n-times sum-error 0
  (let* ((test-value (generate-test-string)))
    (should-error (Sum "1"))))

(ert-deftest-n-times all-success 100
  (let* ((test-value (random-boolean))
	 (actual-wrapped-value (All test-value)))
    (should (equal (cl-type-of actual-wrapped-value) 'all))
    (should (equal (get-all actual-wrapped-value) test-value))))

(ert-deftest-n-times all-error 100
  (let* ((test-value (random)))
    (should-error (actual-wrapped-value (All test-value)))))

(ert-deftest-n-times plist-success 100
  (let* ((test-value (generate-random-plist))
	 (actual-wrapped-value (Plist test-value)))
    (should (equal (cl-type-of actual-wrapped-value) 'plist))
    (should (equal (get-plist actual-wrapped-value) test-value))))

(ert-deftest-n-times plist-error 100
  (let* ((test-value (random)))
    (should-error (Plist test-value))))

(ert-deftest-n-times semigroup-concat-strings 100
    (-let* ((((test-string-one test-string-two) expected-string-length) (funcall (-compose (-juxt #'identity #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-string))
	    (((actual-string actual-string-length) result-startswith-string-one result-startswith-string-two)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'string-prefix-p test-string-one) (apply-partially #'string-suffix-p test-string-two)) #'semigroup-concat) test-string-one test-string-two)))
      (should (and result-startswith-string-one result-startswith-string-two))
      (should (equal actual-string-length expected-string-length))
      (should (stringp actual-string))))

(ert-deftest-n-times semigroup-concat-lists 0
    (-let* ((((test-list-one test-list-two) (test-list-one-length test-list-two-length) expected-list-length)
	     (funcall (-compose (-juxt #'identity #'seq-map-seq-length #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-list-of-floats))
	    (((actual-list actual-list-length) actual-list-prefix actual-list-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'take test-list-one-length) (apply-partially #'-take-last test-list-two-length)) #'semigroup-concat) test-list-one test-list-two)))
      (should (equal actual-list-prefix test-list-one))
      (should (equal actual-list-suffix test-list-two))        
      (should (equal actual-list-length expected-list-length))
      (should (listp actual-list))))

(ert-deftest-n-times con-of-strings-p-t 100
  (should (con-of-strings-p (generate-test-con-of-strings))))

(ert-deftest-n-times con-of-strings-p-nil-0 100
  (should-not (con-of-strings-p (generate-test-con-of-nat-numbers))))

(ert-deftest-n-times con-of-strings-p-nil-1 100
  (should-not (con-of-strings-p (generate-test-string-nat-number-con))))

(ert-deftest-n-times con-of-strings-p-nil-2 100
  (should-not (con-of-strings-p (generate-test-nat-number-string-con))))

(ert-deftest-n-times semigroup-concat-con-of-strings-succeed 100
  (-let* ((((test-con-1 test-con-2) ((test-car-1 . test-cdr-1) (test-car-2 . test-cdr-2))) (funcall (-compose #'-duplicate #'generate-test-alist-of-strings) :min-length 2))
	  ((actual-con (actual-car . actual-cdr)) (funcall (-compose #'-duplicate #'semigroup-concat) test-con-1 test-con-2)))
    (should (seq-every-p (-rpartial #'seq-subsetp actual-car) (list test-car-1 test-car-2)))
    (should (seq-every-p (-rpartial #'seq-subsetp actual-cdr) (list test-cdr-1 test-cdr-2)))))

(ert-deftest-n-times semigroup-concat-con-of-strings-error 100
  (-let (((test-con-1 test-con-2) (generate-test-alist-of-nat-numbers :min-length 2 :max-length 2)))
    (should-error (semigroup-concat test-con-1 test-con-2))))

(ert-deftest-n-times semigroup-concat-string-vector-con 100
  (-let* ((((test-con-1 test-con-2) ((test-car-1 . test-cdr-1) (test-car-2 . test-cdr-2))) (funcall (-compose #'-duplicate #'generate-test-alist-of-string-vector-of-nat-numbers-cons) :min-length 2))
	  ((actual-con (actual-car . actual-cdr)) (funcall (-compose #'-duplicate #'semigroup-concat) test-con-1 test-con-2)))
    (should (seq-every-p (-rpartial #'seq-subsetp actual-car) (list test-car-1 test-car-2)))
    (should (seq-every-p (-rpartial #'seq-subsetp actual-cdr) (list test-cdr-1 test-cdr-2)))))

(ert-deftest-n-times semigroup-concat-vectors 100
    (-let* ((((test-vector-one test-vector-two) (test-vector-one-length test-vector-two-length) expected-vector-length)
	     (funcall (-compose (-juxt #'identity #'seq-map-seq-length #'seq-sum-seq-lengths) #'-times-no-args-twice) #'generate-test-vector-of-nat-numbers))
	    (((actual-vector actual-vector-length) actual-vector-prefix actual-vector-suffix)
	     (funcall (-compose (-juxt #'identity-and-seq-length (-rpartial #'seq-take test-vector-one-length) (apply-partially #'seq-take-last test-vector-two-length)) #'semigroup-concat) test-vector-one test-vector-two)))
      (should (equal actual-vector-prefix test-vector-one))
      (should (equal actual-vector-suffix test-vector-two))        
      (should (equal actual-vector-length expected-vector-length))
      (should (vectorp actual-vector))))



(ert-deftest-n-times semigroup-concat-hash-table 100
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

(ert-deftest-n-times stimes-string 100
    (-let* (((test-string test-string-length) (funcall (-compose #'identity-and-seq-length #'generate-test-string)))
	    ((test-n expected-result-length) (funcall (-compose (-juxt #'identity (apply-partially #'* test-string-length)) #'random-nat-number-in-range-255)))
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length (apply-partially #'seq-random-chunk-of-size-n test-string-length)) #'stimes) test-n test-string)))
    (should (stringp actual-result))
    (should (eql actual-result-length expected-result-length))
    (should (equal actual-random-chunk test-string))))

(ert-deftest-n-times stimes-list 100
    (-let* (((test-list test-list-length) (funcall (-compose #'identity-and-seq-length #'generate-test-list-of-floats)))
	    (test-n (random-nat-number-in-range-255))
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length #'nested-seq-one-random-value) #'stimes) test-n test-list)))
    (should (listp actual-result))
    (should (eql actual-result-length test-n))
    (should (equal actual-random-chunk test-list))))

(ert-deftest-n-times stimes-con-of-strings-succeed 100
    (-let* (((test-con test-n) (funcall (-juxt #'generate-test-con-of-strings #'random-nat-number-in-range-255)))
	    ((expected-car expected-cdr) (funcall (-compose #'seq-map-string-to-char #'-cons-to-list) test-con))
	    ((actual-car . actual-cdr) (stimes test-n test-con)))
      (should (eql (seq--count-successive (-partial #'equal expected-car) actual-car) test-n))
      (should (eql (seq--count-successive (-partial #'equal expected-cdr) actual-cdr) test-n))))

(ert-deftest-n-times stimes-con-of-strings-fail 100
    (-let* (((test-con test-n) (funcall (-juxt #'generate-test-con-of-nat-numbers #'random-nat-number-in-range-255))))
    (should-error (stimes test-n test-con))))

(ert-deftest-n-times stimes-string-vector-con-succeed 100
    (-let* (((test-con test-n) (funcall (-juxt #'generate-test-string-vector-of-nat-numbers-con #'random-nat-number-in-range-255)))
	    ((actual-result (actual-car . actual-cdr)) (funcall (-compose #'-duplicate #'stimes) test-n test-con)))
      (should (-cons-pair-p actual-result))
      (should (stringp actual-car))
      (should (vectorp actual-cdr))
      (should (length= actual-car test-n))
      (should (length= actual-cdr test-n))))

(ert-deftest-n-times stimes-vector 100
    (-let* (((test-vector test-vector-length) (funcall (-compose #'identity-and-seq-length #'generate-test-vector-of-nat-numbers)))
	    (test-n (random-nat-number-in-range-255)) 
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length #'nested-seq-one-random-value) #'stimes) test-n test-vector)))
    (should (vectorp actual-result))
    (should (eql actual-result-length test-n))
    (should (equal actual-random-chunk test-vector))))

(ert-deftest-n-times stimes-alist 100
    (-let* (((test-alist _) (generate-random-alist))
	    (test-n (random-nat-number-in-range-255)) 
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length #'nested-seq-one-random-value) #'stimes) test-n test-alist)))
    (should (proper-list-p actual-result))
    (should (eql actual-result-length test-n))
    (should (equal actual-random-chunk test-alist))))

(ert-deftest-n-times stimes-hash-table 100
    (-let* (((test-hash-table _) (generate-one-random-hash-table))
	    (test-n (random-nat-number-in-range-255)) 
	    (((actual-result actual-result-length) actual-random-chunk) (funcall (-compose (-juxt #'identity-and-seq-length #'nested-seq-one-random-value) #'stimes) test-n test-hash-table)))
    (should (proper-list-p actual-result))
    (should (eql actual-result-length test-n))
    (should (equal actual-random-chunk test-hash-table))))

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
