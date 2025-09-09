;;; quickcheck-testing-utilities-tests.el --- quickcheck: Testing utilities tests  -*- lexical-binding: t; -*-  
        (require 'seq)
        (require 'map)
        (require 'dash)
        (require 'quickcheck)

(ert-deftest-n-times random-nat-number-in-range 100    
      (let* ((test-max (random 10000000))
  	   (test-min (- test-max (random test-max) 2))  	   
  	   (actual-nat-number (random-nat-number-in-range (list test-min test-max))))
	(should (natnump actual-nat-number))
        (should (greater-than-or-equal actual-nat-number test-min))
        (should (less-than actual-nat-number test-max))))

(ert-deftest-n-times random-nat-number-list 100
  (let ((actual-list (funcall (-compose #'random-nat-number-list #'calcFunc-random-255))))
  (should (seq-every-p-nat-number actual-list))))

(ert-deftest-n-times random-nat-number-range 100
  (-let (((actual-range expected-range-length) (funcall (-juxt #'random-nat-number-range #'identity) (random-nat-number-in-range (list 1 10000)))))
  (should (eql (range-size actual-range) expected-range-length))))

(ert-deftest-n-times divide-by-random-value 100
  (-let (((actual-result actual-input-value) (funcall (-compose (-juxt #'divide-by-random-value #'identity) #'random-nat-number-in-range-255))))
    (should (floatp actual-result))
    (should (less-than-or-equal actual-result actual-input-value))))

(ert-deftest-n-times generate-test-cl-constantly 100
  (-let* (((expected-super-set test-list) (generate-array-of-test-cl-constantlys)))
    (should (eql (seq-length expected-super-set) (seq-length test-list)))
    (should (seq-every-p-nat-number expected-super-set))
    (should (seq-every-p-function test-list))))

(ert-deftest-n-times call-random-function 100
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers)))
	 (actual-value (call-random-function test-list)))
    (should (seq-contains-p expected-super-set actual-value))))

(ert-deftest-n-times call-random-function-n-times 100
  (-let* ((((expected-super-set test-list) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers) #'random-nat-number-in-range-255)))
	 ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length #'call-random-function-n-times) test-calls test-list)))
    (should (seq-subsetp actual-seq expected-super-set))
    (should (eql actual-seq-length test-calls))))

(ert-deftest-n-times call-n-random-functions 100
  (-let* (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers)))
	 (test-n (seq-random-chunk-length expected-super-set))
	 ((actual-list actual-list-length) (funcall (-compose #'identity-and-seq-length #'call-n-random-functions) test-n test-list)))
    (should (seq-subsetp actual-list expected-super-set))
    (should (eql actual-list-length test-n))))

(ert-deftest-n-times divide-array-values-by-random-seq-value 100
  (let ((actual-list (funcall (-compose #'divide-array-values-by-random-value #'random-nat-number-list-in-range-255))))
    (should (seq-every-p-float actual-list))))

(ert-deftest-n-times generate-test-data-for-list-of-nat-numbers 100
   (let ((test-list (generate-test-list-of-nat-numbers)))
    (should (seq-every-p-nat-number test-list))))

(ert-deftest-n-times generate-test-data-for-list-of-floats-1 100
  (let ((test-list (generate-test-list-of-floats)))
    (should (seq-every-p-float test-list))))

(ert-deftest-n-times generate-test-data-for-list-of-floats-2 100
    (let ((test-list (generate-test-list-of-floats-between-zero-and-one)))
    (should (seq-every-p-float test-list))))

(ert-deftest-n-times generate-test-data-for-list-of-strings 100
  (let ((test-list (generate-test-list-of-strings)))
    (should (seq-every-p-string test-list))))

(ert-deftest-n-times generate-test-data-for-list-of-lists-of-nat-numbers 100
    (let ((test-list (generate-test-list-of-lists-nat-numbers)))
    (should (seq-every-p-list test-list))))

(ert-deftest-n-times generate-test-data-for-alist-of-nat-numbers 100
    (-let (((actual-alist actual-car-and-actual-cdr) (funcall (-compose (-juxt #'identity (-compose #'-cons-to-list #'nested-seq-one-random-value)) #'generate-test-alist-of-nat-numbers))))
      (should (seq-every-p-nat-number actual-car-and-actual-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-strings 100
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity (-compose #'nested-seq-one-random-value)) #'generate-test-alist-of-strings))))
      (should (stringp actual-random-car))
      (should (stringp actual-random-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-strings-nat-number-cons 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'nested-seq-one-random-value) #'generate-test-alist-of-string-nat-number-cons))))
    (should (stringp actual-random-car))
    (should (natnump actual-random-cdr))
    (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-nat-number-strings-cons 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'nested-seq-one-random-value) #'generate-test-alist-of-nat-number-string-cons))))
    (should (natnump actual-random-car))
    (should (stringp actual-random-cdr))
    (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-string-vector-cons 100
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'nested-seq-one-random-value) #'generate-test-alist-of-string-vector-of-nat-numbers-cons))))
      (should (stringp actual-random-car))
      (should (vectorp actual-random-cdr))
      (should (seq-every-p-nat-number actual-random-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-plist-of-nat-numbers 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-plist-of-nat-numbers))))
      (should (plistp actual-plist))
      (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-plist-of-strings 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-plist-of-strings))))
      (should (plistp actual-plist))
      (should (stringp (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-plist-from-strings-nat-number-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-plist-from-string-nat-number-pairs))))
    (should (plistp actual-plist))
    (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-plist-from-nat-number-strings-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-plist-from-nat-number-string-pairs))))
    (should (plistp actual-plist))
    (should (stringp (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-hash-table-of-nat-numbers 100
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-hash-table-of-nat-numbers))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-hash-table-of-strings 100
    (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-hash-table-of-strings))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-hash-table-from-strings-nat-number-pairs 100
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-hash-table-from-string-nat-number-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (natnump (map-elt actual-hash-table actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-hash-table-from-nat-number-string-pairs 100
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-hash-table-from-nat-number-string-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (stringp (map-elt actual-hash-table actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-hash-table-from-string-vector-of-nat-numbers-pairs 100
  (-let (((actual-hash-table actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-hash-table-from-string-vector-of-nat-numbers-pairs))))
      (should (hash-table-p actual-hash-table))
      (should (vectorp (map-elt actual-hash-table actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-con-of-nat-numbers 100
  (-let (((actual-con (actual-car . actual-cdr))(funcall (-compose (-juxt #'identity #'identity) #'generate-test-con-of-nat-numbers))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (natnump actual-cdr))))

(ert-deftest-n-times generate-test-data-for-con-of-floats 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-test-con-of-floats))))
	 (should (-cons-pair-p actual-con))
	 (should (floatp actual-car))
	 (should (floatp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-con-of-strings 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-test-con-of-strings))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (stringp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-string-nat-number-con 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-test-string-nat-number-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (natnump actual-cdr))))

(ert-deftest-n-times generate-test-data-for-nat-number-string-con 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-test-nat-number-string-con))))
	 (should (-cons-pair-p actual-con))
	 (should (natnump actual-car))
	 (should (stringp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-string-vector-of-nat-numbers-con 100
  (-let (((actual-con (actual-car . actual-cdr)) (funcall (-compose (-juxt #'identity #'identity) #'generate-test-string-vector-of-nat-numbers-con))))
	 (should (-cons-pair-p actual-con))
	 (should (stringp actual-car))
	 (should (vectorp actual-cdr))))

(ert-deftest-n-times generate-test-data-for-single-string 100
  (-let (((actual-string actual-string-length) (funcall (-compose #'identity-and-seq-length #'generate-test-string))))
    (should (stringp actual-string))
    (should (between-one-and-255 actual-string-length))))

(ert-deftest-n-times generate-test-data-for-vector-of-nat-numbers 100
    (let ((actual-vector (generate-test-vector-of-nat-numbers)))
      (should (vectorp actual-vector))
      (should (seq-every-p-nat-number actual-vector))))



(ert-deftest-n-times generate-one-random-seq-type-n-times 100
  (-let* ((((actual-seqs actual-ag-seqs-length actual-seq-type) expected-seq-count) (funcall (-compose (-juxt #'generate-one-random-seq-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (seq-every-p-seq actual-seqs))
    (should (eql (seq-length actual-seqs) expected-seq-count))
    (should (greater-than-zero actual-ag-seqs-length))
    (should (symbolp actual-seq-type))))

(ert-deftest-n-times generate-one-random-seq-type-n-random-times 100
  (-let* ((((actual-seqs actual-ag-seqs-length actual-seq-type) actual-seq-count) (generate-one-random-seq-type-n-random-times)))
    (should (seq-every-p-seq actual-seqs))
    (should (greater-than-zero actual-seq-count))
    (should (greater-than-zero actual-ag-seqs-length))
    (should (symbolp actual-seq-type))))

(ert-deftest-n-times generate-two-random-seq-types 100
  (-let* ((((test-seq-one test-seq-one-length test-seq-one-type) (test-seq-two test-seq-two-length test-seq-two-type)) (generate-two-random-seq-types)))
    (should (seq-every-p-seq (list test-seq-one test-seq-two)))
    (should (greater-than-zero test-seq-one-length))
    (should (greater-than-zero test-seq-two-length))
    (should (seq-every-p-symbol (list test-seq-one-type test-seq-two-type)))))

(ert-deftest-n-times generate-one-random-list 100
  (-let* (((actual-list actual-list-length) (generate-one-random-list)))
    (should (proper-list-p actual-list))
    (should (natnump actual-list-length))))

(ert-deftest-n-times generate-random-map 100
  (-let* (((actual-map actual-map-length actual-map-type) (generate-random-map)))
    (should (mapp actual-map))
    (should (greater-than-zero actual-map-length))
    (should (symbolp actual-map-type))))

(ert-deftest-n-times generate-one-random-map-type-n-times 100
  (-let* ((((actual-maps actual-ag-maps-length actual-map-type) expected-map-count) (funcall (-compose (-juxt #'generate-one-random-map-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (seq-every-p-map actual-maps))
    (should (eql (seq-length actual-maps) expected-map-count))
    (should (greater-than-zero actual-ag-maps-length))
    (should (symbolp actual-map-type))))

(ert-deftest-n-times generate-one-random-map-type-n-random-times 100
  (-let* ((((actual-maps actual-ag-maps-length actual-map-type) actual-map-count) (generate-one-random-map-type-n-random-times)))
    (should (seq-every-p-map actual-maps))
    (should (greater-than-zero actual-map-count))
    (should (greater-than-zero actual-ag-maps-length))
    (should (symbolp actual-map-type))))

(ert-deftest-n-times generate-one-random-hash-table 100
  (-let* (((actual-hash-table actual-size) (generate-one-random-hash-table)))
    (should (hash-table-p actual-hash-table))
    (should (natnump actual-size))))

(ert-deftest-n-times generate-random-semigroup 0
  (-let* (((actual-semigroup actual-semigroup-size actual-semigroup-type) (generate-random-semigroup-type)))
    (should (semigroupp actual-semigroup))
    (should (greater-than-zero actual-semigroup-size))
    (should (symbolp actual-semigroup-type))))

(provide 'quickcheck-testing-utilities-tests)
;;; quickcheck-testing-utilities-tests.el ends here
