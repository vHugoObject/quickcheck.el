;;; quickcheck-testing-utilities-tests.el --- quickcheck: Testing utilities tests  -*- lexical-binding: t; -*-  
        (require 'seq)
        (require 'calc-comb)
        (require 'cl-lib)
        (require 'map)
        (require 'dash)
        (require 's)
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

(ert-deftest-n-times call-random-function 100
  (-let (((expected-super-set test-list) (funcall (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers))))
    (should (funcall (-compose (apply-partially #'seq-contains-p expected-super-set) #'call-random-function) test-list))))

(ert-deftest-n-times call-random-function-n-times 100
  (-let* ((((expected-super-set test-list) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers) #'random-nat-number-in-range-255)))
	 ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length #'call-random-function-n-times) test-calls test-list)))
    (should (seq-subsetp actual-seq expected-super-set))
    (should (eql actual-seq-length test-calls))))

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
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'nested-seq-one-random-value) #'generate-test-alist-of-strings-nat-number-cons))))
    (should (stringp actual-random-car))
    (should (natnump actual-random-cdr))
    (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-nat-number-strings-cons 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'nested-seq-one-random-value) #'generate-test-alist-of-nat-number-strings-cons))))
    (should (natnump actual-random-car))
    (should (stringp actual-random-cdr))
    (should (seq-every-p-con actual-alist))))



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

(ert-deftest-n-times generate-n-random-seqs 100
  (-let* (((actual-seqs actual-seq-count actual-ag-seqs-length actual-seq-type) (generate-n-random-seqs)))
    (should (seq-every-p-seq actual-seqs))
    (should (greater-than-zero actual-seq-count))
    (should (greater-than-zero actual-ag-seqs-length))))

(provide 'quickcheck-testing-utilities-tests)
;;; quickcheck-testing-utilities-tests.el ends here
