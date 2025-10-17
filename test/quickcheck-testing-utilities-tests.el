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

(ert-deftest-n-times generate-one-random-wrapped-integer 100
  (-let* (((actual-wrapped-integer actual-pred) (generate-one-random-wrapped-integer)))
    (should (consp actual-wrapped-integer))
    (should (functionp actual-pred))))

(ert-deftest-n-times generate-one-random-wrapped-integer-type-n-times 100
  (-let* ((((actual-wrapped-integers actual-predicate) expected-integer-count) (funcall (-compose (-juxt #'generate-one-random-wrapped-integer-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (length= actual-wrapped-integers expected-integer-count))
    (should (functionp actual-predicate))))

(ert-deftest-n-times generate-test-data-for-alist-of-nat-numbers 100
    (-let (((actual-alist actual-car-and-actual-cdr) (funcall (-compose (-juxt #'identity (-compose #'-cons-to-list #'seq-take-one-random-value-from-seq)) #'generate-test-alist-of-nat-numbers))))
      (should (seq-every-p-nat-number actual-car-and-actual-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-strings 100
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity (-compose #'seq-take-one-random-value-from-seq)) #'generate-test-alist-of-strings))))
      (should (stringp actual-random-car))
      (should (stringp actual-random-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-strings-nat-number-cons 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) #'generate-test-alist-of-string-nat-number-cons))))
    (should (stringp actual-random-car))
    (should (natnump actual-random-cdr))
    (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-nat-number-strings-cons 100
  (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) #'generate-test-alist-of-nat-number-string-cons))))
    (should (natnump actual-random-car))
    (should (stringp actual-random-cdr))
    (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-alist-of-string-vector-cons 100
    (-let (((actual-alist (actual-random-car . actual-random-cdr)) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) #'generate-test-alist-of-string-vector-of-nat-numbers-cons))))
      (should (stringp actual-random-car))
      (should (vectorp actual-random-cdr))
      (should (seq-every-p-nat-number actual-random-cdr))
      (should (seq-every-p-con actual-alist))))

(ert-deftest-n-times generate-test-data-for-unwrapped-plist-of-nat-numbers 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-unwrapped-plist-of-nat-numbers))))
      (should (plistp actual-plist))
      (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-unwrapped-plist-of-strings 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-unwrapped-plist-of-strings))))
      (should (plistp actual-plist))
      (should (stringp (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-unwrapped-plist-from-strings-nat-number-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-unwrapped-plist-from-string-nat-number-pairs))))
    (should (plistp actual-plist))
    (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-unwrapped-plist-from-nat-number-strings-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'generate-test-unwrapped-plist-from-nat-number-string-pairs))))
    (should (plistp actual-plist))
    (should (stringp (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-wrapped-plist-of-nat-numbers 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'get-plist #'generate-test-wrapped-plist-of-nat-numbers))))
      (should (plistp actual-plist))
      (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-wrapped-plist-of-strings 100
    (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'get-plist #'generate-test-wrapped-plist-of-strings))))
      (should (plistp actual-plist))
      (should (stringp (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-wrapped-plist-from-strings-nat-number-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'get-plist #'generate-test-wrapped-plist-from-string-nat-number-pairs))))
    (should (plistp actual-plist))
    (should (natnump (map-elt actual-plist actual-random-key)))))

(ert-deftest-n-times generate-test-data-for-wrapped-plist-from-nat-number-strings-pairs 100
  (-let (((actual-plist actual-random-key) (funcall (-compose (-juxt #'identity #'map-one-random-key) #'get-plist #'generate-test-wrapped-plist-from-nat-number-string-pairs))))
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

(ert-deftest-n-times generate-one-random-map 100
  (-let* (((actual-map actual-map-pred) (generate-one-random-map)))
    (should (mapp actual-map))
    (should (functionp actual-map-pred))))

(ert-deftest-n-times generate-one-random-map-type-n-times 100
  (-let* ((((actual-maps actual-map-pred) expected-map-count) (funcall (-compose (-juxt #'generate-one-random-map-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (seq-every-p-map actual-maps))
    (should (functionp actual-map-pred))))

(ert-deftest-n-times generate-one-random-map-type-n-random-times 100
  (-let* (((actual-maps actual-map-pred actual-map-count) (generate-one-random-map-type-n-random-times)))
    (should (seq-every-p-map actual-maps))
    (should (greater-than-zero actual-map-count))
    (should (functionp actual-map-pred))))

(ert-deftest-n-times generate-one-random-hash-table 100
  (-let* (((actual-hash-table actual-pred) (generate-one-random-hash-table)))
    (should (hash-table-p actual-hash-table))
    (should (functionp actual-pred))))

(ert-deftest-n-times generate-random-semigroup 0
  (-let* (((actual-semigroup actual-semigroup-size actual-semigroup-type) (generate-random-semigroup-type)))
    (should (semigroupp actual-semigroup))
    (should (greater-than-zero actual-semigroup-size))
    (should (symbolp actual-semigroup-type))))

(ert-deftest-n-times generate-one-random-wrapped-const-type-n-times 100
  (-let* ((((actual-list-of-consts actual-pred) expected-length)
	 (funcall (-compose (-juxt #'generate-one-random-wrapped-const-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (length= actual-list-of-consts expected-length))      
    (should (seq-every-p #'constp actual-list-of-consts))
    (should (function actual-pred))))

(ert-deftest-n-times generate-one-random-wrapped-boolean-type-n-times 100
  (-let* ((((actual-list-of-wrapped-booleans actual-predicate) expected-length)
	 (funcall (-compose (-juxt #'generate-one-random-wrapped-boolean-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (length= actual-list-of-wrapped-booleans expected-length))
    (should (seq-every-p #'wrapped-booleanp actual-list-of-wrapped-booleans))
    (should (functionp actual-predicate))))

(ert-deftest-n-times generate-one-random-wrapped-just-type-n-times 0
  (-let* ((((actual-list-of-justs actual-wrapped-type) expected-length)
	 (funcall (-compose (-juxt #'generate-one-random-wrapped-just-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (length= actual-list-of-justs expected-length))      
    (should (seq-every-p #'justp actual-list-of-justs))
    (should (symbolp actual-wrapped-type))))

(ert-deftest-n-times generate-one-random-wrapped-maybe-just-type-n-times 100
  (-let* ((((actual-list-of-maybes actual-pred) expected-length)
	 (funcall (-compose (-juxt #'generate-one-random-wrapped-maybe-just-type-n-times #'identity) #'random-nat-number-in-range-10))))
    (should (length= actual-list-of-maybes expected-length))      
    (should (seq-every-p #'maybep actual-list-of-maybes))
    (should (functionp actual-pred))))

(ert-deftest-n-times generate-one-random-seq-type-n-times 100
  (-let* ((expected-seq-length (random-nat-number-in-range-10))
	  ((actual-seqs actual-seq-pred) (generate-one-random-seq-type-n-times expected-seq-length)))
    (should (seq-every-p-seq actual-seqs))
    (should (functionp actual-seq-pred))))

(ert-deftest-n-times generate-one-random-seq-type-n-random-times 100
  (-let* (((actual-seqs actual-seq-pred) (generate-one-random-seq-type-n-random-times)))
    (should (seq-every-p-seq actual-seqs))
    (should (symbolp actual-seq-pred))))

(ert-deftest-n-times generate-one-random-seq 100
  (-let* (((actual-list actual-seq-pred) (generate-one-random-seq)))
    (should (seqp actual-list))      
    (should (functionp actual-seq-pred))))

(ert-deftest-n-times generate-seq-of-random-wrapped-integers 0
  (-let* ((test-seq-gen (seq-take-one-random-value-from-seq BASE-SEQ-GENERATORS-FOR-WRAPPED-INTEGERS))
	  (actual-seq (generate-seq-of-random-wrapped-integers test-seq-gen))
	 ((actual-type actual-value) (seq-take-one-random-value-from-seq actual-seq)))
    (should (is-new-type-symbol actual-type))
    (should (functionp actual-value))))

(ert-deftest-n-times generate-one-random-list-of-wrapped-booleans 100
  (-let* (((actual-list-of-wrapped-booleans actual-predicate) (generate-one-random-list-of-wrapped-booleans)))
    (should (seq-every-p #'wrapped-booleanp actual-list-of-wrapped-booleans))      
    (should (functionp actual-predicate))))

(ert-deftest-n-times generate-one-random-list-of-wrapped-integers 100
  (-let* (((actual-list-of-wrapped-integers actual-predicate) (generate-one-random-list-of-wrapped-integers)))
    (should (seq-every-p #'wrapped-integerp actual-list-of-wrapped-integers))
    (should (functionp actual-predicate))))

(ert-deftest-n-times generate-one-random-list-of-maybe-nothings 100
  (-let* ((actual-list (generate-one-random-list-of-maybe-nothings))
	  ((actual-random-wrapped-maybe actual-random-unwrapped-maybe) (funcall (-compose (-juxt #'identity #'get-maybe) #'seq-take-one-random-value-from-seq) actual-list)))
    (should (listp actual-list))
    (should (maybep actual-random-wrapped-maybe))
    (should (nothingp actual-random-unwrapped-maybe))))

(ert-deftest-n-times generate-one-random-wrapped-maybe-just-type-n-random-times 100
  (-let* (((actual-list actual-wrapper-pred actual-wrapped-val-pred actual-list-of-unwrapped-values) (generate-one-random-wrapped-maybe-just-type-n-random-times))
	  ((actual-random-wrapped-maybe actual-random-unwrapped-maybe) (funcall (-compose (-juxt #'identity #'get-maybe) #'seq-take-one-random-value-from-seq) actual-list)))
    (should (listp actual-list))
    (should (listp actual-list-of-unwrapped-values))
    (should (maybep actual-random-wrapped-maybe))
    (should (functionp actual-wrapper-pred))
    (should (functionp actual-wrapped-val-pred))))

(ert-deftest-n-times generate-one-random-list-of-maybes 100
  (-let* (((actual-list actual-wrapper-pred actual-unwrapped-val-pred actual-list-of-unwrapped-values) (generate-one-random-list-of-maybes))
	  (actual-random-wrapped-maybe (seq-take-one-random-value-from-seq actual-list)))
    (should (listp actual-list))
    (should (listp actual-list-of-unwrapped-values))      
    (should (maybep actual-random-wrapped-maybe))
    (should (functionp actual-wrapper-pred))
    (should (functionp actual-unwrapped-val-pred))))

(provide 'quickcheck-testing-utilities-tests)
;;; quickcheck-testing-utilities-tests.el ends here
