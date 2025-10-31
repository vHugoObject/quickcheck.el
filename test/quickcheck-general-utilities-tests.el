;;; quickcheck-general-utilities-tests.el --- quickcheck: General Utilities tests  -*- lexical-binding: t; -*-
          (require 'seq)
          (require 'cl-lib)
          (require 'dash)
          (require 'quickcheck)

(ert-deftest-n-times convert-calc-value-into-lisp 100
  (should (floatp (convert-calc-value-into-lisp (math-gaussian-float)))))

(ert-deftest-n-times -times 100
  (-let* ((((expected-num test-func) test-calls) (funcall (-juxt (-compose (-juxt #'identity #'cl-constantly) #'random-nat-number-in-range-255) #'random-nat-number-in-range-255)))
	 ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length #'-times) test-calls test-func)))
    (should (seq-every-p (-partial #'eql expected-num) actual-seq))
    (should (eql actual-seq-length test-calls))))

(ert-deftest-n-times -compose-and-call 0
  (-let* (((test-list (test-func-one test-func-two)) (generate-array-of-test-cl-constantlys))
	  (test-arg (random-nat-number-in-range-255))
	 ((actual-value) (-compose-and-call (test-func-one test-func-two) test-arg)))
    (should (eql actual-value (car test-list)))))

(ert-deftest-n-times -juxt-every-true 100
  (-let* (((test-predicates test-list) (generate-test-list-of-integer-member-predicates))
	  (test-juxt-every (funcall (-applify #'-juxt-every) test-predicates)))
    (should (funcall test-juxt-every test-list))))

(ert-deftest-n-times -juxt-every-false 100
  (-let* (((test-predicates full-test-list) (generate-test-list-of-integer-member-predicates))
	  (test-juxt-every (funcall (-applify #'-juxt-every) test-predicates))
	  (test-list (seq-random-chunk full-test-list)))
    (should-not (funcall test-juxt-every test-list))))

(ert-deftest-n-times -juxt-any-true 100
  (-let* (((test-predicates full-test-list) (generate-test-list-of-integer-member-predicates))
	  (test-juxt-any (funcall (-applify #'-juxt-any) test-predicates))
	  (test-list (seq-random-chunk full-test-list)))
    (should (funcall test-juxt-any test-list))))

(ert-deftest-n-times -juxt-any-false 100
  (-let* (((test-predicates _) (generate-test-list-of-integer-member-predicates))
	  (test-juxt-any (funcall (-applify #'-juxt-any) test-predicates))
	  (test-list (generate-test-list-of-strings)))
    (should-not (funcall test-juxt-any test-list))))

(ert-deftest-n-times map-on-alist-of-nat-numbers 100
  (-let* ((test-alist (generate-test-alist-of-nat-numbers))
	  ((actual-car . actual-cdr) (map-on #'-applify-cons #'-sum #'-sum test-alist)))
  (should (natnump actual-car))
  (should (natnump actual-cdr))))

(ert-deftest-n-times map-on-alist-of-strings 100
  (-let* (((test-alist test-alist-length) (funcall (-compose #'identity-and-seq-length #'generate-test-alist-of-nat-numbers)))
	  (actual-result (map-on #'identity #'concat #'concat test-alist)))      
  (should (seq-every-p-string actual-result))
  (should (seq-every-p (-rpartial #'length= test-alist-length) actual-result))))

(ert-deftest-n-times map-on-alist-of-string-nat-number-cons 100
  (-let* (((test-alist test-alist-length) (funcall (-compose #'identity-and-seq-length #'generate-test-alist-of-nat-numbers)))
	  ((actual-string actual-sum) (map-on #'identity #'concat #'-sum test-alist)))      
  (should (stringp actual-string))
  (should (length= actual-string test-alist-length))
  (should (integerp actual-sum))))

(ert-deftest-n-times map-one-random-value 100
  (-let* (((test-map _ __) (generate-random-map))
	  (actual-value (map-one-random-value test-map))
	  (expected-values (map-values test-map)))
    (should (seq-contains expected-values actual-value))))

(ert-deftest-n-times between-one-and-?-true 100
  (-let* (((test-? test-nat-number) (funcall (-compose (-juxt #'1+ #'identity) #'random-nat-number-in-range-255))))
    (should (eq (funcall (between-one-and-? test-?) test-nat-number) t))))

(ert-deftest-n-times between-one-and-?-false 100
  (-let* (((test-nat-number test-?) (funcall (-compose (-juxt #'1+ #'identity) #'random-nat-number-in-range-255))))
    (should (eq (funcall (between-one-and-? test-?) test-nat-number) 'nil))))

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

(ert-deftest-n-times non-zero-bounded-modular-addition-basic-nat-number-test 100
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
  (let ((actual-list (funcall (-compose #'divide-array-values-by-max-array-value #'random-nat-number-list-in-range-255))))
    (should (seq-every-p-between-zero-and-one actual-list))))

(ert-deftest-n-times const 100
  (-let (((test-a . test-b) (generate-test-con-of-nat-numbers)))
    (should (equal (const test-a test-b) test-a))))

(ert-deftest-n-times homogenic-list-p-t 0
  (-let* (((test-list _) (generate-one-random-list)))
    (should (homogenic-list-p test-list))))

(ert-deftest-n-times homogenic-list-p-nil 100
  (let ((test-list (funcall (-compose #'-applify-append #'generate-two-random-distinct-list-types))))
    (should-not (homogenic-list-p test-list))))

(provide 'quickcheck-general-utilities-tests)
;;; quickcheck-general-utilities-tests.el ends here
