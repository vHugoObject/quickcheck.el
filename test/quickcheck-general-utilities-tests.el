;;; quickcheck-general-utilities-tests.el --- quickcheck: General Utilities tests  -*- lexical-binding: t; -*-
          (require 'seq)
          (require 'calc-comb)
          (require 'cl-lib)
          (require 'dash)
          (require 's)  
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

(provide 'quickcheck-general-utilities-tests)
;;; quickcheck-general-utilities-tests.el ends here
