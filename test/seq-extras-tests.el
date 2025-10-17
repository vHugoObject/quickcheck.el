;;; seq-extras-tests.el --- quickcheck: seq-extras  -*- lexical-binding: t; -*-
(require 'seq)
(require 'dash)
(require 's)
(require 'quickcheck)

(ert-deftest-n-times seq-take-right-for-lists 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-list-of-nat-numbers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-right) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (cl-subsetp actual-result test-list))))

(ert-deftest-n-times seq-take-right-for-vectors 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-vector-of-nat-numbers)))
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
  (-let* (((actual-shuffled-vector test-vector) (funcall (-compose (-juxt #'seq-shuffle #'identity) #'generate-test-vector-of-nat-numbers))))
        (should (vectorp actual-shuffled-vector))
	(should-not (seq-difference actual-shuffled-vector test-vector))))

(ert-deftest-n-times seq-shuffle-string 100
  (-let* (((actual-shuffled-string test-string) (funcall (-compose (-juxt #'seq-shuffle #'identity) #'generate-test-string))))
        (should (stringp actual-shuffled-string))
	(should-not (seq-difference actual-shuffled-string test-string))))

(ert-deftest-n-times seq-take-last-for-lists 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-list-of-nat-numbers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (cl-subsetp actual-result test-list))))

(ert-deftest-n-times seq-take-last-for-vectors 100
    (-let* (((test-list test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-vector-of-nat-numbers)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-list)))
      (should (eql actual-result-length test-chunk-length))
      (should (vectorp actual-result))))

(ert-deftest-n-times seq-take-last-for-strings 100
    (-let* (((test-string test-chunk-length) (funcall (-compose (-juxt #'identity #'seq-random-chunk-length) #'generate-test-string)))
	    ((actual-result actual-result-length) (funcall (-compose #'identity-and-seq-length #'seq-take-last) test-chunk-length test-string)))
      (should (eql actual-result-length test-chunk-length))
      (should (stringp actual-result))))

(ert-deftest-n-times seq-random-chunk-length 100
    (-let* (((test-chunk-length test-list-length) (funcall (-compose (-juxt #'seq-random-chunk-length #'seq-length) #'generate-test-list-of-nat-numbers) :min-length 2)))
      (should (less-than test-chunk-length test-list-length))
      (should (greater-than-or-equal test-chunk-length 1))))

(ert-deftest-n-times seq-split-random-list 100
  (let ((actual-list (funcall (-compose #'seq-split-random #'generate-test-list-of-strings) :min-length 2)))
    (should (seq-every-p-list actual-list))))

(ert-deftest-n-times seq-split-random-vector 100
 (let ((actual-vector (funcall (-compose #'seq-split-random #'generate-test-vector-of-nat-numbers) :min-length 2)))
    (should (seq-every-p-vector actual-vector))))

(ert-deftest-n-times seq-split-random-string 100
  (let ((actual-string (funcall (-compose #'seq-split-random #'generate-test-string) :min-length 2)))
    (should (seq-every-p-string actual-string))))

(ert-deftest-n-times seq-n-random-values-list 100
  (-let* (((test-count test-list) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-list-of-strings) :min-length 2))
	  (actual-length (funcall (-compose #'seq-length #'seq-n-random-values) test-count test-list)))
    (should (eql actual-length test-count))))

(ert-deftest-n-times seq-n-random-values-vector 100
  (-let* (((test-count test-vector) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-vector-of-nat-numbers)))
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
	  (funcall (-compose (apply-partially #'seq-map #'identity-and-seq-length) (-juxt #'seq-random-values #'identity) #'generate-test-vector-of-nat-numbers))))      
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
  (-let* (((actual-vector test-vector-max) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'seq-max) #'generate-test-vector-of-nat-numbers))))
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
      (-let* (((test-subset test-list) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'identity) #'generate-test-list-of-nat-numbers)))
        (should-not (seq-subsetp test-subset test-list)))))

(ert-deftest-n-times seq-subsetp-vector-true 100
      (-let* (((test-subset test-vector) (funcall (-compose (-juxt #'seq-random-values #'identity) #'generate-test-vector-of-nat-numbers))))
        (should (seq-subsetp test-subset test-vector))))

(ert-deftest-n-times seq-subsetp-vector-false 100
      (-let* (((test-subset test-vector) (funcall (-compose (-juxt #'seq-random-iterate-from-max #'identity) #'generate-test-vector-of-nat-numbers))))
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
    (-let* (((test-chunk-length test-list) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-list-of-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'identity-and-seq-length #'seq-random-chunk-of-size-n) test-chunk-length test-list)))
      (should (listp actual-chunk))
      (should (seq-subsetp actual-chunk test-list))))

(ert-deftest-n-times seq-random-chunk-of-size-n-vector 100
    (-let* (((test-chunk-length test-vector) (funcall (-compose (-juxt #'seq-random-chunk-length #'identity) #'generate-test-vector-of-nat-numbers)))
	    ((actual-chunk actual-chunk-length) (funcall (-compose #'identity-and-seq-length #'seq-random-chunk-of-size-n) test-chunk-length test-vector)))
      (should (vectorp actual-chunk))
      (should (seq-subsetp actual-chunk test-vector))))

(ert-deftest-n-times seq-random-position-lists 100
  (-let* ((((test-list test-list-length) actual-position) (funcall (-compose (-juxt #'identity-and-seq-length #'seq-random-position) #'generate-test-list-of-strings))))
	(should (funcall (between-zero-and-? test-list-length) actual-position))))

(ert-deftest-n-times seq-random-position-vectors 100
  (-let* ((((test-vector test-vector-length) actual-position) (funcall (-compose (-juxt #'identity-and-seq-length #'seq-random-position) #'generate-test-vector-of-nat-numbers))))
	(should (funcall (between-zero-and-? test-vector-length) actual-position))))

(ert-deftest-n-times seq-random-position-strings 100
  (-let* ((((test-string test-string-length) actual-position) (funcall (-compose (-juxt #'identity-and-seq-length #'seq-random-position) #'generate-test-string))))
	(should (funcall (between-zero-and-? test-string-length) actual-position))))

(ert-deftest-n-times seq-random-item-with-position 100
  (-let* (((test-seq test-seq-length expected-seq-type) (generate-random-seq))
	  ((actual-item actual-position) (seq-random-item-with-position test-seq)))
    (should (seq-contains-p test-seq actual-item))
    (should (funcall (between-zero-and-? test-seq-length) actual-position))))

(ert-deftest-n-times seq-butlast 100
  (-let* (((test-seq test-seq-length expected-seq-type) (generate-random-seq))
	  (expected-last-item-as-seq (funcall (-compose #'list #'seq-last) test-seq))
	  ((actual-seq actual-seq-length) (funcall (-compose (-juxt #'identity #'seq-length) #'seq-butlast) test-seq)))
    (should (eql actual-seq-length (1- test-seq-length)))
    (should (equal (seq-difference test-seq actual-seq) expected-last-item-as-seq))))

(ert-deftest-n-times seq-cons 100
  (-let* (((expected-seq _ expected-seq-type) (generate-random-seq))
	  ((test-seq test-item-to-cons) (funcall (-juxt #'seq-rest #'seq-first) expected-seq))
	  ((actual-seq actual-seq-type) (funcall (-compose (-juxt #'identity #'seq-type) #'seq-cons) test-item-to-cons test-seq)))
    (should (equal actual-seq-type expected-seq-type))
    (should (equal actual-seq expected-seq))))

(ert-deftest-n-times seq-snoc 100
  (-let* (((expected-seq _ expected-seq-type) (generate-random-seq))
	  ((test-seq test-item-to-snoc) (funcall (-juxt #'seq-butlast #'seq-last) expected-seq))
	  ((actual-seq actual-seq-type) (funcall (-compose (-juxt #'identity #'seq-type) #'seq-snoc) test-item-to-snoc test-seq)))
    (should (equal actual-seq-type expected-seq-type))
    (should (equal actual-seq expected-seq))))

(ert-deftest-n-times seq-append 100
  (-let* (((expected-seq _ expected-seq-type) (generate-random-seq))
	  ((test-seq test-item-to-append) (funcall (-juxt #'seq-butlast #'seq-last) expected-seq)))
    (seq-append test-item-to-append test-seq)
    (should (cl-typep test-seq expected-seq-type))
    (should (equal test-seq expected-seq))))

(ert-deftest-n-times seq-push 100
  (-let* (((expected-seq _ expected-seq-type) (generate-random-seq))
	  ((test-seq test-item-to-push) (funcall (-juxt #'seq-rest #'seq-first) expected-seq)))
    (seq-push test-item-to-push test-seq)
    (should (cl-typep test-seq expected-seq-type))
    (should (equal test-seq expected-seq))))

(ert-deftest-n-times generate-random-seq 100
  (-let* (((actual-seq actual-seq-length actual-seq-type) (generate-random-seq)))
    (should (seqp actual-seq))
    (should (symbolp actual-seq-type))
    (should (integerp actual-seq-length))))

(ert-deftest-n-times seq-rotate 100
  (-let* (((test-rotations (test-seq test-seq-length test-seq-type)) (funcall (-juxt #'random-nat-number-in-range-255 #'generate-random-seq)))
	  ((test-position test-item) (funcall (-compose (-juxt #'identity (-partial #'seq-elt test-seq)) #'seq-random-position) test-seq))
	  (expected-test-item-position (funcall (-compose (-rpartial #'mod test-seq-length) (-partial #'+ test-rotations)) test-position))
	  ((actual-seq actual-item) (funcall (-compose (-juxt #'identity (-rpartial #'seq-elt expected-test-item-position)) #'seq-rotate) test-rotations test-seq)))
    (should (cl-typep actual-seq test-seq-type))
    (should (eql actual-item test-item))))

(ert-deftest-n-times seq-take-infinite 100
  (-let* ((((test-seq _ test-seq-type) test-n) (funcall (-juxt #'generate-random-seq #'random-nat-number-in-range-500)))
	  ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length #'seq-take-infinite) test-n test-seq)))
    (should (cl-typep actual-seq test-seq-type))
    (should (eql actual-seq-length test-n))))

(ert-deftest-n-times seq-subseq-infinite 100
  (-let* ((((test-seq _ test-seq-type) test-subseq-start) (funcall (-compose (-juxt #'identity (-compose #'seq-random-position #'car)) #'generate-random-seq)))
	  ((test-subseq-end expected-subseq-length) (funcall (-compose (-juxt #'identity (-rpartial #'- test-subseq-start)) (-partial #'+ test-subseq-start) #'random-nat-number-in-range-255)))
	  ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length #'seq-subseq-infinite) test-subseq-start test-subseq-end test-seq)))
    (should (cl-typep actual-seq test-seq-type))
    (should (eql actual-seq-length expected-subseq-length))))

(ert-deftest-n-times seq-concat 100
  (-let* (((test-seqs _ expected-seq-length expected-seq-type) (generate-one-random-seq-type-n-times))
	  ((actual-seq actual-seq-length) (funcall (-compose #'identity-and-seq-length (-applify #'seq-concat)) test-seqs)))
  (should (cl-typep actual-seq expected-seq-type))
  (should (eql actual-seq-length expected-seq-length))))

(ert-deftest-n-times seq-split-infinite 100
  (-let* ((((test-seq test-seq-length test-seq-type) test-chunk-size) (funcall (-juxt #'generate-random-seq #'random-nat-number-in-range-255)))
	  ((actual-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'nested-seq-take-one-random-value) #'seq-split-infinite) test-chunk-size test-seq)))
    (should (cl-typep actual-random-chunk test-seq-type))
    (should (length= actual-random-chunk test-chunk-size))))

(ert-deftest-n-times seq-n-random-chunks-of-size-x 100
  (-let* (((test-chunk-length test-chunk-count (test-seq _ test-seq-type)) (funcall (-juxt #'random-nat-number-in-range-500 #'random-nat-number-in-range-255 #'generate-random-seq)))
	  ((actual-chunked-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'nested-seq-take-one-random-value) #'seq-n-random-chunks-of-size-x) test-chunk-length test-chunk-count test-seq)))
  (should (length= actual-chunked-seq test-chunk-count))
  (should (cl-typep actual-random-chunk test-seq-type))
  (should (length= actual-random-chunk test-chunk-length))))

(ert-deftest-n-times seq-n-random-chunks-of-random-size 100
  (-let* (((test-chunk-count (test-seq _ test-seq-type)) (funcall (-juxt #'random-nat-number-in-range-255 #'generate-random-seq)))
	  ((actual-chunked-seq actual-random-chunk) (funcall (-compose (-juxt #'identity #'nested-seq-take-one-random-value) #'seq-n-random-chunks-of-random-size) test-chunk-count test-seq)))
  (should (length= actual-chunked-seq test-chunk-count))
  (should (cl-typep actual-random-chunk test-seq-type))))

(ert-deftest-n-times seq-zip-shortest-pair 100
    (-let* ((((test-seq-one test-seq-one-length _) (test-seq-two test-seq-two-length __)) (generate-two-random-seq-types))
	   ((actual-zipped-seqs ((actual-car . actual-cdr) actual-random-position)) (funcall (-compose (-juxt #'identity #'seq-random-item-with-position) #'seq-zip-shortest-pair) test-seq-one test-seq-two)))
      (should (equal (seq-length actual-zipped-seqs) (min test-seq-one-length test-seq-two-length)))        
      (should (equal actual-car (seq-elt test-seq-one actual-random-position)))
      (should (equal actual-cdr (seq-elt test-seq-two actual-random-position)))))

(provide 'seq-extras-tests)
;;; seq-extras-tests.el ends here
