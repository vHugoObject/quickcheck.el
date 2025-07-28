;;; quickcheck.el --- quickcheck: quickcheck clone  -*- lexical-binding: t; no-byte-compile: t -*-

  
;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: testing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a quickcheck clone.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'calc-comb)
(require 'range)
(require 'dash)
(require 's)

(defconst DEFAULTRANDOMNUMBERRANGE
  (list 1 255)    
  "default random number range")
(defconst THOUSAND
  (float 1000)
  "Float creator")
(defconst SIZE
  30
  "Size for generators")

(defalias '2+ (apply-partially #'+ 2))
(defalias 'print (apply-partially #'message "%s"))

(defalias 'not-equal #'/=)
(defalias 'less-than #'<)
(defalias 'less-than-or-equal #'<=)
(defalias 'greater-than #'>)
(defalias 'greater-than-or-equal #'>=)
(defalias 'greater-than-or-equal-one (-rpartial #'>= 1))
(defalias 'equal-zero (apply-partially #'eql 0))
(defalias 'equal-one (apply-partially #'eql 0))

(defalias 'calcFunc-random-255 (apply-partially #'calcFunc-random 255))

(defalias 'cons-vec (apply-partially #'cons 'vec))

(defalias 'shuffle-list (-compose #'cdr (-applify #'math-shuffle-list) (-juxt #'seq-length #'seq-length #'cons-vec)))

(defun convert-calc-value-into-lisp (calc-value)
  (read (math-format-value calc-value)))

(defalias '-first-and-last-item  (-juxt #'-first-item #'-last-item))
(defalias '-iterate-plus-one  (-partial #'-iterate #'1+))


(defalias '-applify-rpartial (-applify #'-rpartial))
(defalias '-applify-partial (-applify #'-partial))

(defalias '-applify-subtract (-applify #'-))
(defalias '-applify-iterate-plus-one  (-applify #'-iterate-plus-one))
  
(defalias '-applify-zip  (-applify #'-zip))
(defalias '-applify-cons  (-applify #'cons))
(defalias '-applify-mapcar  (-applify #'mapcar))
(defalias '-applify-divide (-applify #'/))
(defalias '-applify-cl-subsetp (-applify #'cl-subsetp))


(defalias 'divide-by-THOUSAND   (-rpartial #'/ THOUSAND))
(defalias 'divide-array-values-by-max-array-value (-compose #'-applify-mapcar (-juxt (-compose #'-applify-rpartial (apply-partially #'list #'/) #'float #'1+ #'-max) #'identity)))  
(defalias 'identity-and-seq-length (-juxt #'identity #'seq-length))

(defun times (function n)
  (cdr (-iterate function nil (1+ n))))

(defun times-no-args (function n)
  (cdr (-iterate (lambda (_) (funcall function)) nil (1+ n))))

(cl-defun range-member-exclusive-p ((range-min range-max) number)
  (and (greater-than-or-equal number range-min) (less-than number range-max)))

(defalias 'between-one-and-255 (apply-partially #'range-member-exclusive-p (list 1 255)))
(defalias 'between-zero-and-one (apply-partially #'range-member-exclusive-p (list 0 1)))
(defalias 'between-one-and-? (-compose #'-applify-partial (-partial #'list #'range-member-exclusive-p) (-partial #'list 1)))

(cl-defun non-zero-bounded-modular-addition ((range-min range-max) increase current-number)
  (when (greater-than-or-equal range-min range-max)
    (user-error "range-min %d is not less than range-max %d" range-min range-max))
  (let* ((range-size (- range-max range-min))
	 (adjusted-increase (mod increase range-size))
	 (current-number-index (max (- current-number range-min) 0))
	 (adjusted-current-number-index (mod current-number-index range-size))
	 (new-number-index (mod (+ adjusted-current-number-index adjusted-increase) range-size))
	 (new-number (+ range-min new-number-index)))
  new-number))

(defalias 'range-size (-compose #'-applify-subtract #'reverse))

(cl-defun scale-float-to-range ((min max) float-to-scale)
  ;; Float must be between 0 and 1
  (when (greater-than-or-equal min max)
    (error "min must be less than max"))
  (let* ((min-ceiled (ceiling min))
	 (max-floored (floor max))
	 (min-max (- max-floored min-ceiled))
	 (float-times-min-max (* float-to-scale min-max))
	 (plus-min-ceiled (+ float-times-min-max min-ceiled)))
  (floor plus-min-ceiled)))

(defun random-float-between-0-and-1 ()    
  (funcall (-compose #'convert-calc-value-into-lisp #'math-random-float)))

(cl-defun random-integer-in-range ((min max))
  (if (eql min max)
      min
    (funcall (-compose (apply-partially #'scale-float-to-range (list min max))  #'random-float-between-0-and-1))))

(defalias 'random-integer-in-range-255 (apply-partially #'random-integer-in-range DEFAULTRANDOMNUMBERRANGE))

(defalias 'random-integer-in-range-from-one (-compose #'random-integer-in-range (apply-partially #'list 1)))

(defalias 'random-con-from-array (-compose #'-applify-cons #'seq-two-random-values))

(defun random-integer-list (length)    
  (funcall (-compose #'seq-shuffle #'-iterate-plus-one) (math-random-three-digit-number) length))  
(defalias 'random-integer-list-in-range-255 (-compose #'random-integer-list #'random-integer-in-range-255))

(defun random-integer-range (length)    
  (funcall (-juxt #'identity (apply-partially #'+ length))
	   (math-random-three-digit-number)))

(defalias 'divide-by-random-value (funcall (-compose #'-applify-rpartial (apply-partially #'list #'/) (-compose #'float #'random-integer-in-range-255))))

(defalias 'divide-array-values-by-random-value (apply-partially #'mapcar #'divide-by-random-value))

(defalias 'seq-count-integers (apply-partially #'seq-count #'integerp))
(defalias 'seq-count-floats (apply-partially #'seq-count #'floatp))
(defalias 'seq-count-strings (apply-partially #'seq-count #'stringp))  
(defalias 'seq-count-cons (apply-partially #'seq-count #'consp))

(defalias 'seq-count-between-zero-and-one (apply-partially #'seq-count #'between-zero-and-one))
(defalias 'seq-count-greater-than-or-equal-one (apply-partially #'seq-count #'greater-than-or-equal-one))

(defalias 'seq-map-add-one (apply-partially #'seq-map #'1+))
(defalias 'seq-map-length (apply-partially #'seq-map #'seq-length))
(defalias 'seq-sum-map-length (-compose #'-sum #'seq-map-length))

(defalias 'seq-map-seq--into-list (apply-partially #'seq-map #'seq--into-list))
(defalias 'seq-max-plus-one (-compose #'1+ #'seq-max))
(defalias 'seq-max-plus-one-and-random-chunk-length (-juxt #'seq-max-plus-one  #'seq-random-chunk-length))

(defun seq-take-right (n seq)    
    (funcall (-compose (-rpartial #'seq-take n) #'seq-reverse) seq))

(defun seq-take-last (n seq)    
  (funcall (-compose (apply-partially #'seq-subseq seq)  (-applify #'-)  #'nreverse (apply-partially #'list n) #'seq-length) seq))

(cl-defgeneric seq-shuffle (seq)
  (shuffle-list seq))

(cl-defmethod seq-shuffle ((seq vector))
   (funcall (-compose #'seq--into-vector #'shuffle-list #'seq--into-list) seq))

(cl-defmethod seq-shuffle ((seq string))
   (funcall (-compose #'seq--into-string #'shuffle-list #'seq--into-list) seq))

(defun seq-n-random-values (count seq)
  (funcall (-compose (-rpartial #'seq-take count) #'seq-shuffle) seq))

(defalias 'seq-one-random-value (apply-partially #'seq-n-random-values 1))

(defalias 'seq-two-random-values (apply-partially #'seq-n-random-values 2))

(defalias 'seq-random-chunk-length (-compose #'random-integer-in-range-from-one  #'seq-length))

(defun seq-random-values (seq)
    (funcall (-compose (-rpartial #'seq-n-random-values seq) #'seq-random-chunk-length) seq))

(cl-defgeneric seq-random-iterate-from-max (seq)    
  (funcall (-compose #'-applify-iterate-plus-one #'seq-max-plus-one-and-random-chunk-length) seq))

(cl-defmethod seq-random-iterate-from-max ((seq vector))    
  (funcall (-compose #'seq--into-vector #'-applify-iterate-plus-one #'seq-max-plus-one-and-random-chunk-length) seq))

(cl-defmethod seq-random-iterate-from-max ((seq string))    
  (funcall (-compose #'seq--into-string #'-applify-iterate-plus-one #'seq-max-plus-one-and-random-chunk-length) seq))

(cl-defgeneric seq-subsetp (seq-one seq-two)
    (cl-subsetp seq-one seq-two))

(cl-defmethod seq-subsetp ((seq-one vector) seq-two)
  (funcall (-compose #'-applify-cl-subsetp  #'seq-map-seq--into-list) (list seq-one seq-two)))

(cl-defmethod seq-subsetp ((seq-one string) seq-two)
  (s-contains? seq-one seq-two nil))

(defun seq-random-chunk-of-size-n (chunk-length seq)
  (funcall (-compose #'-first-item  #'seq-one-random-value #'seq-split) seq chunk-length))
 (defalias '-applify-seq-random-chunk-of-size-n (-applify #'seq-random-chunk-of-size-n))

(defalias 'seq-random-chunk (-compose #'-applify-seq-random-chunk-of-size-n (-juxt #'seq-random-chunk-length #'identity)))

;; test-runner
;; needs a test
(defmacro ert-deftest-n-times (name runs body)
  (declare (indent 2))
  (let ((fun-sym (gensym "test")))
    `(ert-deftest ,name ()
       (let ((,fun-sym (lambda (x) (progn
				     ,body 1))))  			 
	(times ,fun-sym ,runs)))))

(cl-defun generate-test-data (&optional &key item-transformer &key list-transformer
				     &key min-length &key max-length)
  (let* ((min-items (or min-length 1))
	 (max-items (or max-length 255))
	 (item-func (or item-transformer #'identity))
	 (list-func (or list-transformer #'seq-shuffle))
	 (range-length (random-integer-in-range (list min-items max-items)))
	 (list-items (random-integer-list range-length)))
    (funcall (-on list-func (apply-partially #'mapcar item-func)) list-items)))

(defalias 'generate-test-list-of-integers #'generate-test-data)

(defalias 'generate-test-list-of-floats-between-zero-and-one (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-max-array-value #'seq-shuffle)))
(defalias 'generate-test-list-of-floats (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-random-value #'seq-shuffle)))
(defalias 'generate-test-list-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string))

(defalias 'generate-test-string (apply-partially #'generate-test-data :item-transformer #'identity :min-length 2 :list-transformer (-compose #'seq--into-string #'seq-shuffle)))


(defalias 'generate-test-vector-of-integers (apply-partially #'generate-test-data :list-transformer (-compose #'seq--into-vector #'seq-shuffle)))


(defalias 'generate-test-alist-of-integers (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'reverse #'seq-shuffle))))


(defalias 'generate-test-con-of-integers (apply-partially #'generate-test-data :min-length 2 :list-transformer #'random-con-from-array))  
(defalias 'generate-test-con-of-floats (apply-partially #'generate-test-data :min-length 2 :list-transformer (-compose #'random-con-from-array #'divide-array-values-by-max-array-value)))
(defalias 'generate-test-con-of-strings (apply-partially #'generate-test-data :min-length 2 :item-transformer #'char-to-string :list-transformer #'random-con-from-array))

(cl-defgeneric semigroup-concat (a b)
  (concat a b))

(cl-defmethod semigroup-concat ((a list) b)
  (append a b))

(cl-defmethod semigroup-concat ((a vector) b)
  (vconcat a b))

;;  (cl-deftype)



(cl-defmethod fmap (function (functor list))
    (seq-map function functor))

(cl-defmethod fmap (function (functor vector))
     (seq--into-vector (funcall (-compose #'seq-map function) functor)))





(defun <$ (a fb)
    (let ((func (funcall (-compose #'partial-fmap #'cl-constantly) a)))
      (funcall func fb)))

;;(pure "x" list)

;;(pure "x" list)

;;(pure "x" list)





;; renames StdGen QcGen
;; newtype QCGen = QCGen StdGen
;; StdGen is renamed QCGen
;; Then Show, Read, RandomGen instances are rewritten
;; showPrec
;; readPrec
;; genRange
;; next

;; declare pure?

;; declare pure?
;; showsPrec n (QCGen g) s = showsPrec n g s







;; newtype Age = Age { unAge:: Int}
;; constructor
;; Age :: Int -> Age
;; deconstructor
;; unAge :: Age -> Int

;; newtype Gen a = MkGen{ unGen :: QCGen -> Int -> a}
;; constructor
;; Gen a :: a -> Gen a
;; deconstructor
;; unGen ::  Gen a -> QCGen -> Int -> a
;; unGen -> 
;; To get a value out generate :: Gen a -> IO a













(provide 'quickcheck)
;;; quickcheck.el ends here
