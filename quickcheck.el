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

(require 'ert)
(require 'eieio)
(require 'cl-lib)
(require 'calc-comb)
(require 'range)
(require 'map)  
(require 'macroexp)
(require 'gv)
(require 'dash)
(require 's)

(defconst TENRANGE
  (list 1 10)    
  "default random number range")  
(defconst DEFAULTRANDOMNUMBERRANGE
  (list 1 255))    
(defconst FIVEHUNDREDRANGE
  (list 1 500))
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
(defalias 'greater-than-or-equal-zero (-rpartial #'>= 0))
(defalias 'greater-than-zero (-rpartial #'> 0))  
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
(defalias '-applify-divide (-applify #'/))


(defalias '-applify-zip  (-applify #'-zip))
(defalias '-applify-zip-pair  (-applify #'-zip-pair))

(defalias '-applify-cons  (-applify #'cons))
(defalias '-applify-concat  (-applify #'concat))
(defalias '-applify-vconcat  (-applify #'vconcat))
(defalias '-applify-append (-applify #'append))


(defalias '-applify-mapcar  (-applify #'mapcar))
(defalias '-applify-cl-subsetp (-applify #'cl-subsetp))
(defalias '-applify-seq-split (-applify #'seq-split))
(defalias '-applify-seq-take (-applify #'seq-take))
(defalias '-applify-vector (-applify #'vector))
(defalias '-applify-map-elt (-applify #'map-elt))

(defalias 'seq-take-flipped (-flip #'seq-take))
(defalias '-applify-seq-take-flipped (-applify #'seq-take-flipped))
(defalias 'seq-elt-flipped (-flip #'seq-elt))
(defalias '-applify-seq-elt-flipped (-applify #'seq-elt-flipped))

(defalias 'divide-by-THOUSAND   (-rpartial #'/ THOUSAND))
(defalias 'divide-array-values-by-max-array-value (-compose #'-applify-mapcar (-juxt (-compose #'-applify-rpartial (apply-partially #'list #'/) #'float #'1+ #'-max) #'identity)))  

(defalias 'identity-and-seq-length (-juxt #'identity #'seq-length))
(defalias '-duplicate (-juxt #'identity #'identity))

(defun -times (calls function)
  "Convenience wrapper for -dotimes"
  (let ((result '()))
    (-dotimes calls (lambda (index) (seq-append (funcall function index) result)))
    result))

(defun -times-no-args (calls function)
  "Call a function n times with no args"
  (let ((result '()))
    (-dotimes calls (lambda (_) (seq-append (funcall function) result)))
    result))

(defalias '-times-no-args-twice (-partial #'-times-no-args 2))

(cl-defun range-member-exclusive-p ((range-min range-max) number)
  (and (greater-than-or-equal number range-min) (less-than number range-max)))

(defalias 'between-one-and-255 (apply-partially #'range-member-exclusive-p (list 1 255)))
(defalias 'between-zero-and-one (apply-partially #'range-member-exclusive-p (list 0 1)))
(defalias 'between-one-and-? (-compose #'-applify-partial (-partial #'list #'range-member-exclusive-p) (-partial #'list 1)))
(defalias 'between-zero-and-? (-compose #'-applify-partial (-partial #'list #'range-member-exclusive-p) (-partial #'list 0)))

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

(cl-defun random-nat-number-in-range ((min max))
  (if (eql min max)
      min
    (funcall (-compose (apply-partially #'scale-float-to-range (list min max))  #'random-float-between-0-and-1))))

(defalias 'random-nat-number-in-range-10 (apply-partially #'random-nat-number-in-range TENRANGE))

(defalias 'random-nat-number-in-range-255 (apply-partially #'random-nat-number-in-range DEFAULTRANDOMNUMBERRANGE))

(defalias 'random-nat-number-in-range-500 (apply-partially #'random-nat-number-in-range FIVEHUNDREDRANGE))

(defalias 'random-nat-number-in-range-from-one (-compose #'random-nat-number-in-range (apply-partially #'list 1)))

(defalias 'random-nat-number-in-range-from-zero (-compose #'random-nat-number-in-range (apply-partially #'list 0)))

(defalias 'random-con-from-array (-compose #'-applify-cons #'seq-two-random-values))

(defun random-nat-number-list (length)    
  (funcall (-compose #'seq-shuffle #'-iterate-plus-one) (math-random-three-digit-number) length))  
(defalias 'random-nat-number-list-in-range-255 (-compose #'random-nat-number-list #'random-nat-number-in-range-255))

(defun random-nat-number-range (length)    
  (funcall (-juxt #'identity (apply-partially #'+ length))
	   (math-random-three-digit-number)))

(defalias 'divide-by-random-value (funcall (-compose #'-applify-rpartial (apply-partially #'list #'/) (-compose #'float #'random-nat-number-in-range-255))))

(defalias 'divide-array-values-by-random-value (apply-partially #'mapcar #'divide-by-random-value))

(defalias 'call-random-function (-compose #'funcall #'nested-seq-one-random-value))

(defun call-random-function-n-times (calls list)
  (funcall (-compose (-partial #'-times-no-args calls) #'nested-seq-one-random-value) list))

(defun call-n-random-functions (n funcs)
  (funcall (-compose (-partial #'-map #'funcall) (-partial #'-take n) #'shuffle-list) funcs))

(defalias 'random-boolean (-partial #'nested-seq-one-random-value (list 't 'nil)))

(defalias 'seq-count-nat-numbers (apply-partially #'seq-count #'natnump))
(defalias 'seq-count-floats (apply-partially #'seq-count #'floatp))
(defalias 'seq-count-strings (apply-partially #'seq-count #'stringp))  
(defalias 'seq-count-cons (apply-partially #'seq-count #'consp))
(defalias 'seq-count-between-zero-and-one (apply-partially #'seq-count #'between-zero-and-one))
(defalias 'seq-count-greater-than-or-equal-one (apply-partially #'seq-count #'greater-than-or-equal-one))

(defalias 'seq-map-add-one (apply-partially #'seq-map #'1+))
(defalias 'seq-map-seq-length (apply-partially #'seq-map #'seq-length))

(defalias 'seq-map-map-size (apply-partially #'seq-map #'map-length))

(defalias 'seq-map-seq--into-list (apply-partially #'seq-map #'seq--into-list))
(defalias 'seq-map-char-to-string (apply-partially #'seq-map #'char-to-string))
(defalias 'seq-map-string-to-char (apply-partially #'seq-map #'string-to-char))
(defalias 'seq-map-cl-constantly (apply-partially #'seq-map #'cl-constantly))

(defalias 'seq-min-length (-compose #'-min #'seq-map-seq-length))
(defalias 'seq-sum-seq-lengths (-compose #'-sum #'seq-map-seq-length))
(defalias 'seq-sum-map-sizes (-compose #'-sum #'seq-map-map-size))

(defalias 'seq-max-plus-one (-compose #'1+ #'seq-max))
(defalias 'seq-max-plus-one-and-random-chunk-length (-juxt #'seq-max-plus-one  #'seq-random-chunk-length))

(defalias 'seq-every-p-integer (apply-partially #'seq-every-p #'integerp))
(defalias 'seq-every-p-nat-number (apply-partially #'seq-every-p #'natnump))
(defalias 'seq-every-p-float (apply-partially #'seq-every-p #'floatp))
(defalias 'seq-every-p-between-zero-and-one (apply-partially #'seq-every-p #'between-zero-and-one))

(defalias 'seq-every-p-string (apply-partially #'seq-every-p #'stringp))
(defalias 'seq-every-p-seq (apply-partially #'seq-every-p #'seqp))
(defalias 'seq-every-p-map (apply-partially #'seq-every-p #'mapp))
(defalias 'seq-every-p-list (apply-partially #'seq-every-p #'listp))
(defalias 'seq-every-p-proper-list (apply-partially #'seq-every-p #'proper-list-p))
(defalias 'seq-every-p-vector (apply-partially #'seq-every-p #'vectorp))
(defalias 'seq-every-p-con (apply-partially #'seq-every-p #'-cons-pair-p))
(defalias 'seq-every-p-symbol (apply-partially #'seq-every-p #'symbolp))

(defalias 'seq-every-p-function (apply-partially #'seq-every-p #'functionp))

(defalias 'seq-take-one (-rpartial #'seq-take 1))
(defalias 'seq-take-two (-rpartial #'seq-take 2))
(defalias 'seq-take-three (-rpartial #'seq-take 3))

(defun seq-type (seq)
  (pcase (cl-type-of seq)
    ('vector 'vector)
    ('string 'string)
    ('cons 'list)
    (type (error "Not a sequence: %S" type))))

(defun seq-concat (&rest seqs)
  (let* ((type (funcall (-compose #'seq-type #'seq-first) seqs)))
    (funcall (-applify (apply-partially #'seq-concatenate type)) seqs)))
(defalias '-applify-seq-concat (-applify #'seq-concat))

(defun seq-take-right (n seq)    
    (funcall (-compose (-rpartial #'seq-take n) #'seq-reverse) seq))

(defun seq-take-last (n seq)    
  (funcall (-compose (apply-partially #'seq-subseq seq)  (-applify #'-)  #'nreverse (apply-partially #'list n) #'seq-length) seq))

(defalias 'seq-last (-compose #'seq-first (apply-partially #'seq-take-last 1)))

(cl-defgeneric seq-cons (a b)
  (cons a b))

(cl-defmethod seq-cons (a (b vector))
  (funcall (-compose (-rpartial #'vconcat b) #'vector) a))

(cl-defmethod seq-cons (a (b string))
  (funcall (-compose (-rpartial #'concat b) #'char-to-string) a))

(cl-defgeneric seq-snoc (a b)
  (funcall (-compose (-partial #'append b) #'list) a))



(cl-defmethod seq-snoc (a (b vector))
  (funcall (-compose (-partial #'vconcat b) #'vector) a))

(cl-defmethod seq-snoc (a (b string))
  (funcall (-compose (-partial #'concat b) #'char-to-string) a))

(defmacro seq-append (newelt place)
  "Add NEWELT to the end of the seq stored in the generalized variable PLACE.
  Based on push"
  (declare (debug (form gv-place)))
  (macroexp-let2 macroexp-copyable-p x newelt
    (gv-letplace (getter setter) place
      (funcall setter `(seq-snoc ,x ,getter)))))

(defmacro seq-push (newelt place)
  "Add NEWELT to the seq stored in the generalized variable PLACE.
  This is just a generic version of push"
  (declare (debug (form gv-place)))
  (macroexp-let2 macroexp-copyable-p x newelt
    (gv-letplace (getter setter) place
      (funcall setter `(seq-cons ,x ,getter)))))

(cl-defgeneric seq-take-infinite (n seq)
  "When n is larger than the seq-length, we loop back around")

(cl-defmethod seq-take-infinite (n (seq cons))
  (funcall (-compose (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod seq-take-infinite (n (seq vector))
  (funcall (-compose #'seq--into-vector (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defmethod seq-take-infinite (n (seq string))
  (funcall (-compose #'seq--into-string (-rpartial #'seq-take n) #'-cycle) seq))

(cl-defgeneric seq-shuffle (seq)
  (shuffle-list seq))

(cl-defmethod seq-shuffle ((seq vector))
   (funcall (-compose #'seq--into-vector #'shuffle-list #'seq--into-list) seq))

(cl-defmethod seq-shuffle ((seq string))
   (funcall (-compose #'seq--into-string #'shuffle-list #'seq--into-list) seq))

(defalias 'seq-split-the-shuffle (-compose #'seq-shuffle #'seq-split))

(defalias 'seq-one-random-value (-compose #'seq-take-one #'seq-shuffle))

(defalias 'nested-seq-one-random-value (-compose #'seq-first #'seq-one-random-value))

(defalias 'seq-two-random-values (-compose #'seq-take-two #'seq-shuffle))

(defalias 'seq-random-chunk-length (-compose #'random-nat-number-in-range-from-one #'seq-length))

(defun seq-random-chunk-of-size-n (chunk-length seq)
  (funcall (-compose #'seq-first  #'seq-one-random-value #'seq-split) seq chunk-length))

 (defalias '-applify-seq-random-chunk-of-size-n (-applify #'seq-random-chunk-of-size-n))

(defalias 'seq-random-chunk (-compose #'-applify-seq-random-chunk-of-size-n (-juxt #'seq-random-chunk-length #'identity)))

(defalias 'seq-random-position (-compose #'random-nat-number-in-range-from-zero #'seq-length))

(defalias 'seq-split-random (-compose #'-applify-seq-split (-juxt #'identity #'seq-random-chunk-length)))

(defun seq-random-values (seq)
    (funcall (-compose (-rpartial #'seq-n-random-values seq) #'seq-random-chunk-length) seq))

(defun seq-n-random-values (count seq)
  (funcall (-compose (-rpartial #'seq-take count) #'seq-shuffle) seq))

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

(defalias 'seq-random-item-with-position (-compose (-juxt #'-applify-seq-elt-flipped #'seq-first) (-juxt #'seq-random-position #'identity)))

(defun seq-butlast (seq)
  (funcall (-compose (apply-partially #'seq-subseq seq 0) #'1- #'seq-length) seq))

(defun seq-rotate (rotations seq)
  "based on -rotate"
  (cond
    ((eql (seq-length seq) 0) (seq))
    ((eql rotations 0) (seq))
    ((-let (((seq-head seq-tail)
	    (funcall (-compose (-juxt (-partial #'seq-subseq seq) (-partial #'seq-take seq)) #'-applify-subtract (-juxt #'identity (-partial #'mod rotations)) #'seq-length) seq)))
       (funcall (-compose (-rpartial #'seq-concatenate seq-head seq-tail) #'seq-type) seq)))))

(defalias 'seq-rotate-flipped (-flip #'seq-rotate))
(defalias '-applify-seq-rotate-flipped (-applify #'seq-rotate-flipped))
(defalias '-applify-seq-rotate (-applify #'seq-rotate))

(defun seq-subseq-infinite (start end seq)
  "We use seq-take-infinite so that we loop around"
  (when (or (cl-minusp start) (cl-minusp end))
    (error "Positions can not be negative"))
  (when (greater-than start end)
    (error "Start can not be greater than end"))
  (let* ((length (seq-length seq)))
    (if (greater-than end length)
	(funcall (-compose (-rpartial #'seq-subseq start end) (-rpartial #'seq-take-infinite seq) (-partial #'+ length) (-partial #'- end)) length)
      (seq-subseq seq start end))))

(cl-defgeneric seq-split-infinite (chunk-size seq)
  "For strings and list"
  (let ((chunks '()))
    (funcall (-compose (-rpartial #'-dotimes (lambda (index) (seq-append (seq-subseq-infinite (* index chunk-size) (* (1+ index) chunk-size) seq) chunks)))
		       (-rpartial #'ceiling chunk-size)
		       #'seq-length)
	     seq)
    chunks))

(cl-defmethod seq-split-infinite (chunk-size (seq vector))
  (let ((chunks []))
    (funcall (-compose (-rpartial #'-dotimes (lambda (index) (seq-append (seq-subseq-infinite (* index chunk-size) (* (1+ index) chunk-size) seq) chunks)))
		       (-rpartial #'ceiling chunk-size)
		       #'seq-length)
	     seq)
    chunks))

(defun seq-n-random-chunks-of-size-x (chunk-size chunk-count seq)
  (funcall (-compose (apply-partially #'seq-take-infinite chunk-count) #'seq-shuffle #'seq-split-infinite) chunk-size seq))

(defun seq-n-random-chunks-of-random-size (chunk-count seq)
  (funcall (-compose (-rpartial #'seq-n-random-chunks-of-size-x chunk-count seq) #'seq-random-chunk-length) seq))

(defalias '-applify-seq-n-random-chunks-of-random-size (-applify #'seq-n-random-chunks-of-random-size))

(defun seq-zip-shortest-with (function seqs)
  (let ((zipper (lambda (index) (funcall (-compose function #'seq-map) (-rpartial #'seq-elt index) seqs))))
    (-times (seq-min-length seqs) zipper)))

(defalias 'seq-zip-shortest-pair (-compose (-partial #'seq-zip-shortest-with #'-applify-cons) #'list))
(defalias '-applify-seq-zip-shortest-pair (-applify #'seq-zip-shortest-pair))

(defalias 'seq-zip-shortest-pair-into-plist (-compose #'map-into-plist #'seq-zip-shortest-pair))

(defalias 'map-into-alist (-rpartial #'map-into 'alist))
(defalias 'map-into-plist (-rpartial #'map-into 'plist))
(defalias 'map-into-hash-table (-rpartial #'map-into 'hash-table))

(defalias 'alistp (-partial #'seq-every-p #'-cons-pair-p))

(defun map-type (map)
  (cond
   ((hash-table-p map) 'hash-table)
   ((alistp map) 'alist)
   ((plistp map) 'plist)
   (_ (error "not a map"))))

(defalias 'map-one-random-key (-compose #'nested-seq-one-random-value #'map-keys))
(defalias 'seq-map-one-random-map-key (-partial #'seq-map #'map-one-random-key))

(defalias 'map-one-random-value (-compose #'-applify-map-elt (-juxt #'identity #'map-one-random-key)))

(defun map-on (op keys-trans values-trans map)
  "Apply one function to map keys, one function to map values and one function the result"
   (funcall (-compose op (-juxt (-compose keys-trans #'map-keys) (-compose values-trans #'map-values))) map))

(defalias 'concat-two-cons-of-strings (-compose (-partial #'map-on #'-applify-cons #'-applify-concat #'-applify-concat) #'list))

(defalias 'concat-two-string-vector-cons (-compose (-partial #'map-on #'-applify-cons #'-applify-concat #'-applify-vconcat) #'list))

;; test-runner
;; needs a test
(defmacro ert-deftest-n-times (name runs body)
  (declare (indent 2))
  (let ((fun-sym (gensym "test")))
    `(ert-deftest ,name ()
       (let ((,fun-sym (lambda (x) (progn
				     ,body 1))))  			 
	(-dotimes ,runs ,fun-sym)))))

(defalias 'generate-array-of-test-cl-constantlys (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers))

(cl-defun generate-test-data (&optional &key item-transformer &key list-transformer
				     &key min-length &key max-length)
  (let* ((min-items (or min-length 2))
	 (max-items (or max-length 255))
	 (item-func (or item-transformer #'identity))
	 (list-func (or list-transformer #'seq-shuffle))
	 (range-length (random-nat-number-in-range (list min-items max-items)))
	 (list-items (random-nat-number-list range-length)))
    (funcall (-on list-func (apply-partially #'mapcar item-func)) list-items)))

(defalias 'generate-test-list-of-nat-numbers (apply-partially #'generate-test-data))
(defalias 'generate-test-list-of-floats-between-zero-and-one (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-max-array-value #'seq-shuffle)))
(defalias 'generate-test-list-of-floats (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-random-value #'seq-shuffle)))
(defalias 'generate-test-list-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string))
(defalias 'generate-test-list-of-lists-nat-numbers (apply-partially #'generate-test-data :list-transformer #'seq-split-random))

(defconst list-generators
  (list #'generate-test-list-of-nat-numbers
	#'generate-test-list-of-floats-between-zero-and-one
	#'generate-test-list-of-floats
	#'generate-test-list-of-lists-nat-numbers))

(defalias 'generate-one-random-list (-compose (-juxt #'identity #'seq-length) (apply-partially #'call-random-function list-generators)))

(defalias 'generate-test-string (apply-partially #'generate-test-data :list-transformer (-compose #'seq--into-string #'seq-shuffle)))

(defalias 'generate-test-vector-of-nat-numbers (apply-partially #'generate-test-data :list-transformer (-compose #'seq--into-vector #'seq-shuffle)))
(defconst vector-generators
  (list #'generate-test-vector-of-nat-numbers #'generate-test-vector-of-nat-numbers))

(defalias 'generate-test-alist-of-nat-numbers (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse #'seq-shuffle))))
(defalias 'generate-test-alist-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse #'seq-shuffle))))
(defalias 'generate-test-alist-of-string-nat-number-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt (-compose #'seq-map-char-to-string #'seq-reverse) #'seq-shuffle))))
(defalias 'generate-test-alist-of-nat-number-string-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse (-compose #'seq-map-char-to-string #'seq-shuffle)))))
(defalias 'generate-test-alist-of-string-vector-of-nat-numbers-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-seq-zip-shortest-pair (-juxt (-compose #'seq-map-char-to-string #'seq-shuffle) (-compose #'-applify-seq-n-random-chunks-of-random-size (-juxt #'seq-length #'-applify-vector))))))

(defconst alist-generators
  (list #'generate-test-alist-of-nat-numbers
	#'generate-test-alist-of-strings
	#'generate-test-alist-of-string-nat-number-cons
	#'generate-test-alist-of-nat-number-string-cons
	#'generate-test-alist-of-string-vector-of-nat-numbers-cons))

(defalias 'generate-random-alist (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function alist-generators)))

(defalias 'generate-test-plist-of-nat-numbers (-compose #'map-into-plist #'generate-test-alist-of-nat-numbers))
(defalias 'generate-test-plist-of-strings (-compose #'map-into-plist #'generate-test-alist-of-strings))
(defalias 'generate-test-plist-from-string-nat-number-pairs (-compose #'map-into-plist #'generate-test-alist-of-string-nat-number-cons))
(defalias 'generate-test-plist-from-nat-number-string-pairs (-compose #'map-into-plist #'generate-test-alist-of-nat-number-string-cons))

  (defconst plist-generators
    (list #'generate-test-plist-of-nat-numbers
	#'generate-test-plist-of-strings
	#'generate-test-plist-from-string-nat-number-pairs
	#'generate-test-plist-from-nat-number-string-pairs))

(defalias 'generate-random-plist (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function plist-generators)))

(defalias 'generate-test-hash-table-of-nat-numbers (-compose #'map-into-hash-table #'generate-test-alist-of-nat-numbers))
(defalias 'generate-test-hash-table-of-strings (-compose #'map-into-hash-table #'generate-test-alist-of-strings))
(defalias 'generate-test-hash-table-from-string-nat-number-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-string-nat-number-cons))
(defalias 'generate-test-hash-table-from-nat-number-string-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-nat-number-string-cons))
(defalias 'generate-test-hash-table-from-string-vector-of-nat-numbers-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-string-vector-of-nat-numbers-cons))

(defconst hash-table-generators
    (list #'generate-test-hash-table-of-nat-numbers
	#'generate-test-hash-table-of-strings
	#'generate-test-hash-table-from-string-nat-number-pairs
	#'generate-test-hash-table-from-nat-number-string-pairs
	#'generate-test-hash-table-from-string-vector-of-nat-numbers-pairs))

  (defalias 'generate-one-random-hash-table (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function hash-table-generators)))

(defalias 'generate-test-con-of-nat-numbers (apply-partially #'generate-test-data :list-transformer #'random-con-from-array))  
(defalias 'generate-test-con-of-floats (apply-partially #'generate-test-data :list-transformer (-compose #'random-con-from-array #'divide-array-values-by-max-array-value)))
(defalias 'generate-test-con-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string :list-transformer #'random-con-from-array))

(defalias 'generate-test-string-nat-number-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt (-compose #'char-to-string #'-first-item) #'-second-item) #'seq-two-random-values)))
(defalias 'generate-test-nat-number-string-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt #'-first-item (-compose #'char-to-string #'-second-item)) #'seq-two-random-values)))
(defalias 'generate-test-string-vector-of-nat-numbers-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt (-compose #'char-to-string #'-first-item) (-compose #'-applify-vector #'cdr)))))

(defconst seq-generators
  (append list-generators vector-generators (list #'generate-test-string)))

(defconst map-generators
  (append alist-generators plist-generators hash-table-generators))

(defconst generator-types    
  (list "list" "alist" "plist" "hash-table" "vector" "map" "seq"))


(defconst type-to-pred-mapping
  (list `("list" . ,#'proper-list-p)
	`("alist" . ,#'alistp)
	`("plist" . ,#'plistp)
	`("hash-table" . ,#'hash-table-p)
	`("vector" . ,#'vectorp)
	`("map" . ,#'mapp)
	`("seq" . ,#'seqp)))

(defconst type-generator-mapping
  (list `("list" . ,list-generators)
	`("alist" . ,alist-generators)
	`("plist" . ,plist-generators)
	`("hash-table" . ,hash-table-generators)
	`("vector" . ,vector-generators)
	`("map" . ,map-generators)
	`("seq" . ,seq-generators)))
  

(defalias 'assoc-cdr (-compose #'cdr #'assoc))
(defalias 'get-random-generator-type (-partial #'nested-seq-one-random-value generator-types))
(defalias 'get-generators-of-type-x (-rpartial #'assoc-cdr type-generator-mapping))
(defalias 'get-predicate-for-generator-type (-rpartial #'assoc-cdr type-to-pred-mapping))



(defalias 'call-one-random-generator-of-type-x-one-time (-compose #'call-random-function #'get-generators-of-type-x))

(defun call-one-random-generator-of-type-x-n-times (type calls)
  (funcall (-compose (-partial #'call-random-function-n-times calls) #'get-generators-of-type-x) type))

(defalias 'call-one-random-seq-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "seq"))
(defalias 'call-one-random-map-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "map"))
(defalias 'call-one-random-hash-table-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "hash-table"))

(defalias 'call-one-random-list-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "list"))
(defalias 'call-one-random-hash-table-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "hash-table"))
(defalias 'call-one-random-plist-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "plist"))
(defalias 'call-one-random-alist-generator-n-times (-partial #'call-one-random-generator-of-type-x-n-times "alist"))

(defalias 'call-one-random-hash-table-generator-twice (-partial #'call-one-random-generator-of-type-x-n-times "hash-table" 2))
(defalias 'call-one-random-plist-generator-twice (-partial #'call-one-random-generator-of-type-x-n-times "plist" 2))
(defalias 'call-one-random-alist-generator-twice (-partial #'call-one-random-generator-of-type-x-n-times "alist" 2))

(defalias 'generate-random-seq (-compose (-juxt #'identity #'seq-length #'seq-type) (apply-partially #'call-random-function seq-generators)))

(defalias 'generate-n-random-seq-types (-compose (-partial #'seq-map (-juxt #'identity #'seq-length #'seq-type)) (-rpartial #'call-n-random-functions seq-generators)))
(defalias 'generate-two-random-seq-types (-partial #'generate-n-random-seq-types 2))

(defalias 'generate-one-random-seq-type-n-times (-compose (-juxt #'identity #'seq-sum-seq-lengths (-compose #'seq-type #'car)) #'call-one-random-seq-generator-n-times))

(defalias 'generate-one-random-seq-type-n-random-times (-compose (-juxt #'generate-one-random-seq-type-n-times #'identity) #'random-nat-number-in-range-10))

(defalias 'generate-random-map (-compose (-juxt #'identity #'map-length #'map-type) (-partial #'call-random-function map-generators)))

(defalias 'generate-one-random-map-type-n-times (-compose (-juxt #'identity #'seq-sum-map-sizes (-compose #'map-type #'car)) #'call-one-random-map-generator-n-times))
(defalias 'generate-one-random-map-type-twice (-partial #'generate-one-random-map-type-n-times 2))

(defalias 'generate-one-random-map-type-n-random-times (-compose (-juxt 'generate-one-random-map-type-n-times #'identity) #'random-nat-number-in-range-10))
(defalias 'generate-one-random-map-type-two-times (-partial #'generate-one-random-map-type-n-random-times 2))

(defmacro new-type (args)
  (-let* (([type con-name decon-name predicate] args))
    `(defmacro ,con-name (value)
       (defun ,decon-name (sum)
	 (aref sum 1))
       `(if (funcall ,',predicate ,value)
	    (make-record ,',type 1 ,value)
	  (error (format "%s can not be converted into a %s" (type-of ,value) (symbol-name ,',type)))))))

;; (defconst default-new-type-arguments    
;;   (list (cons "sum" ["sum" "Sum" "get-sum" #'integerp])
;; 	(cons "product" [ "product" "Product" "get-product" #'integerp))
;; 	(cons "min"  ["min" "Min" "get-min" #'integerp))
;; 	(cons "max" . ,[ 'max 'Max 'get-max #'integerp))
;; 	(cons "const" . ,[ 'const 'Const 'get-const #'integerp))
;; 	(cons "first" . ,[ 'first 'First 'get-first #'integerp))
;; 	(cons "last" . ,[ 'last 'Last 'get-last #'integerp))
;; 	(cons "any" [ 'any  'Any 'get-any #'booleanp))
;; 	(cons "all"  ["all" "All" "get-all" #'booleanp))
;; 	(cons "plist"  ["plist" "Plist" "get-plist" #'plistp]))

(defconst default-new-type-arguments    
  (list (cons "sum" ['sum Sum get-sum #'integerp])))

(new-type ['sum Sum get-sum #'integerp])
(new-type ['all All get-all #'booleanp])
(new-type ['plist Plist get-plist #'plistp])

(defun con-of-strings-p (x)
  (pcase x
    ((and (pred -cons-pair-p) `(,(pred stringp) . ,(pred stringp))) t)
    (_ nil)))

(defun string-vector-con-p (x)
  (pcase x
    ((and (pred -cons-pair-p) `(,(pred stringp) . ,(pred vectorp))) t)
    (_ nil)))

(defalias 'seq-every-p-cons-of-strings-p (apply-partially #'seq-every-p #'con-of-strings-p))

(cl-defgeneric semigroup-concat (a b))

(cl-defmethod semigroup-concat ((a string) b)
  (concat a b))

(cl-defmethod semigroup-concat ((a cons) b)
  (cond
   ((con-of-strings-p a)
    (concat-two-cons-of-strings a b))
   ((string-vector-con-p a)
    (concat-two-string-vector-cons a b))
   ((alistp a) (map-merge 'alist a b))
   ((proper-list-p a) (append a b))
   (_ (error "Not a semigroup"))))

(cl-defmethod semigroup-concat ((a vector) b)
  (vconcat a b))

(cl-defmethod semigroup-concat ((a hash-table) b)
  (map-merge 'hash-table a b))

(defalias 'repeat-con-of-semigroups (cl-function (lambda (n (a . b)) (cons (stimes n a) (stimes n b)))))



;; test copy-alist
;; test copy-list
(cl-defmethod stimes (n (a cons))
  (cond
   ((con-of-strings-p a) (repeat-con-of-semigroups n a))
   ((string-vector-con-p a) (repeat-con-of-semigroups n a))
   ((alistp a) (make-list n a))
   ((proper-list-p a) (make-list n a))
   (_ (error "Not a semigroup"))))

(cl-defmethod stimes (n (a string))
  (funcall (-compose #'-applify-concat #'make-list) n a))

(cl-defmethod stimes (n (a vector))
  (make-vector n a))

;; test copy-hash-table
(cl-defmethod stimes (n (a hash-table))
  (make-list n a))

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
