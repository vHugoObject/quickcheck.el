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
  (list 1 10))    
(defconst TWENTYFIVERANGE
  (list 1 25))    
(defconst FIFTYRANGE
  (list 1 50))
(defconst DEFAULTRANDOMNUMBERRANGE
  (list 1 255))
(defconst FIVEHUNDREDRANGE
  (list 1 500))
(defconst THOUSAND
  (float 1000)
  "Float creator")
(defconst LESSTHANZERO
  (list most-negative-fixnum 0))

(defconst NEGATIVENUMS
  (list most-negative-fixnum -1))

(defalias '2+ (apply-partially #'+ 2))
(defalias 'print (apply-partially #'message "%s"))

(defalias 'not-equal #'/=)
(defalias 'less-than #'<)
(defalias 'less-than-or-equal #'<=)
(defalias 'greater-than #'>)
(defalias 'greater-than-or-equal #'>=)
(defalias 'greater-than-or-equal-one (-rpartial #'>= 1))
(defalias 'greater-than-or-equal-zero (-rpartial #'>= 0))
(defalias 'less-than-or-equal-zero (-rpartial #'<= 0))
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

(defalias '-applify-equal (-applify #'equal))
(defalias '-applify-rpartial (-applify #'-rpartial))
(defalias '-applify-partial (-applify #'-partial))

(defalias '-applify-subtract (-applify #'-))
(defalias '-applify-multiply (-applify #'*))
(defalias '-applify-iterate-plus-one  (-applify #'-iterate-plus-one))
(defalias '-applify-divide (-applify #'/))


(defalias '-applify-zip  (-applify #'-zip))
(defalias '-applify-zip-pair  (-applify #'-zip-pair))

(defalias '-applify-juxt  (-applify #'-juxt))

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

(defalias '-any-true (-partial #'-any-p #'identity))
(defalias '-every-true (-partial #'-every-p #'identity))

(defalias 'flatten-one-level (-partial #'-flatten-n 1))

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

(defun -juxt-every (&rest fns)
  (let* ((-juxt-func (funcall #'-applify-juxt fns)))
    (-compose #'-every-true -juxt-func)))

(defun -juxt-any (&rest fns)
  (let* ((-juxt-func (funcall #'-applify-juxt fns)))
    (-compose #'-any-true -juxt-func)))

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

(defalias 'type-of-as-string (-compose #'symbol-name #'cl-type-of))

(defun random-float-between-0-and-1 ()    
  (funcall (-compose #'convert-calc-value-into-lisp #'math-random-float)))

(defalias 'random-float (-compose #'-applify-multiply (-juxt #'random #'random-float-between-0-and-1)))

(cl-defun random-nat-number-in-range ((min max))
  (if (eql min max)
      min
    (funcall (-compose (apply-partially #'scale-float-to-range (list min max))  #'random-float-between-0-and-1))))

(defalias 'random-nat-number-in-range-10 (apply-partially #'random-nat-number-in-range TENRANGE))

(defalias 'random-number-less-than-or-equal-zero (apply-partially #'random-nat-number-in-range LESSTHANZERO))

(defalias 'random-negative-number (apply-partially #'random-nat-number-in-range NEGATIVENUMS))

(defalias 'random-nat-number-in-range-25 (apply-partially #'random-nat-number-in-range TWENTYFIVERANGE))

(defalias 'random-nat-number-in-range-50 (apply-partially #'random-nat-number-in-range FIFTYRANGE))

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

(defalias 'seq-count-nat-numbers (apply-partially #'seq-count #'natnump))
(defalias 'seq-count-floats (apply-partially #'seq-count #'floatp))
(defalias 'seq-count-strings (apply-partially #'seq-count #'stringp))  
(defalias 'seq-count-cons (apply-partially #'seq-count #'consp))
(defalias 'seq-count-between-zero-and-one (apply-partially #'seq-count #'between-zero-and-one))
(defalias 'seq-count-greater-than-or-equal-one (apply-partially #'seq-count #'greater-than-or-equal-one))

(defalias 'seq-map-add-one (-partial #'seq-map #'1+))
(defalias 'seq-map-seq-length (-partial #'seq-map #'seq-length))
(defalias 'seq-map-member (-partial #'seq-map (lambda (x) (-partial #'member x))))
(defalias 'seq-map-cl-type-of (-partial #'seq-map #'cl-type-of))

(defalias 'seq-map-map-size (-partial #'seq-map #'map-length))

(defalias 'seq-map-seq--into-list (-partial #'seq-map #'seq--into-list))
(defalias 'seq-map-char-to-string (-partial #'seq-map #'char-to-string))
(defalias 'seq-map-string-to-char (-partial #'seq-map #'string-to-char))
(defalias 'seq-map-cl-constantly (-partial #'seq-map #'cl-constantly))

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
  (pcase-exhaustive (cl-type-of seq)
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

(defalias 'seq-take-one-random-value (-compose #'seq-take-one #'seq-shuffle))

(defalias 'seq-take-one-random-value-from-seq (-compose #'seq-first #'seq-take-one-random-value))

(defalias 'seq-two-random-values (-compose #'seq-take-two #'seq-shuffle))

(defalias 'seq-random-chunk-length (-compose #'random-nat-number-in-range-from-one #'seq-length))

(defun seq-random-chunk-of-size-n (chunk-length seq)
  (funcall (-compose #'seq-take-one-random-value-from-seq #'seq-split) seq chunk-length))

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

(cl-defgeneric seq-equal (seq-one seq-two)
  (and (cl-subsetp seq-one seq-two) (cl-subsetp seq-two seq-one)))

(cl-defmethod seq-equal ((seq-one string) seq-two)
  (string-equal seq-one seq-two))

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

(defalias 'seq-zip-shortest-pair-into-hash-table (-compose #'map-into-hash-table #'seq-zip-shortest-pair))

(defmacro seq-splice! (op seq)
  `(,op ,@(seq-map #'identity seq)))

(defalias 'map-into-alist (-rpartial #'map-into 'alist))
(defalias 'map-into-plist (-rpartial #'map-into 'plist))  
(defalias 'map-into-hash-table (-rpartial #'map-into 'hash-table))

(defalias 'map-merge-alist (-partial #'map-merge 'alist))
(defalias 'map-merge-plist (-partial #'map-merge 'plist))
(defalias 'map-merge-hash-table (-partial #'map-merge 'hash-table))

(defalias 'assoc-cdr (-compose #'cdr #'assoc))

(defalias 'alistp (-partial #'seq-every-p #'-cons-pair-p))

(defun map-type (map)
  (cond
   ((hash-table-p map) 'hash-table)
   ((alistp map) 'alist)
   ((plistp map) 'plist)
   (_ (error "not a map"))))

(defalias 'map-one-random-key (-compose #'seq-take-one-random-value-from-seq #'map-keys))
(defalias 'seq-map-one-random-map-key (-partial #'seq-map #'map-one-random-key))

(defalias 'map-one-random-value (-compose #'-applify-map-elt (-juxt #'identity #'map-one-random-key)))

(defun map-on (op keys-trans values-trans map)
  "Apply one function to map keys, one function to map values and one function the result"
   (funcall (-compose op (-juxt (-compose keys-trans #'map-keys) (-compose values-trans #'map-values))) map))

(defalias 'concat-two-cons-of-strings (-compose (-partial #'map-on #'-applify-cons #'-applify-concat #'-applify-concat) #'list))

(defalias 'concat-two-string-vector-cons (-compose (-partial #'map-on #'-applify-cons #'-applify-concat #'-applify-vconcat) #'list))

(defmacro plural (macro props)
  `(progn
     ,@(seq-map (lambda (p) `(,macro ,p))
	       (symbol-value props))))

(defmacro plural-splice! (macro props)
  `(progn
     ,@(seq-map (lambda (p) `(seq-splice! ,macro ,p))
	       (symbol-value props))))

(defalias 'last-elt (-compose #'car #'last))
(defalias 'first-and-last-elt (-juxt #'car #'last-elt))
(defalias 'head-type-equal-tail-type (-compose #'-applify-equal #'seq-map-cl-type-of #'first-and-last-elt))
(defalias 'homogenic-list-p (-juxt-every #'proper-list-p #'head-type-equal-tail-type))
(defalias 'make-list-then-flatten (-compose #'flatten-list #'make-list))



(defmacro ert-deftest-n-times (name runs body)
  (declare (indent 2))    
  (let ((fun-sym (gensym "test")))
    `(ert-deftest ,name ()
       (let ((,fun-sym (lambda (x) (progn
				     ,body 1))))  			 
	(-dotimes ,runs ,fun-sym)))))

(defalias 'call-random-function (-compose #'funcall #'seq-take-one-random-value-from-seq))

(defun call-random-function-n-times (calls list)
  (funcall (-compose (-partial #'-times-no-args calls) #'seq-take-one-random-value-from-seq) list))

(defun call-n-random-functions (n funcs)
  (funcall (-compose (-partial #'-map #'funcall) (-partial #'-take n) #'shuffle-list) funcs))

(defalias 'generate-one-test-cl-constantly (-compose (-juxt #'identity #'cl-constantly) #'random))

(defalias 'generate-array-of-test-cl-constantlys (-compose (-juxt #'identity #'seq-map-cl-constantly) #'generate-test-list-of-nat-numbers))

(defalias 'generate-test-list-of-integer-member-predicates (-compose (-juxt #'seq-map-member #'identity) #'generate-test-list-of-nat-numbers))

(cl-defun generate-test-data (&optional &key item-transformer &key list-transformer
				     &key min-length &key max-length)
  (let* ((min-items (or min-length 2))
	 (max-items (or max-length 50))
	 (item-func (or item-transformer #'identity))
	 (list-func (or list-transformer #'seq-shuffle))
	 (range-length (random-nat-number-in-range (list min-items max-items)))
	 (list-items (random-nat-number-list range-length)))
    (funcall (-on list-func (apply-partially #'mapcar item-func)) list-items)))

(defalias 'generate-test-list-of-nat-numbers (-partial #'generate-test-data))
(defalias 'generate-test-list-of-floats-between-zero-and-one (-partial #'generate-test-data :list-transformer (-compose #'divide-array-values-by-max-array-value #'seq-shuffle)))
(defalias 'generate-test-list-of-floats (-partial #'generate-test-data :list-transformer (-compose #'divide-array-values-by-random-value #'seq-shuffle)))
(defalias 'generate-test-list-of-strings (-partial #'generate-test-data :item-transformer #'char-to-string))
(defalias 'generate-test-list-of-lists-nat-numbers (-partial #'generate-test-data :list-transformer #'seq-split-random))

(defconst LIST-GENERATORS
  (vector #'generate-test-list-of-nat-numbers
	#'generate-test-list-of-floats-between-zero-and-one
	#'generate-test-list-of-floats
	#'generate-test-list-of-strings
	#'generate-test-list-of-lists-nat-numbers))


(defconst DISTINCT-LIST-GENERATORS
  (vector #'generate-test-list-of-nat-numbers
	#'generate-test-list-of-floats
	#'generate-test-list-of-strings
	#'generate-test-list-of-lists-nat-numbers))

(defalias 'generate-test-sum (-compose #'wrap-const #'random))
(defalias 'generate-test-product (-compose #'wrap-product #'random))
(defalias 'generate-test-min (-compose #'wrap-min #'random))
(defalias 'generate-test-max (-compose #'wrap-max #'random))
(defalias 'generate-test-first (-compose #'wrap-first #'random))
(defalias 'generate-test-last (-compose #'wrap-last #'random))

(defconst SINGLE-WRAPPED-INTEGER-GENERATORS
  (vector #'generate-test-sum
	#'generate-test-product
	#'generate-test-min
	#'generate-test-max
	#'generate-test-first
	#'generate-test-last))



(defalias 'generate-test-list-of-sums (-partial #'generate-test-data :item-transformer #'wrap-sum))
(defalias 'generate-test-list-of-products (-partial #'generate-test-data :item-transformer #'wrap-product))
(defalias 'generate-test-list-of-mins (-partial #'generate-test-data :item-transformer #'wrap-min))
(defalias 'generate-test-list-of-maxes (-partial #'generate-test-data :item-transformer #'wrap-max))
(defalias 'generate-test-list-of-firsts (-partial #'generate-test-data :item-transformer #'wrap-first))
(defalias 'generate-test-list-of-lasts (-partial #'generate-test-data :item-transformer #'wrap-last))


(defconst LIST-OF-WRAPPED-INTEGER-GENERATORS
  (vector #'generate-test-list-of-sums
	#'generate-test-list-of-products
	#'generate-test-list-of-mins
	#'generate-test-list-of-maxes
	#'generate-test-list-of-firsts
	#'generate-test-list-of-lasts))

(defconst WRAPPED-INTEGERS-VECTOR
  (vector 'sum 'product 'min 'max 'first 'last))

(defalias 'wrapped-integerp (-compose (-partial #'seq-contains-p WRAPPED-INTEGERS-VECTOR) #'car))

(defalias 'random-boolean (-partial #'seq-take-one-random-value-from-seq (list 't 'nil)))

(defalias 'generate-random-any (-compose #'wrap-any #'random-boolean))
(defalias 'generate-random-all (-compose #'wrap-all #'random-boolean))
(defalias 'generate-n-random-anys (-compose (-partial #'seq-map #'wrap-any) (-rpartial #'-times-no-args #'random-boolean) #'random-nat-number-in-range-25))
(defalias 'generate-n-random-alls (-compose (-partial #'seq-map #'wrap-all) (-rpartial #'-times-no-args #'random-boolean) #'random-nat-number-in-range-25))
(defconst SINGLE-WRAPPED-BOOLEAN-GENERATORS
  (vector #'generate-random-any #'generate-random-all))
(defconst LIST-OF-WRAPPED-BOOLEAN-GENERATORS
  (vector #'generate-n-random-anys #'generate-n-random-alls))

(defalias 'generate-test-string (apply-partially #'generate-test-data :list-transformer (-compose #'seq--into-string #'seq-shuffle)))

(defalias 'generate-test-vector-of-nat-numbers (-compose #'-applify-vector #'generate-test-list-of-nat-numbers))
(defalias 'generate-test-vector-of-floats-between-zero-and-one (-compose #'-applify-vector #'generate-test-list-of-floats-between-zero-and-one))
(defalias 'generate-test-vector-of-floats (-compose #'-applify-vector #'generate-test-list-of-floats))
(defalias 'generate-test-vector-of-strings (-compose #'-applify-vector #'generate-test-list-of-strings))

(defconst VECTOR-GENERATORS
  (vector #'generate-test-vector-of-nat-numbers
	#'generate-test-vector-of-floats-between-zero-and-one
	#'generate-test-vector-of-floats
	#'generate-test-vector-of-strings))

(defalias 'generate-test-alist-of-nat-numbers (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse #'seq-shuffle))))
(defalias 'generate-test-alist-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse #'seq-shuffle))))
(defalias 'generate-test-alist-of-string-nat-number-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt (-compose #'seq-map-char-to-string #'seq-reverse) #'seq-shuffle))))
(defalias 'generate-test-alist-of-nat-number-string-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'seq-reverse (-compose #'seq-map-char-to-string #'seq-shuffle)))))
(defalias 'generate-test-alist-of-string-vector-of-nat-numbers-cons (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-seq-zip-shortest-pair (-juxt (-compose #'seq-map-char-to-string #'seq-shuffle) (-compose #'-applify-seq-n-random-chunks-of-random-size (-juxt #'seq-length #'-applify-vector))))))

(defconst ALIST-GENERATORS
  (vector #'generate-test-alist-of-nat-numbers
	#'generate-test-alist-of-strings
	#'generate-test-alist-of-string-nat-number-cons
	#'generate-test-alist-of-nat-number-string-cons
	#'generate-test-alist-of-string-vector-of-nat-numbers-cons))

(defalias 'generate-test-unwrapped-plist-of-nat-numbers (-compose #'map-into-plist #'generate-test-alist-of-nat-numbers))
(defalias 'generate-test-unwrapped-plist-of-strings (-compose #'map-into-plist #'generate-test-alist-of-strings))
(defalias 'generate-test-unwrapped-plist-from-string-nat-number-pairs (-compose #'map-into-plist #'generate-test-alist-of-string-nat-number-cons))
(defalias 'generate-test-unwrapped-plist-from-nat-number-string-pairs (-compose #'map-into-plist #'generate-test-alist-of-nat-number-string-cons))

  (defconst UNWRAPPED-PLIST-GENERATORS
    (vector #'generate-test-unwrapped-plist-of-nat-numbers
	#'generate-test-unwrapped-plist-of-strings
	#'generate-test-unwrapped-plist-from-string-nat-number-pairs
	#'generate-test-unwrapped-plist-from-nat-number-string-pairs))

(defalias 'generate-random-unwrapped-plist (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function UNWRAPPED-PLIST-GENERATORS)))

(defalias 'generate-test-wrapped-plist-of-nat-numbers (-compose #'wrap-plist #'generate-test-unwrapped-plist-of-nat-numbers))
(defalias 'generate-test-wrapped-plist-of-strings (-compose #'wrap-plist #'generate-test-unwrapped-plist-of-strings))
(defalias 'generate-test-wrapped-plist-from-string-nat-number-pairs (-compose #'wrap-plist #'generate-test-unwrapped-plist-from-string-nat-number-pairs))
(defalias 'generate-test-wrapped-plist-from-nat-number-string-pairs (-compose #'wrap-plist #'generate-test-unwrapped-plist-from-nat-number-string-pairs))

(defconst WRAPPED-PLIST-GENERATORS
    (vector #'generate-test-wrapped-plist-of-nat-numbers
	#'generate-test-wrapped-plist-of-strings
	#'generate-test-wrapped-plist-from-string-nat-number-pairs
	#'generate-test-wrapped-plist-from-nat-number-string-pairs))

(defalias 'generate-random-wrapped-plist (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function WRAPPED-PLIST-GENERATORS)))

(defalias 'generate-test-hash-table-of-nat-numbers (-compose #'map-into-hash-table #'generate-test-alist-of-nat-numbers))
(defalias 'generate-test-hash-table-of-strings (-compose #'map-into-hash-table #'generate-test-alist-of-strings))
(defalias 'generate-test-hash-table-from-string-nat-number-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-string-nat-number-cons))
(defalias 'generate-test-hash-table-from-nat-number-string-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-nat-number-string-cons))
(defalias 'generate-test-hash-table-from-string-vector-of-nat-numbers-pairs (-compose #'map-into-hash-table #'generate-test-alist-of-string-vector-of-nat-numbers-cons))

(defconst HASH-TABLE-GENERATORS
    (vector #'generate-test-hash-table-of-nat-numbers
	#'generate-test-hash-table-of-strings
	#'generate-test-hash-table-from-string-nat-number-pairs
	#'generate-test-hash-table-from-nat-number-string-pairs
	#'generate-test-hash-table-from-string-vector-of-nat-numbers-pairs))

  (defalias 'generate-one-random-hash-table (-compose (-juxt #'identity #'map-length) (apply-partially #'call-random-function HASH-TABLE-GENERATORS)))

(defalias 'generate-test-con-of-nat-numbers (apply-partially #'generate-test-data :list-transformer #'random-con-from-array))  
(defalias 'generate-test-con-of-floats (apply-partially #'generate-test-data :list-transformer (-compose #'random-con-from-array #'divide-array-values-by-max-array-value)))
(defalias 'generate-test-con-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string :list-transformer #'random-con-from-array))

(defalias 'generate-test-string-nat-number-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt (-compose #'char-to-string #'-first-item) #'-second-item) #'seq-two-random-values)))
(defalias 'generate-test-nat-number-string-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt #'-first-item (-compose #'char-to-string #'-second-item)) #'seq-two-random-values)))
(defalias 'generate-test-string-vector-of-nat-numbers-con (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-cons (-juxt (-compose #'char-to-string #'-first-item) (-compose #'-applify-vector #'cdr)))))

(defconst SEQ-GENERATORS
  (vconcat LIST-GENERATORS VECTOR-GENERATORS (vector #'generate-test-string)))

(defconst MAP-GENERATORS
  (vconcat ALIST-GENERATORS HASH-TABLE-GENERATORS WRAPPED-PLIST-GENERATORS))


(defconst PRIMITIVE-GENERATOR-TYPES    
  (vector "list" "alist" "hash-table" "vector" "map" "seq"))


(defconst TYPE-TO-PRED-MAPPING
  (list
   (cons "fixnum" #'proper-list-p)
   (cons "string" #'stringp)
   (cons "float" #'floatp)
   (cons "list" #'proper-list-p)
   (cons "cons" #'consp)
   (cons "alist" #'alistp)
   (cons "wrapped-plist" #'wrapped-plistp)
   (cons "hash-table" #'hash-table-p)
   (cons "vector" #'vectorp)
   (cons "map" #'mapp)
   (cons "seq" #'seqp)
   (cons "sum" #'sump)
   (cons "product" #'productp)
   (cons "min" #'minp)
   (cons "max" #'maxp)
   (cons "const" #'constp)
   (cons "first" #'firstp)
   (cons "last" #'lastp)
   (cons "any" #'anyp)
   (cons "all" #'allp)
   (cons "just" #'justp)
   (cons "maybe" #'maybep)
   (cons "wrapped-integer" #'wrapped-integerp)
   (cons "wrapped-boolean" #'wrapped-booleanp)
   (cons "list-of-sums" #'sump)
   (cons "list-of-products" #'productp)
   (cons "list-of-mins" #'minp)
   (cons "list-of-maxes" #'maxp)
   (cons "list-of-firsts" #'firstp)
   (cons "list-of-lasts" #'lastp)
   (cons "list-of-anys" #'anyp)
   (cons "list-of-alls" #'allp)
   (cons "list-of-wrapped-booleans" #'wrapped-booleanp)
   (cons "list-of-wrapped-integers" #'wrapped-integerp)))


(defconst TYPE-GENERATOR-MAPPING
  (list
   (cons "fixnum" (vector #'random))
   (cons "float" (vector #'random-float-between-0-and-1))
   (cons "list" LIST-GENERATORS)
   (cons "alist" ALIST-GENERATORS)
   (cons "hash-table" HASH-TABLE-GENERATORS)
   (cons "wrapped-plist" WRAPPED-PLIST-GENERATORS)
   (cons "vector" VECTOR-GENERATORS)
   (cons "map" MAP-GENERATORS)
   (cons "seq" SEQ-GENERATORS)
   (cons "any" (vector #'generate-random-any))
   (cons "all" (vector #'generate-random-all))
   (cons "sum" (vector #'generate-test-sum))
   (cons "product" (vector #'generate-test-product))
   (cons "min" (vector #'generate-test-min))
   (cons "max" (vector #'generate-test-max))
   (cons "first" (vector #'generate-test-first))
   (cons "last" (vector #'generate-test-last))
   (cons "wrapped-boolean" SINGLE-WRAPPED-BOOLEAN-GENERATORS)
   (cons "wrapped-integer" SINGLE-WRAPPED-INTEGER-GENERATORS)
   (cons "list-of-anys" (vector #'generate-n-random-anys))
   (cons "list-of-alls" (vector #'generate-n-random-alls))
   (cons "list-of-wrapped-booleans" LIST-OF-WRAPPED-BOOLEAN-GENERATORS)
   (cons "list-of-sums" (vector #'generate-test-list-of-sums))
   (cons "list-of-products" (vector #'generate-test-list-of-products))
   (cons "list-of-mins" (vector #'generate-test-list-of-mins))
   (cons "list-of-maxes" (vector #'generate-test-list-of-maxes))
   (cons "list-of-firsts" (vector #'generate-test-list-of-firsts))
   (cons "list-of-lasts" (vector #'generate-test-list-of-lasts))
   (cons "list-of-wrapped-integers" LIST-OF-WRAPPED-INTEGER-GENERATORS)))
  
(defalias 'get-random-generator-type (-partial (-compose #'seq-take-one-random-value-from-seq #'map-keys) TYPE-GENERATOR-MAPPING))
(defalias 'get-generators-of-type-x (-rpartial #'assoc-cdr TYPE-GENERATOR-MAPPING))
(defalias 'get-predicate-for-type (-rpartial #'assoc-cdr TYPE-TO-PRED-MAPPING))
(defalias 'get-random-generator (-compose #'seq-take-one-random-value-from-seq (-partial #'map-one-random-value TYPE-GENERATOR-MAPPING)))
(defalias 'generate-random-value (-compose #'funcall #'get-random-type-generator))

(defun generate-one-random-x (type generators-list)
  (let ((pred (get-predicate-for-type type)))
    (list (call-random-function generators-list) pred)))

(defun generate-one-random-x-type-n-times (type generators-list calls)
  (let ((pred (get-predicate-for-type type)))
    (list (call-random-function-n-times calls generators-list) pred)))

(defun generate-one-random-x-type-n-random-times (type generators-list)
  (let ((pred (get-predicate-for-type type))
	(calls (random-nat-number-in-range-25)))
    (list (call-random-function-n-times calls generators-list) pred calls)))

(cl-defmacro create-generate-one-random-x ((type . generators-list))
  (cl-with-gensyms (alias-name)      
    `(let ((,alias-name (intern (format "generate-one-random-%s" ,type))))
       (defalias ,alias-name (-partial #'generate-one-random-x ,type ,generators-list)))))

(cl-defmacro create-generate-one-random-x-type-n-times ((type . generators-list))
  (cl-with-gensyms (alias-name)      
    `(let ((,alias-name (intern (format "generate-one-random-%s-type-n-times" ,type))))
       (defalias ,alias-name (-partial #'generate-one-random-x-type-n-times ,type ,generators-list)))))

(cl-defmacro create-generate-one-random-x-type-n-random-times ((type . generators-list))
  (cl-with-gensyms (alias-name)      
    `(let ((,alias-name (intern (format "generate-one-random-%s-type-n-random-times" ,type))))
       (defalias ,alias-name (-partial #'generate-one-random-x-type-n-random-times ,type ,generators-list)))))

(defmacro create-list-of-generate-one-random-x (args)    
  `(plural create-generate-one-random-x ,args))

(defmacro create-list-of-generate-one-random-x-type-n-times (args)    
  `(plural create-generate-one-random-x-type-n-times ,args))

(defmacro create-list-of-generate-one-random-x-type-n-random-times (args)    
  `(plural create-generate-one-random-x-type-n-random-times ,args))

(create-list-of-generate-one-random-x TYPE-GENERATOR-MAPPING)
(create-list-of-generate-one-random-x-type-n-times TYPE-GENERATOR-MAPPING)
(create-list-of-generate-one-random-x-type-n-random-times TYPE-GENERATOR-MAPPING)

(defalias 'generate-one-random-hash-table-generator-twice (-partial #'generate-one-random-hash-table-type-n-times 2))
(defalias 'generate-one-random-alist-generator-twice (-partial #'generate-one-random-alist-type-n-times 2))
(defalias 'generate-one-random-wrapped-plist-generator-twice (-partial #'generate-one-random-wrapped-plist-type-n-times 2))  
(defalias 'generate-one-random-map-type-twice (-partial #'generate-one-random-map-type-n-times 2))
(defalias 'generate-one-random-wrapped-boolean-type-twice (-partial #'generate-one-random-wrapped-boolean-type-n-times 2))

(defconst WRAPPED-TYPES-MAPPING
  (list (cons "const" #'wrap-const)
	(cons "maybe" #'wrap-maybe)))

(defalias 'get-wrapper-for-wrapped-type (-rpartial #'assoc-cdr WRAPPED-TYPES-MAPPING))

(defun generate-one-random-wrapped-x-type (type)
  (-let* (((wrapper-pred wrapper) (funcall (-juxt #'get-predicate-for-type #'get-wrapper-for-wrapped-type) type))
	  ((generators-list wrapped-type-pred) (funcall (-compose (-juxt #'get-generators-of-type-x #'get-predicate-for-type) #'get-random-generator-type)))
	  ((wrapped-val original-value) (funcall (-compose (-juxt #'wrapper #'identity) #'call-random-function) generators-list)))
    (list wrapped-val wrapper-pred wrapped-type-pred original-value)))

(defun generate-one-random-wrapped-x-type-n-times (type calls)
  (-let* (((wrapper-pred wrapper) (funcall (-juxt #'get-predicate-for-type #'get-wrapper-for-wrapped-type) type))
	  ((generators-list wrapped-type-pred) (funcall (-compose (-juxt #'get-generators-of-type-x #'get-predicate-for-type) #'get-random-generator-type)))
	  ((list-of-wrapped-vals original-list) (funcall (-compose (-juxt (-partial #'seq-map wrapper) #'identity) #'call-random-function-n-times) calls generators-list)))
    (list list-of-wrapped-vals wrapper-pred wrapped-type-pred original-list)))

(defun generate-one-random-wrapped-x-type-n-random-times (type)
  (let ((calls (random-nat-number-in-range-25)))
    (generate-one-random-wrapped-x-type-n-times type calls)))


(defalias 'generate-one-random-maybe-just (-partial #'generate-one-random-wrapped-x-type "maybe"))
(defalias 'generate-one-random-wrapped-const (-partial #'generate-one-random-wrapped-x-type "const"))

(defalias 'generate-one-random-wrapped-maybe-just-type-n-times (-partial #'generate-one-random-wrapped-x-type-n-times "maybe"))
(defalias 'generate-one-random-wrapped-const-type-n-times (-partial #'generate-one-random-wrapped-x-type-n-times "const"))

(defalias 'generate-one-random-wrapped-maybe-type-twice (-partial #'generate-one-random-wrapped-x-type-n-times "maybe" 2))  
(defalias 'generate-one-random-wrapped-const-type-twice (-partial #'generate-one-random-wrapped-x-type-n-times "const" 2))

(defalias 'generate-one-random-wrapped-maybe-just-type-n-random-times (-partial #'generate-one-random-wrapped-x-type-n-random-times "maybe"))
(defalias 'generate-one-random-wrapped-const-type-n-random-times (-partial #'generate-one-random-wrapped-x-type-n-random-times "const"))

(defalias 'generate-one-random-list-of-maybe-nothings (-compose (-rpartial #'-times-no-args #'create-maybe-nothing) #'random-nat-number-in-range-10))  
(defun generate-one-random-list-of-maybes ()
  (-let* ((nothings-list (generate-one-random-list-of-maybe-nothings))
	  ((justs-list wrapper-pred wrapped-type-pred justs-list-of-unwrapped-values) (generate-one-random-wrapped-maybe-just-type-n-random-times))
	  (mixed-list (funcall (-compose #'shuffle-list #'append) justs-list nothings-list)))
    (list mixed-list wrapper-pred wrapped-type-pred justs-list-of-unwrapped-values)))

(defalias 'const (lambda (a b) a))

(defalias 'wrapped-booleanp (-juxt-any #'anyp #'allp))

(defalias 'nilp (-not #'identity))

(defalias 'nothingp (-compose (-partial #'equal 'nothing) #'car))

(defalias 'can-be-maybe (-juxt-any #'justp #'nothingp))

;; needs to use plural-splice! here
(defmacro new-type (args)
  (-let* (([type con-name decon-name predicate] args))
    `(defmacro ,con-name (value)
       (defun ,decon-name (wrapped-val)
	   (cadr wrapped-val))
       `(if (funcall ,',predicate ,value)
	    (list ,',type ,value)
	  (error (format "%s can not be converted into a %s" (type-of ,value) (symbol-name ,',type)))))))

(defmacro create-new-type-lambda-wrapper (props)
  (-let (([wrapper-name con-name] props))
    `(defalias ,wrapper-name (lambda (x) (,con-name x)))))

(defmacro create-new-type-predicate (props)
  (-let (([type predicate-name] props))
    `(defalias ,predicate-name (lambda (x) (and (equal (car x) ,type) (length= x 2))))))


(defmacro create-new-types (props)
  `(plural new-type ,props))

(defmacro create-new-type-lambda-wrappers (props)
  `(plural create-new-type-lambda-wrapper ,props))

(defmacro create-new-type-predicates (props)
  `(plural create-new-type-predicate ,props))

(defconst DEFAULT-NEW-TYPE-ARGUMENTS
    (list
     ['sum Sum get-sum #'integerp]
     ['product Product get-product #'integerp]
     ['min Min get-min #'integerp]
     ['max Max get-max #'integerp]
     ['const Const get-const #'identity]
     ['first First get-first #'integerp]
     ['last Last get-last #'integerp]
     ['any Any get-any #'booleanp]
     ['all All get-all #'booleanp]
     ['plist Plist get-plist #'plistp]
     ['just Just get-just #'identity]
     ['maybe Maybe get-maybe #'can-be-maybe]))

(defconst DEFAULT-NEW-TYPE-LAMBDA-WRAPPER-ARGS
  (list
   ['wrap-sum Sum]
   ['wrap-product Product]
   ['wrap-min Min]
   ['wrap-max Max]
   ['wrap-const Const]
   ['wrap-first First]
   ['wrap-last Last]
   ['wrap-any Any]
   ['wrap-all All]
   ['wrap-plist Plist]
   ['wrap-just Just]
   ['create-maybe Maybe]))

(defconst DEFAULT-NEW-TYPE-PREDICATES
  (list
   ['sum 'sump]
   ['product 'productp]
   ['min 'minp]
   ['max 'maxp]
   ['const 'constp]
   ['first 'firstp]
   ['last 'lastp]
   ['any 'anyp]
   ['all 'allp]
   ['plist 'wrapped-plistp]
   ['just 'justp]
   ['maybe 'maybep]))


(defconst ALLNEWTYPES
  (seq-map #'seq-first DEFAULT-NEW-TYPE-ARGUMENTS))


(defalias 'is-new-type-symbol (-rpartial #'member ALLNEWTYPES))

(create-new-types DEFAULT-NEW-TYPE-ARGUMENTS)
(create-new-type-lambda-wrappers DEFAULT-NEW-TYPE-LAMBDA-WRAPPER-ARGS)
(create-new-type-predicates DEFAULT-NEW-TYPE-PREDICATES)

(defalias 'create-nothing (lambda () (list 'nothing)))

(defalias 'create-maybe-nothing (-compose #'create-maybe #'create-nothing))

(defalias 'wrap-maybe-just (-compose #'create-maybe #'wrap-just))

(defalias 'unwrap-maybe-just (-compose #'get-just #'get-maybe))

(defun wrap-maybe (val)    
  (pcase-exhaustive (nilp val)
    ('t (create-maybe-nothing))
    (_ (wrap-maybe-just val))))

(defun from-just (maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) (error "Maybe.fromJust: Nothing"))
    (`(just ,x) x)))

(defun from-maybe (default maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) default)
    (`(just ,x) x)))

(defalias 'or-else (-flip #'from-maybe))

(defun is-just (maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) 'nil)
    (_ 't)))

(defun is-nothing (maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) 't)
    (_ 'nil)))

(defun maybe-to-list (maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) '())
    (`(just ,x) (list x))))

(defun list-to-maybe (list)
  (let ((initial-val (create-maybe-nothing)))
    (-reduce-r-from (-compose #'const #'wrap-maybe) initial-val list)))

(defun map-maybes (func list)
  (funcall (-compose (-partial #'mapcar (-compose func #'unwrap-maybe-just)) (-partial #'seq-filter #'is-just)) list))

(defalias 'cat-maybes (-partial #'map-maybes #'identity))

(defun apply-maybe (default func maybe)
  (pcase-exhaustive (get-maybe maybe)
    (`(nothing) default)
    (`(just ,x) (funcall func x))))

(cl-defgeneric semigroup-concat (a b))

(cl-defmethod semigroup-concat ((a cons) b)
  (pcase-exhaustive a
   ((pred alistp) (map-merge 'alist a b))
   ((pred homogenic-list-p a) (append a b))
   (_ (cl-call-next-method a b))))

(cl-defmethod semigroup-concat ((a number) b)
  (error "More than one semigroup be formed with numbers. Please wrap your integers."))

(cl-defmethod semigroup-concat ((a string) b)
  (concat a b))

(cl-defmethod semigroup-concat ((a vector) b)
  (vconcat a b))

(cl-defmethod semigroup-concat ((a hash-table) b)
  (map-merge-hash-table a b))

(defmacro semigroup-concat-for-wrapped-type (type decon op wrapper)
  `(cl-defmethod semigroup-concat ((a (head ,type)) b)
     (funcall (-compose ,wrapper ,op (-partial #'mapcar ,decon) #'list) a b)))

(defmacro semigroup-concat-for-list-of-wrapped-types (args)
    `(plural-splice! semigroup-concat-for-wrapped-type ,args))


(defconst DEFAULT-SEMIGROUP-CONCAT-WRAPPED-TYPES
  (list
   [sum #'get-sum #'-sum #'wrap-sum]
   [max #'get-max #'-max #'wrap-max]
   [min #'get-min #'-min #'wrap-min]
   [product #'get-product #'-applify-multiply #'wrap-product]
   [first #'get-first #'car #'wrap-first]
   [last #'get-last #'cadr #'wrap-last]
   [any #'get-any #'-any-true #'wrap-any]
   [all #'get-all #'-every-true #'wrap-all]
   [const #'get-const (-applify #'semigroup-concat) #'wrap-const]))


(semigroup-concat-for-list-of-wrapped-types DEFAULT-SEMIGROUP-CONCAT-WRAPPED-TYPES)

(cl-defmethod semigroup-concat ((a (head plist)) b)

  (funcall (-compose #'wrap-plist (-applify (-partial #'map-merge 'plist)) (-partial #'mapcar #'get-plist) #'list) a b))

(semigroup-concat (list 'plist (list 1 2 3)) (list 'plist (list 4 5 6)))

(cl-defmethod semigroup-concat ((a (head maybe)) b)
  (pcase-exhaustive (mapcar #'get-maybe (list a b))
    (`((just ,x) (just ,y)) (funcall (-compose #'wrap-maybe-just #'semigroup-concat) x y))
    (`((just ,x) (nothing)) (wrap-maybe-just x))
    (`((nothing) (just ,x)) (wrap-maybe-just x))
    (`((nothing) (nothing)) (create-maybe-nothing))))

;; test copy-hash-table
(defalias 'less-than-or-equal #'<=)
(defun stimes-idempotent (n x)
  (pcase n
   ((pred less-than-or-equal-zero) (error "stimes-idempotent: positive multiplier expected"))
   (_ x)))

(cl-defmethod stimes (n (a cons))
  (when (less-than-or-equal-zero n)
    (error "stimes: positive multiplier expected"))
  (pcase-exhaustive a
   ((pred alistp) (stimes-idempotent n a))
   ((and (pred homogenic-list-p) (guard (plusp n))) (make-list-then-flatten n a))
   ((and (pred homogenic-list-p) (guard (zerop n))) [])
   (_ (cl-call-next-method n a))))

(cl-defmethod stimes (n (a string))
  (pcase-exhaustive n
    ((pred minusp) (error "stimes: positive multiplier expected"))
    ((pred zerop) "")
    ((pred plusp) (funcall (-compose #'-applify-concat #'make-list) n a))))

(cl-defmethod stimes (n (a vector))
  (pcase-exhaustive n
    ((pred minusp) (error "stimes: positive multiplier expected"))
    ((pred zerop) [])
    ((pred plusp) (funcall (-compose #'-applify-vconcat #'make-list) n a))))

(cl-defmethod stimes (n (a hash-table))    
  (stimes-idempotent n a))

(defmacro stimes-for-wrapped-type (type decon op wrapper)
  `(cl-defmethod stimes (n (a (head ,type)))
     (when (less-than-or-equal-zero n)
       (error "stimes: positive multiplier expected"))
     (funcall (-compose ,wrapper (-partial ,op n) ,decon) a)))

(defmacro stimes-for-list-of-wrapped-types (args)
    `(plural-splice! stimes-for-wrapped-type ,args))


(defconst DEFAULT-STIMES-WRAPPED-TYPES-ARGS
  (list
   [sum #'get-sum #'* #'wrap-sum]
   [product #'get-product (-flip #'expt) #'wrap-product]
   [const #'get-const #'stimes #'wrap-const]
   [plist #'get-plist #'stimes-idempotent #'wrap-plist]
   [max #'get-max #'stimes-idempotent #'wrap-max]     
   [min #'get-min #'stimes-idempotent #'wrap-min]
   [first #'get-first #'stimes-idempotent #'wrap-first]
   [last #'get-last #'stimes-idempotent #'wrap-last]
   [any #'get-any #'stimes-idempotent #'wrap-any]
   [all #'get-all #'stimes-idempotent #'wrap-all]))
   

(stimes-for-list-of-wrapped-types DEFAULT-STIMES-WRAPPED-TYPES-ARGS)

(cl-defmethod stimes (n (a (head maybe)))
  (pcase-exhaustive (get-maybe a)
    (`(nothing) (create-maybe-nothing))
    ((and `(just ,x) (guard (minusp n))) (error "stimes: Maybe, negative multiplier"))
    ((and `(just ,x) (guard (zerop n))) (create-maybe-nothing))      
    ((and `(just ,x) (guard (plusp n))) (funcall (-compose #'wrap-maybe #'stimes) n x))))

(cl-defgeneric mempty ())

(cl-defmethod mempty ())



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
