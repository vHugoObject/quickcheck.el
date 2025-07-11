;;; quickcheck.el --- Quickcheck clone       -**- lexical-binding: t -**-

  (require 'eieio)
  (require 'cl-lib)
  (require 'calc-comb)
  (require 'range)

(defconst DEFAULTRANDOMNUMBERRANGE
  (list 1 255)    
  "default random number range")
(defconst THOUSAND
  (float 1000)
  "Float creator")
(defconst SIZE
  30
  "Size for generators")

(cl-defun range-member-exclusive-p ((range-min range-max) number)
  (and (greater-than-or-equal number range-min) (less-than number range-max)))

(defalias 'between-one-and-255 (apply-partially #'range-member-exclusive-p (list 1 255)))
(defalias 'between-zero-and-one (apply-partially #'range-member-exclusive-p (list 0 1)))

(defalias '2+ (apply-partially #'+ 2))

(defalias 'not-equal #'/=)
(defalias 'less-than #'<)
(defalias 'less-than-or-equal #'<=)
(defalias 'greater-than #'>)
(defalias 'greater-than-or-equal #'>=)
(defalias 'greater-than-or-equal-one (-rpartial #'>= 1))
(defalias 'equal-zero (apply-partially #'eql 0))
(defalias 'equal-one (apply-partially #'eql 0))

(defalias 'seq-count-integers (apply-partially #'seq-count #'integerp))
(defalias 'seq-count-floats (apply-partially #'seq-count #'floatp))
(defalias 'seq-count-strings (apply-partially #'seq-count #'stringp))  
(defalias 'seq-count-cons (apply-partially #'seq-count #'consp))

(defalias 'seq-count-between-zero-and-one (apply-partially #'seq-count #'between-zero-and-one))
(defalias 'seq-count-greater-than-or-equal-one (apply-partially #'seq-count #'greater-than-or-equal-one))

(defalias 'seq-map-add-one (apply-partially #'seq-map #'1+))
(defalias 'seq-map-length (apply-partially #'seq-map #'seq-length))

(defalias 'calcFunc-random-255 (apply-partially #'calcFunc-random 255))

(defalias '-applify-rpartial (-applify #'-rpartial))
(defalias '-applify-partial (-applify #'-partial))

(defalias '-applify-subtract (-applify #'-))

(defalias '-iterate-plus-one  (apply-partially #'-iterate #'1+))
(defalias '-first-and-last-item  (-juxt #'-first-item #'-last-item))
(defalias '-applify-zip  (-applify #'-zip))
(defalias '-applify-cons  (-applify #'cons))
(defalias '-applify-mapcar  (-applify #'mapcar))

(defalias 'divide-by-THOUSAND   (-rpartial #'/ THOUSAND))
(defalias 'divide-array-values-by-max-array-value (-compose #'-applify-mapcar (-juxt (-compose #'-applify-rpartial (apply-partially #'list #'/) #'float #'1+ #'-max) #'identity)))
(defalias '-applify-divide (-applify #'/))

(defalias 'print (apply-partially #'message "%s"))

(defun times (function n)
  (cdr (-iterate function nil (1+ n))))

(defun times-no-args (function n)
  (cdr (-iterate (lambda (_) (funcall function)) nil (1+ n))))

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

(defalias 'cons-vec (apply-partially #'cons 'vec))

(defun convert-calc-value-into-lisp (calc-value)
  (read (math-format-value calc-value)))

(defun shuffle (list)
  (let* ((list-length (seq-length list))
	 (vec (cons 'vec list))
	 (shuffled-vec (math-shuffle-list list-length list-length vec)))
  (cdr shuffled-vec)))

;; test-runner
;; needs a test
(defmacro ert-deftest-n-times (name runs body)
  (declare (indent 2))
  (let ((fun-sym (gensym "test")))
    `(ert-deftest ,name ()
       (let ((,fun-sym (lambda (x) (progn
				     ,body 1))))  			 
	(times ,fun-sym ,runs)))))

(defun random-float-between-0-and-1 ()    
  (funcall (-compose #'convert-calc-value-into-lisp #'math-random-float)))

(cl-defun random-integer-in-range ((min max))
  (if (eql min max)
      min
    (funcall (-compose (apply-partially #'scale-float-to-range (list min max))  #'random-float-between-0-and-1))))

(defalias 'random-integer-in-range-255 (apply-partially #'random-integer-in-range DEFAULTRANDOMNUMBERRANGE))

(defun random-integer-list (length)    
  (funcall (-compose #'shuffle #'-iterate-plus-one) (math-random-three-digit-number) length))  
(defalias 'random-integer-list-in-range-255 (-compose #'random-integer-list #'random-integer-in-range-255))

(defun n-random-values-from-array (count array)
  (funcall (-compose (apply-partially #'take count) #'shuffle) array))

(defalias 'random-array-value (-compose #'-first-item #'shuffle))

(defalias 'two-random-array-value (apply-partially #'n-random-values-from-array 2))
(defalias 'random-con-from-array (-compose #'-applify-cons #'two-random-array-value))

(defun random-integer-range (length)    
  (funcall (-juxt #'identity (apply-partially #'+ length))
	   (math-random-three-digit-number)))

(defalias 'divide-by-random-value (funcall (-compose #'-applify-rpartial (apply-partially #'list #'/) (-compose #'float #'random-integer-in-range-255))))

(defalias 'divide-array-values-by-random-value (apply-partially #'mapcar #'divide-by-random-value))

(cl-defun generate-test-data (&optional &key item-transformer &key list-transformer
				     &key min-length &key max-length)
  (let* ((min-items (or min-length 1))
	 (max-items (or max-length 255))
	 (item-func (or item-transformer #'identity))
	 (list-func (or list-transformer #'shuffle))
	 (range-length (random-integer-in-range (list min-items max-items)))
	 (list-items (random-integer-list range-length)))
    (funcall (-on list-func (apply-partially #'mapcar item-func)) list-items)))

(defalias 'generate-test-list-of-floats-between-zero-and-one (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-max-array-value #'shuffle)))
(defalias 'generate-test-list-of-floats (apply-partially #'generate-test-data :list-transformer (-compose #'divide-array-values-by-random-value #'shuffle)))
(defalias 'generate-test-list-of-strings (apply-partially #'generate-test-data :item-transformer #'char-to-string))

(defalias 'generate-test-string (apply-partially #'generate-test-data :item-transformer #'identity :list-transformer (-compose #'seq--into-string #'shuffle)))


(defalias 'generate-test-vector-of-integers (apply-partially #'generate-test-data :list-transformer (-compose #'seq--into-vector #'shuffle)))

(defalias 'generate-test-alist-of-integers (apply-partially #'generate-test-data :list-transformer (-compose #'-applify-zip (-juxt #'reverse #'shuffle))))


(defalias 'generate-test-con-of-integers (apply-partially #'generate-test-data :min-length 2 :list-transformer #'random-con-from-array))  
(defalias 'generate-test-con-of-floats (apply-partially #'generate-test-data :min-length 2 :list-transformer (-compose #'random-con-from-array #'divide-array-values-by-max-array-value)))
(defalias 'generate-test-con-of-strings (apply-partially #'generate-test-data :min-length 2 :item-transformer #'char-to-string :list-transformer #'random-con-from-array))

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
(defclass qc-gen ()
  ((seed
    :initform '()
    :type list
    :reader read-seed
    :writer next-int
    :printer show-seed))
  "quickcheck generator")

;; declare pure?
(cl-defmethod read-seed ((qc qc-gen))
  (-last-item (oref qc seed)))

;; declare pure?
;; showsPrec n (QCGen g) s = showsPrec n g s
(cl-defmethod show-seed ((qc qc-gen))
  (princ (read-seed qc)))

(cl-defmethod next-int ((qc qc-gen))
  ;; Next integer between 0 and 999
  (let** ((previous (oref qc seed))
	(next (std-gen))
	(new-qc (oset qc seed (append previous (list next)))))
    (list next qc)))

(cl-defmethod next-double ((qc qc-gen))
  ;; Next double between 0 and 1
  (-let (((int new-qc) (next-int qc)))
    (list (/ int THOUSAND) qc)))

(cl-defmethod next-integer ((qc qc-gen) &optional &key min &key max)    
  (when (and min max (greater-than-or-equal min max))
    (error "min must be less than max"))
  (let** ((minimum (or min most-negative-fixnum))
	 (maximum (or max most-positive-fixnum))
	 (double-and-qc-gen (next-double qc))
	 (next (scale-float-to-range (list minimum maximum) (-first-item double-and-qc-gen))))  	 
    (list (truncate next) qc)))

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
(defclass gen ()
  ((generator
    :initarg :generator
    :type symbol
    :accessor un-gen))
  "generator creator")



(cl-defgeneric gen-fmap (gen)
    ;; Uses a qc-gen seed and a SIZE
    ;; Returns a generator
  )

(cl-defgeneric gen-applicative (gen)
    ;; Uses a qc-gen seed and a SIZE
    ;; Returns a generator
  )

(cl-defgeneric gen-monad (un-gen)
    ;; Uses a qc-gen seed and a SIZE
    ;; Returns a generator
  )

(cl-defgeneric gen-sequencer (gen)
    ;; Uses a qc-gen seed and a SIZE
    ;; Returns a generator
  )

(cl-defgeneric gen-monad-fix (gen)
    ;; Uses a qc-gen seed and a SIZE
    ;; Returns a generator
  )

(provide 'quickcheck)
