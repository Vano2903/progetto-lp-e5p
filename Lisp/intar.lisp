;; -*- Mode: Lisp -*-

;; Gianfarelli	Giorgio	894499
;; Lauria	Luca	900326
;; Vanoncini	Davide	903214


;; Interval Arithmetic
;; This file contains the implementation of the interval arithmetic operations

; todo chose representation
(defconstant +neg-infinity+ 'neg-infinity "Negative infinity")

(defconstant +pos-infinity+ 'pos-infinity "Positive infinity")

(defconstant +empty-interval+ () "Empty interval")

; (defun is-zero-in-interval (i)
;   (and (<= (inf i) 0) (>= (sup i) 0)))

(defun is-cons-interval (i)
  (and (is-extended-real (car i))
       (is-extended-real (cdr i))))

(defun is-infinity (x)
  (or (eq x +neg-infinity+) (eq x +pos-infinity+)))

(defun is-extended-real (x)
  (or (numberp x) (is-infinity x)))

; x > y
(defun extended-real-gt (x y)
  (cond
   ((or (not (is-extended-real x))
        (not (is-extended-real y)))
     nil)
   ((or (eq x +neg-infinity+)
        (eq y +pos-infinity+))
     nil)
   ((eq y +neg-infinity+)
     t)
   ((eq x +pos-infinity+)
     (not (eq y +pos-infinity+)))
   (T (> x y))))

; x == y
(defun extended-real-eq (x y)
  (if (and (is-extended-real x) (is-extended-real y))
      (eq x y)))

; ===== operation tables =====

(defun extended-real-sum (x y)
  (when (not (is-extended-real x))
        (error "value ~a is invalid, X must be an extended real number" x))
  (when (not (is-extended-real y))
        (error "value ~a is invalid, Y must be an extended real number" y))
  (cond
   ((and (numberp x) (numberp y)) (+ x y))
   ((or (and (eq x +neg-infinity+) (eq y +pos-infinity+))
        (and (eq x +pos-infinity+) (eq y +neg-infinity+))) nil)
   ((eq x +neg-infinity+) +neg-infinity+)
   ((eq y +neg-infinity+) +neg-infinity+)
   ((eq x +pos-infinity+) +pos-infinity+)
   ((eq y +pos-infinity+) +pos-infinity+)))

(defun extended-real-sub (x y)
  (when (not (is-extended-real x))
        (error "value ~a is invalid, X must be an extended real number" x))
  (when (not (is-extended-real y))
        (error "value ~a is invalid, Y must be an extended real number" y))
  (cond
   ((and (numberp x) (numberp y)) (- x y))
   ((or (and (eq x +neg-infinity+) (eq y +neg-infinity+))
        (and (eq x +pos-infinity+) (eq y +pos-infinity+))) nil)
   ((eq x +neg-infinity+) +neg-infinity+)
   ((eq y +neg-infinity+) +pos-infinity+)
   ((eq x +pos-infinity+) +pos-infinity+)
   ((eq y +pos-infinity+) +neg-infinity+)))

(defun extended-real-mul (x y)
  (when (not (is-extended-real x))
        (error "value ~a is invalid, X must be an extended real number" x))
  (when (not (is-extended-real y))
        (error "value ~a is invalid, Y must be an extended real number" y))
  (cond
   ((and (numberp x) (numberp y)) (* x y))
   ((or (and (eq x 0) (is-infinity y))
        (and (eq y 0) (is-infinity x)))
     nil)
   ((eq x +neg-infinity+)
     (cond ((is-infinity y)
             (if (eq y +pos-infinity+)
                 +neg-infinity+
                 +pos-infinity+))
           (t
             (if (> y 0)
                 +neg-infinity+
                 +pos-infinity+))))
   ((eq x +pos-infinity+)
     (cond
      ((is-infinity y)
        (if (eq y +pos-infinity+)
            +pos-infinity+
            +neg-infinity+))
      (t
        (if (> y 0)
            +pos-infinity+
            +neg-infinity+))))
   ((is-infinity y)
     (extended-real-mul y x))))

(defun extended-real-div (x y)
  (when (not (is-extended-real x))
        (error "value ~a is invalid, X must be an extended real number" x))
  (when (not (is-extended-real y))
        (error "value ~a is invalid, Y must be an extended real number" y))
  (cond
   ((eq y 0) nil)
   ((and (numberp x) (numberp y)) (/ x y))
   ((or (and (eq x +neg-infinity+) (eq y +neg-infinity+))
        (and (eq x +pos-infinity+) (eq y +pos-infinity+))) nil)
   ((or (and (eq x +pos-infinity+) (eq y +neg-infinity+))
        (and (eq x +neg-infinity+) (eq y +pos-infinity+))) nil)
   ((eq x +neg-infinity+)
     (if (> y 0)
         +neg-infinity+
         +pos-infinity+))
   ((eq x +pos-infinity+)
     (if (> y 0)
         +pos-infinity+
         +neg-infinity+))
   ((or (eq y +neg-infinity+)
        (eq y +pos-infinity+)) 0)))

; find the reciprocal of x by subtracting x from 1
(defun sub-reciprocal (x)
  (extended-real-sub 0 x))

; find the reciprocal of x by dividing x from 1
(defun div-reciprocal (x)
  (extended-real-div 1 x))

;!===== API FUNCTIONS =====!

(defun empty-interval () +empty-interval+)

(defun interval (&optional l h)
  (cond
   ((and (null l) (null h))
     (empty-interval))
   ((and (null h) (is-extended-real l))
     (cons l l))
   ((and (is-extended-real l)
         (is-extended-real h))
     (if (or (extended-real-gt h l)
             (extended-real-eq l h))
         (cons l h)
         (empty-interval)))))

(defun whole ()
  (interval +neg-infinity+ +pos-infinity+))

; checks if i is an interval,
; an interval is either be a cons cell with two extended reals
; or a list of intervals
(defun is-interval (i)
  (cond
   ((eq i +empty-interval+)
     T)
   ((not (listp i))
     nil)
   ((listp (cdr i))
     (and (is-interval (car i))
          (is-interval (cdr i))))
   (T
     (is-cons-interval i))))

(defun is-empty (i)
  (if (is-interval i)
      (eq i +empty-interval+)
      (error "The provided value ~A is not an interval" i)))

(defun is-singleton (i)
  (if (is-interval i)
      (and (not (is-empty i)) (eq (inf i) (sup i)))
      (error "The provided value ~A is not an interval" i)))

(defun inf (i)
  (cond
   ((is-empty i)
     (error "The interval is empty"))
   ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
   ((is-cons-interval i)
     (car i))
   (T
     (caar i))))

(defun sup (i)
  (cond
   ((is-empty i)
     (error "The interval is empty"))
   ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
   ((is-cons-interval i)
     (cdr i))
   (T
     (sup (cdr i)))))

; (defun contains (i x))

; (defun overlap (i1 i2))

; if 0 args return 0
; if 2 args return the sum of the args
(defun +e (&optional x y)
  (cond
   ((and (null x) (null y)) 0)
   ((null y) x)
   (T (let ((result (extended-real-sum x y)))
        (if (null result)
            (error "The sum of ~a and ~a is not defined" x y)
            result)))))

(defun *e (&optional x y)
  (cond
   ((and (null x) (null y)) 1)
   ((null y) x)
   (T (let ((result (extended-real-mul x y)))
        (if (null result)
            (error "The multiplication of ~a and ~a is not defined" x y)
            result)))))

(defun -e (x &optional y)
  (if (null y)
      (sub-reciprocal x)
      (let ((result (extended-real-sub x y)))
        (if (null result)
            (error "The difference of ~a and ~a is not defined" x y)
            result))))

(defun /e (x &optional y)
  (if (null y)
      (div-reciprocal x)
      (let ((result (extended-real-div x y)))
        (if (null result)
            (error "The division of ~a and ~a is not defined" x y)
            result))))


(defun i+ (&optional x y)
  (if (not (or (is-interval x) (is-extended-real x)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" x))
  (if (not (or (is-interval y) (is-extended-real y)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" y))

  (let ((xi (if (is-interval x)
                x
                (interval x)))
        (yi (if (is-interval y)
                y
                (interval y))))
    (cond
     ((and (null xi) (null yi))
       (interval 0))
     ((or (null xi) (null yi))
       (if (null xi)
           yi
           xi))
     (T
       (interval
         (+e (inf xi) (inf yi))
         (+e (sup xi) (sup yi)))))))

(defun i- (x &optional y)
  (if (not (or (is-interval x) (is-extended-real x)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" x))
  (if (not (or (is-interval y) (is-extended-real y)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" y))

  (let ((xi (if (is-interval x)
                x
                (interval x)))
        (yi (if (is-interval y)
                y
                (interval y))))
    (cond
     ((null y)
       (interval
         (sub-reciprocal (inf xi))
         (sub-reciprocal (sup xi))))
     (T
       (interval
         (-e (inf xi) (sup yi))
         (-e (sup xi) (inf yi)))))))

(defun i* (&optional x y)
  (if (not (or (is-interval x) (is-extended-real x)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" x))
  (if (not (or (is-interval y) (is-extended-real y)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" y))

  (let ((xi (if (is-interval x)
                x
                (interval x)))
        (yi (if (is-interval y)
                y
                (interval y))))
    (cond
     ((and (null xi) (null yi))
       (interval 1))
     ((or (null xi) (null yi))
       (if (null xi)
           yi
           xi))
     (T
       (interval
         (min (*e (inf xi) (inf yi))
           (*e (inf xi) (sup yi))
           (*e (sup xi) (inf yi))
           (*e (sup xi) (sup yi)))
         (max (*e (inf xi) (inf yi))
           (*e (inf xi) (sup yi))
           (*e (sup xi) (inf yi))
           (*e (sup xi) (sup yi))))))))

; (defun i/ (&optional x y))

;! ===== TEST =====

(assert (is-extended-real +neg-infinity+))
(assert (is-extended-real +pos-infinity+))
(assert (is-extended-real 0))
(assert (is-extended-real 0.1))
(assert (is-extended-real 1))
(assert (is-extended-real -1))
(assert (not (is-extended-real 'a)))
(assert (not (is-extended-real '(1 2 3))))
(assert (not (is-extended-real '(1))))

; (assert (eq +empty-interval+ (empty-interval)))

;!=== asserts comparisons on extended reals
; x > y
;!====== asserts for greater than
(assert (null (extended-real-gt +neg-infinity+ +neg-infinity+)))
(assert (null (extended-real-gt +neg-infinity+ -1)))
(assert (null (extended-real-gt +neg-infinity+ 0)))
(assert (null (extended-real-gt +neg-infinity+ 1)))
(assert (null (extended-real-gt +neg-infinity+ +pos-infinity+)))

(assert (extended-real-gt -1 +neg-infinity+))
(assert (null (extended-real-gt -1 -1)))
(assert (null (extended-real-gt -1 0)))
(assert (null (extended-real-gt -1 1)))
(assert (null (extended-real-gt -1 +pos-infinity+)))

(assert (extended-real-gt 0 +neg-infinity+))
(assert (extended-real-gt 0 -1))
(assert (null (extended-real-gt 0 0)))
(assert (null (extended-real-gt 0 1)))
(assert (null (extended-real-gt 0 +pos-infinity+)))

(assert (extended-real-gt 1 +neg-infinity+))
(assert (extended-real-gt 1 -1))
(assert (extended-real-gt 1 0))
(assert (null (extended-real-gt 1 1)))
(assert (null (extended-real-gt 1 +pos-infinity+)))

(assert (extended-real-gt +pos-infinity+ +neg-infinity+))
(assert (extended-real-gt +pos-infinity+ -1))
(assert (extended-real-gt +pos-infinity+ 0))
(assert (extended-real-gt +pos-infinity+ 1))
(assert (null (extended-real-gt +pos-infinity+ +pos-infinity+)))

;!====== asserts for equal
(assert (extended-real-eq +neg-infinity+ +neg-infinity+))
(assert (null (extended-real-eq +neg-infinity+ -1)))
(assert (null (extended-real-eq +neg-infinity+ 0)))
(assert (null (extended-real-eq +neg-infinity+ 1)))
(assert (null (extended-real-eq +neg-infinity+ +pos-infinity+)))

(assert (null (extended-real-eq -1 +neg-infinity+)))
(assert (extended-real-eq -1 -1))
(assert (null (extended-real-eq -1 0)))
(assert (null (extended-real-eq -1 1)))
(assert (null (extended-real-eq -1 +pos-infinity+)))

(assert (null (extended-real-eq 0 +neg-infinity+)))
(assert (null (extended-real-eq 0 -1)))
(assert (extended-real-eq 0 0))
(assert (null (extended-real-eq 0 1)))
(assert (null (extended-real-eq 0 +pos-infinity+)))

(assert (null (extended-real-eq 1 +neg-infinity+)))
(assert (null (extended-real-eq 1 -1)))
(assert (null (extended-real-eq 1 0)))
(assert (extended-real-eq 1 1))
(assert (null (extended-real-eq 1 +pos-infinity+)))

(assert (null (extended-real-eq +pos-infinity+ +neg-infinity+)))
(assert (null (extended-real-eq +pos-infinity+ -1)))
(assert (null (extended-real-eq +pos-infinity+ 0)))
(assert (null (extended-real-eq +pos-infinity+ 1)))
(assert (extended-real-eq +pos-infinity+ +pos-infinity+))

;!=== asserts on operations with extended reals
;!====== sum asserts
; asserts for x=+neg-infinity+
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ -1)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ 1)))
(assert (null (extended-real-sum +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (eq +neg-infinity+ (extended-real-sum -1 +neg-infinity+)))
(assert (> 0 (extended-real-sum -1 -1)))
(assert (> 0 (extended-real-sum -1 0)))
(assert (numberp (extended-real-sum -1 1)))
(assert (eq +pos-infinity+ (extended-real-sum -1 +pos-infinity+)))

; asserts for x=0
(assert (eq +neg-infinity+ (extended-real-sum 0 +neg-infinity+)))
(assert (> 0 (extended-real-sum 0 -1)))
(assert (eq 0 (extended-real-sum 0 0)))
(assert (< 0 (extended-real-sum 0 1)))
(assert (eq +pos-infinity+ (extended-real-sum 0 +pos-infinity+)))

; asserts for x=PR
(assert (eq +neg-infinity+ (extended-real-sum 1 +neg-infinity+)))
(assert (numberp (extended-real-sum 1 -1)))
(assert (< 0 (extended-real-sum 1 0)))
(assert (< 0 (extended-real-sum 1 1)))
(assert (eq +pos-infinity+ (extended-real-sum 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (null (extended-real-sum +pos-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ -1)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ 1)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ +pos-infinity+)))

; ;!====== sub asserts
; asserts for x=+neg-infinity+
(assert (null (extended-real-sub +neg-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-sub +neg-infinity+ -1)))
(assert (eq +neg-infinity+ (extended-real-sub +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-sub +neg-infinity+ 1)))
(assert (eq +neg-infinity+ (extended-real-sub +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (eq +pos-infinity+ (extended-real-sub -1 +neg-infinity+)))
(assert (numberp (extended-real-sub -1 -1)))
(assert (> 0 (extended-real-sub -1 0)))
(assert (> 0 (extended-real-sub -1 1)))
(assert (eq +neg-infinity+ (extended-real-sub -1 +pos-infinity+)))

; asserts for x=0
(assert (eq +pos-infinity+ (extended-real-sub 0 +neg-infinity+)))
(assert (< 0 (extended-real-sub 0 -1)))
(assert (eq 0 (extended-real-sub 0 0)))
(assert (> 0 (extended-real-sub 0 1)))
(assert (eq +neg-infinity+ (extended-real-sub 0 +pos-infinity+)))

; asserts for x=PR
(assert (eq +pos-infinity+ (extended-real-sub 1 +neg-infinity+)))
(assert (< 0 (extended-real-sub 1 -1)))
(assert (< 0 (extended-real-sub 1 0)))
(assert (numberp (extended-real-sub 1 1)))
(assert (eq +neg-infinity+ (extended-real-sub 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (eq +pos-infinity+ (extended-real-sub +pos-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-sub +pos-infinity+ -1)))
(assert (eq +pos-infinity+ (extended-real-sub +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-sub +pos-infinity+ 1)))
(assert (null (extended-real-sub +pos-infinity+ +pos-infinity+)))

;!====== mul asserts
; asserts for x=+neg-infinity+
(assert (eq +pos-infinity+ (extended-real-mul +neg-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-mul +neg-infinity+ -1)))
(assert (null (extended-real-mul +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-mul +neg-infinity+ 1)))
(assert (eq +neg-infinity+ (extended-real-mul +neg-infinity+ +pos-infinity+)))

; ; asserts for x=NR
(assert (eq +pos-infinity+ (extended-real-mul -1 +neg-infinity+)))
(assert (< 0 (extended-real-mul -1 -1)))
(assert (eq 0 (extended-real-mul -1 0)))
(assert (> 0 (extended-real-mul -1 1)))
(assert (eq +neg-infinity+ (extended-real-mul -1 +pos-infinity+)))

; ; asserts for x=0
(assert (null (extended-real-mul 0 +neg-infinity+)))
(assert (eq 0 (extended-real-mul 0 -1)))
(assert (eq 0 (extended-real-mul 0 0)))
(assert (eq 0 (extended-real-mul 0 1)))
(assert (null (extended-real-mul 0 +pos-infinity+)))

; ; asserts for x=PR
(assert (eq +neg-infinity+ (extended-real-mul 1 +neg-infinity+)))
(assert (> 0 (extended-real-mul 1 -1)))
(assert (eq 0 (extended-real-mul 1 0)))
(assert (< 0 (extended-real-mul 1 1)))
(assert (eq +pos-infinity+ (extended-real-mul 1 +pos-infinity+)))

; ; asserts for x=+pos-infinity+
(assert (eq +neg-infinity+ (extended-real-mul +pos-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-mul +pos-infinity+ -1)))
(assert (null (extended-real-mul +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-mul +pos-infinity+ 1)))
(assert (eq +pos-infinity+ (extended-real-mul +pos-infinity+ +pos-infinity+)))

; ;!====== div asserts
; ; asserts for x=+neg-infinity+
(assert (null (extended-real-div +neg-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-div +neg-infinity+ -1)))
(assert (null (extended-real-div +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-div +neg-infinity+ 1)))
(assert (null (extended-real-div +neg-infinity+ +pos-infinity+)))

; ; asserts for x=NR
(assert (eq 0 (extended-real-div -1 +neg-infinity+)))
(assert (< 0 (extended-real-div -1 -1)))
(assert (null (extended-real-div -1 0)))
(assert (> 0 (extended-real-div -1 1)))
(assert (eq 0 (extended-real-div -1 +pos-infinity+)))

; ; asserts for x=0
(assert (eq 0 (extended-real-div 0 +neg-infinity+)))
(assert (eq 0 (extended-real-div 0 -1)))
(assert (null (extended-real-div 0 0)))
(assert (eq 0 (extended-real-div 0 1)))
(assert (eq 0 (extended-real-div 0 +pos-infinity+)))

; ; asserts for x=PR
(assert (eq 0 (extended-real-div 1 +neg-infinity+)))
(assert (> 0 (extended-real-div 1 -1)))
(assert (null (extended-real-div 1 0)))
(assert (< 0 (extended-real-div 1 1)))
(assert (eq 0 (extended-real-div 1 +pos-infinity+)))

; ; asserts for x=+pos-infinity+
(assert (null (extended-real-div +pos-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-div +pos-infinity+ -1)))
(assert (null (extended-real-div +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-div +pos-infinity+ 1)))
(assert (null (extended-real-div +pos-infinity+ +pos-infinity+)))

;!==== asserts with API functions

; (assert (eq +neg-infinity+ (inf (whole))))
; (assert (eq +pos-infinity+ (sup (whole))))

; assert with single value
(assert (eq 0 (+e)))
(assert (eq 1 (*e)))
; (assert (eq 0 (+e)))
; (assert (eq 0 (+e)))

(print "All tests passed")