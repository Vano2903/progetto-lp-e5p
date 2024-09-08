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

(defun is-infinity (x)
  (or (eq x +neg-infinity+) (eq x +pos-infinity+)))

(defun is-extended-real (x)
  (or (numberp x) (is-infinity x)))

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

; (defun interval (&optional l h))

; (defun whole ()
;   (interval +neg-infinity+ +pos-infinity+))

; (defun is-interval (i))

; (defun is-singleton (i))

; (defun inf (i))

; (defun sup (i))

; (defun contains (i x))

; (defun overlap (i1 i2))

; if 0 args return 0
; if 2 args return the sum of the args
(defun +e (&optional x y)
  (if (and (null x) (null y))
      0
      (let ((sum (extended-real-sum x y)))
        (if (null sum)
            (error "The sum of ~a and ~a is not defined" x y)
            sum))))

(defun -e (x &optional y)
  (if (null y)
      (sub-reciprocal x)
      (let ((sum (extended-real-sub x y)))
        (if (null sum)
            (error "The difference of ~a and ~a is not defined" x y)
            sum))))

(defun *e (&optional x y)
  (if (and (null x) (null y))
      0
      (let ((sum (extended-real-mul x y)))
        (if (null sum)
            (error "The multiplication of ~a and ~a is not defined" x y)
            sum))))

(defun /e (x &optional y)
  (if (null y)
      (div-reciprocal x)
      (let ((sum (extended-real-div x y)))
        (if (null sum)
            (error "The division of ~a and ~a is not defined" x y)
            sum))))

; (defun i+ (&optional x y))

; (defun i- (&optional x y))

; (defun i* (&optional x y))

; (defun i/ (&optional x y))

; ===== TEST =====

; (assert (is-extended-real +neg-infinity+))
; (assert (is-extended-real +pos-infinity+))
; (assert (is-extended-real 0))
; (assert (is-extended-real 1))
; (assert (is-extended-real -1))
; (assert (not (is-extended-real 'a)))
; (assert (not (is-extended-real '(1 2 3))))
; (assert (not (is-extended-real '(1))))

; (assert (eq +empty-interval+ (empty-interval)))

;!====== sum asserts
; asserts for x=+neg-infinity+
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ -1)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-sum +neg-infinity+ 1)))
(assert (eq nil (extended-real-sum +neg-infinity+ +pos-infinity+)))

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
(assert (eq nil (extended-real-sum +pos-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ -1)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ 1)))
(assert (eq +pos-infinity+ (extended-real-sum +pos-infinity+ +pos-infinity+)))

; ;!====== sub asserts
; asserts for x=+neg-infinity+
(assert (eq nil (extended-real-sub +neg-infinity+ +neg-infinity+)))
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
(assert (eq nil (extended-real-sub +pos-infinity+ +pos-infinity+)))

;!====== mul asserts
; asserts for x=+neg-infinity+
(assert (eq +pos-infinity+ (extended-real-mul +neg-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-mul +neg-infinity+ -1)))
(assert (eq nil (extended-real-mul +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-mul +neg-infinity+ 1)))
(assert (eq +neg-infinity+ (extended-real-mul +neg-infinity+ +pos-infinity+)))

; ; asserts for x=NR
(assert (eq +pos-infinity+ (extended-real-mul -1 +neg-infinity+)))
(assert (< 0 (extended-real-mul -1 -1)))
(assert (eq 0 (extended-real-mul -1 0)))
(assert (> 0 (extended-real-mul -1 1)))
(assert (eq +neg-infinity+ (extended-real-mul -1 +pos-infinity+)))

; ; asserts for x=0
(assert (eq nil (extended-real-mul 0 +neg-infinity+)))
(assert (eq 0 (extended-real-mul 0 -1)))
(assert (eq 0 (extended-real-mul 0 0)))
(assert (eq 0 (extended-real-mul 0 1)))
(assert (eq nil (extended-real-mul 0 +pos-infinity+)))

; ; asserts for x=PR
(assert (eq +neg-infinity+ (extended-real-mul 1 +neg-infinity+)))
(assert (> 0 (extended-real-mul 1 -1)))
(assert (eq 0 (extended-real-mul 1 0)))
(assert (< 0 (extended-real-mul 1 1)))
(assert (eq +pos-infinity+ (extended-real-mul 1 +pos-infinity+)))

; ; asserts for x=+pos-infinity+
(assert (eq +neg-infinity+ (extended-real-mul +pos-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-mul +pos-infinity+ -1)))
(assert (eq nil (extended-real-mul +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-mul +pos-infinity+ 1)))
(assert (eq +pos-infinity+ (extended-real-mul +pos-infinity+ +pos-infinity+)))

; ;!====== div asserts
; ; asserts for x=+neg-infinity+
(assert (eq nil (extended-real-div +neg-infinity+ +neg-infinity+)))
(assert (eq +pos-infinity+ (extended-real-div +neg-infinity+ -1)))
(assert (eq nil (extended-real-div +neg-infinity+ 0)))
(assert (eq +neg-infinity+ (extended-real-div +neg-infinity+ 1)))
(assert (eq nil (extended-real-div +neg-infinity+ +pos-infinity+)))

; ; asserts for x=NR
(assert (eq 0 (extended-real-div -1 +neg-infinity+)))
(assert (< 0 (extended-real-div -1 -1)))
(assert (eq nil (extended-real-div -1 0)))
(assert (> 0 (extended-real-div -1 1)))
(assert (eq 0 (extended-real-div -1 +pos-infinity+)))

; ; asserts for x=0
(assert (eq 0 (extended-real-div 0 +neg-infinity+)))
(assert (eq 0 (extended-real-div 0 -1)))
(assert (eq nil (extended-real-div 0 0)))
(assert (eq 0 (extended-real-div 0 1)))
(assert (eq 0 (extended-real-div 0 +pos-infinity+)))

; ; asserts for x=PR
(assert (eq 0 (extended-real-div 1 +neg-infinity+)))
(assert (> 0 (extended-real-div 1 -1)))
(assert (eq nil (extended-real-div 1 0)))
(assert (< 0 (extended-real-div 1 1)))
(assert (eq 0 (extended-real-div 1 +pos-infinity+)))

; ; asserts for x=+pos-infinity+
(assert (eq nil (extended-real-div +pos-infinity+ +neg-infinity+)))
(assert (eq +neg-infinity+ (extended-real-div +pos-infinity+ -1)))
(assert (eq nil (extended-real-div +pos-infinity+ 0)))
(assert (eq +pos-infinity+ (extended-real-div +pos-infinity+ 1)))
(assert (eq nil (extended-real-div +pos-infinity+ +pos-infinity+)))

; (assert (eq +neg-infinity+ (inf (whole))))
; (assert (eq +pos-infinity+ (sup (whole))))

(print "All tests passed")