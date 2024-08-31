;; -*- Mode: Lisp -*-

;; Gianfarelli	Giorgio	
;; Lauria	Luca	
;; Vanoncini	Davide	903214

;; Interval Arithmetic
;; This file contains the implementation of the interval arithmetic operations

; todo chose representation
(defconstant +neg-infinity+ NIL "Negative infinity")

(defconstant +pos-infinity+ NIL "Positive infinity")

(defconstant +empty-interval+ () "Empty interval")

(defun empty-interval () +empty-interval+)

(defun +e (&optional x y))

(defun -e (&optional x y))

(defun *e (&optional x y))

(defun /e (&optional x y))

(defun interval (&optional l h))

(defun whole ()
  (interval +neg-infinity+ +pos-infinity+))

(defun is-interval (i))

(defun is-singleton (i))

(defun inf (i))

(defun sup (i))

(defun contains (i x))

(defun overlap (i1 i2))

(defun i+ (&optional x y))

(defun i- (&optional x y))

(defun i* (&optional x y))

(defun i/ (&optional x y))
