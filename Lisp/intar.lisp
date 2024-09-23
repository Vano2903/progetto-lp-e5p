;; -*- Mode: Lisp -*-

;; Gianfarelli	Giorgio	894499
;; Lauria	Luca	900326
;; Vanoncini	Davide	903214


;; Interval Arithmetic
;; This file contains the implementation of the interval arithmetic operations

(defconstant +neg-infinity+ 'neg-infinity "Negative infinity")

(defconstant +pos-infinity+ 'pos-infinity "Positive infinity")

(defconstant +empty-interval+ () "Empty interval")

; (defun is-zero-in-interval (i)
;   (and (<= (inf i) 0) (>= (sup i) 0)))
;!===== validation functions =====!

(defun validate-real-extended-number (x name)
  (if (not (is-extended-real x))
      (error "value ~a is invalid, 
            ~a must be an extended real number" x name)))

(defun validate-parse-interval-operation-parameter (x)
  (if (not (or (is-interval x)
               (is-extended-real x)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" x))
  (if (is-extended-real x)
      (interval x)
      x))
-
; true when the list is empty or every item
; is an extended real
(defun is-valid-exclusion-list (i)
  (cond
   ((null i) T)
   ((not (consp i)) nil)
   (T
     (every #'is-extended-real i))))

; true when the list is empty or every item
; is a cons interval
(defun is-valid-interval-list (i)
  (cond
   ((null i) T)
   ((not (consp i)) nil)
   (T
     (every
         (lambda (x)
           (or (is-cons-interval x)
               (eq x +empty-interval+)))
         i))))

;!===== extended reals =====!
; extended reals definitions
(defun is-infinity (x)
  (or (eq x +neg-infinity+) (eq x +pos-infinity+)))

(defun is-extended-real (x)
  (or (numberp x) (is-infinity x)))

; extended real comparisons
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

(defun extended-real-min (x &rest xs)
  (if (null xs)
      x
      (let ((rest-min (apply #'extended-real-min xs)))
        (if (extended-real-gt x rest-min)
            rest-min
            x))))

(defun extended-real-max (x &rest xs)
  (if (null xs)
      x
      (let ((rest-max (apply #'extended-real-max xs)))
        (if (extended-real-gt x rest-max)
            x
            rest-max))))

; operation tables 
(defun extended-real-sum (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")

  (cond
   ((and (numberp x) (numberp y)) (+ x y))
   ((or (and (eq x +neg-infinity+)
             (eq y +pos-infinity+))
        (and (eq x +pos-infinity+)
             (eq y +neg-infinity+))) nil)
   ((eq x +neg-infinity+) +neg-infinity+)
   ((eq y +neg-infinity+) +neg-infinity+)
   ((eq x +pos-infinity+) +pos-infinity+)
   ((eq y +pos-infinity+) +pos-infinity+)))

(defun extended-real-sub (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
  (cond
   ((and (numberp x) (numberp y)) (- x y))
   ((or (and (eq x +neg-infinity+) (eq y +neg-infinity+))
        (and (eq x +pos-infinity+) (eq y +pos-infinity+))) nil)
   ((eq x +neg-infinity+) +neg-infinity+)
   ((eq y +neg-infinity+) +pos-infinity+)
   ((eq x +pos-infinity+) +pos-infinity+)
   ((eq y +pos-infinity+) +neg-infinity+)))

(defun extended-real-mul (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
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
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
  (cond
   ((eq y 0) nil)
   ((and (numberp x) (numberp y)) (/ x y))
   ((or (and (eq x +neg-infinity+)
             (eq y +neg-infinity+))
        (and (eq x +pos-infinity+)
             (eq y +pos-infinity+))) nil)
   ;  it could be incorporated in the "or"
   ;  but it's more readable this way
   ((or (and (eq x +pos-infinity+)
             (eq y +neg-infinity+))
        (and (eq x +neg-infinity+)
             (eq y +pos-infinity+))) nil)
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

; find the reciprocal of x by subtracting x from o
(defun sub-reciprocal (x)
  (let ((result (extended-real-sub 0 x)))
    (if (or (null result)
            (is-infinity x))
        (error "The complement of ~a is not defined" x)
        result)))

; find the reciprocal of x by dividing 1 by x
(defun div-reciprocal (x)
  (let ((result (extended-real-div 1 x)))
    (if (or (null result)
            (is-infinity x))
        (error "The reciprocal of ~a is not defined" x)
        result)))

;!===== interval functions =====!
(defun is-cons-interval (i)
  (if (not (consp i))
      nil
      (let ((l (car i))
            (h (cdr i)))
        (and (is-extended-real l)
             (is-extended-real h)
             (or (extended-real-gt h l)
                 (extended-real-eq l h))))))

; the function returns nil if the interval provided
; is not a single (valid) interval or if it's the empty interval
; the classification is based on the following table
; p0 = a,b | 0 <= a <= b V b != 0
; p1 = a,b | 0 < a <= b
; z = 0,0
; n0 = a,b | a <= b <= 0 V a != 0
; n1 = a,b | a <= b < 0
; M = a,b | a < 0 < b       
(defun classify-interval (i)
  (if (is-single-interval i)
      (let ((l (inf i))
            (h (sup i)))
        (cond
         ((is-singleton i)
           (cond ((extended-real-eq l 0) 'z)
                 ((extended-real-gt l 0) 'p1)
                 ((extended-real-gt 0 l) 'n1)))

         ;  l == 0 and h > 0 -> p0
         ;  if h == 0 is a singleton or
         ;  if h < 0 is an invalid interval
         ((extended-real-eq l 0) 'p0)
         ;  l>0 and h > l -> p0
         ((extended-real-gt l 0) 'p1)
         ;  l<0 and h == 0 -> n0
         ;  l<0 and h > 0 -> M
         ;  l<0 and h < 0 -> n1
         ((extended-real-gt 0 l)
           (cond
            ((extended-real-eq h 0) 'n0)
            ((extended-real-gt h 0) 'm)
            ((extended-real-gt 0 h) 'n1)))))))

; given a list of intervals
; return the cdr of the last interval
(defun sup-list (i)
  (cond
   ((null i)
     nil)
   ((null (cdr i))
     (cdr (car i)))
   (T
     (sup-list (cdr i)))))

(defun cons-interval (l &optional h)
  (cond
   ((null h)
     (if (is-extended-real l)
         (cons l l)
         (error "The provided value ~A must be an extended real" l)))
   ((and (is-extended-real l)
         (is-extended-real h))
     (if (or (extended-real-gt h l)
             (extended-real-eq l h))
         (cons l h)
         (empty-interval)))
   (T (error "The provided values ~A and ~A are not valid extended reals"
        l h))))

(defun extended-interval (exclusions &rest intervals)
  (cond
   ((and (null exclusions)
         (null intervals))
     (empty-interval))
   ;  TODO: should add a check if the intervals with the exclusions
   ;  make sense
   ;  like [1,1]\{1} doesn't make sense
   ((or (not (is-valid-interval-list intervals))
        (not (is-valid-exclusion-list exclusions)))
     (error "The provided values ~A and ~A are not valid intervals"
       intervals exclusions))
   ((and
     (eq (length intervals) 1)
     (null (car intervals)))
     (empty-interval))
   (T (cons intervals exclusions))))

; get-interval-list and get-exclusion-list
; are not necessary but they make the code more readable
(defun get-interval-list (i)
  (if (is-interval i)
      (car i)
      (error "The provided value ~A is not an interval" i)))

(defun get-exclusion-list (i)
  (if (is-interval i)
      (cdr i)
      (error "The provided value ~A is not an interval" i)))
;!===== API FUNCTIONS =====!

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

(defun empty-interval () +empty-interval+)

(defun interval (&optional l h)
  (cond
   ((and (null l) (null h))
     (empty-interval))
   (T (extended-interval nil (cons-interval l h)))))
; ((null h)
;  (extended-interval nil (cons-interval l)))
; ;  (cond
; ; ((is-infinity l)
; ;   (error "Cannot create a singleton interval with infinity"))
; ;  (if (is-extended-real l)
; ;  (cons (list (cons l l)) (list))
; ;  (extended-interval nil (cons l l))
; ;  (error "The provided value ~A must be an extended real" l)))
; ;  ((and (is-infinity l)
; ;        (is-infinity h)
; ;        (extended-real-eq l h))
; ;    (error "Cannot create an interval with both infinities"))
; ((and (is-extended-real l)
;       (is-extended-real h))
;  (if (or (extended-real-gt h l)
;          (extended-real-eq l h))
;      ;  (cons l h)
;      ;  (cons (list (cons l h)) (list))
;      (extended-interval nil (cons l h))
;      (empty-interval)))
; (T
;  (error T "The provided values ~A 
;      and ~A must be extended reals" l h))))

(defun whole ()
  (interval +neg-infinity+ +pos-infinity+))

; checks if i is an interval,
; an interval is either be a cons cell with two extended reals
; or a list of intervals
(defun is-interval (i)
  (cond
   ((eq i +empty-interval+)
     T)
   ((not (consp i))
     nil)
   (T
     (and (is-valid-interval-list
            (car i))
          (is-valid-exclusion-list
            (cdr i))))))

(defun is-single-interval (i)
  (and (is-interval i)
       (eq (length (get-interval-list i)) 1)
       (is-cons-interval
         (car (get-interval-list i)))))

(defun is-empty (i)
  (if (is-interval i)
      (or (eq i +empty-interval+))
      (error "The provided value ~A is not an interval" i)))

(defun is-singleton (i)
  (cond
   ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
   ((is-empty i)
     nil)
   (t
     (and (is-single-interval i)
          (extended-real-eq (inf i) (sup i))))))

(defun inf (i)
  (cond
   ((is-empty i)
     (error "The interval is empty"))
   ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
   (T
     (caar (get-interval-list i)))))

(defun sup (i)
  (cond
   ((is-empty i)
     (error "The interval is empty"))
   ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
   (T
     (cdar (last (car i))))))
;  ((is-single-interval i)
;    (cdaar i))
;  (T
;    (sup-list (car i)))))

; (defun contains (i x))

; (defun overlap (i1 i2))

(defun i+ (&optional x y)
  (let ((xi (validate-parse-interval-operation-parameter x))
        (yi (validate-parse-interval-operation-parameter y)))
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
  (let ((xi (validate-parse-interval-operation-parameter x))
        (yi (validate-parse-interval-operation-parameter y)))
    (cond
     ((null y)
       (interval
         (sub-reciprocal (sup xi))
         (sub-reciprocal (inf xi))))
     (T
       (interval
         (-e (inf xi) (sup yi))
         (-e (sup xi) (inf yi)))))))

(defun i* (&optional x y)
  (let ((xi (validate-parse-interval-operation-parameter x))
        (yi (validate-parse-interval-operation-parameter y)))
    (cond
     ((and (null xi) (null yi))
       (interval 1))
     ((or (null xi) (null yi))
       (if (null xi)
           yi
           xi))
     (T
       (interval
         (extended-real-min
             (*e (inf xi) (inf yi))
           (*e (inf xi) (sup yi))
           (*e (sup xi) (inf yi))
           (*e (sup xi) (sup yi)))
         (extended-real-max
             (*e (inf xi) (inf yi))
           (*e (inf xi) (sup yi))
           (*e (sup xi) (inf yi))
           (*e (sup xi) (sup yi))))))))

(defun i/ (x &optional y)
  (let* ((xi (validate-parse-interval-operation-parameter x))
         (yi (validate-parse-interval-operation-parameter y))
         (xi-class (classify-interval xi))
         (yi-class (classify-interval yi))
         (a (inf xi))
         (b (sup xi))
         (c (inf yi))
         (d (sup yi)))
    (cond
     ((null y)
       (interval
         (div-reciprocal (sup xi))
         (div-reciprocal (inf xi))))
     ((eq yi-class 'z)
       (error "division by zero"))
     ((eq xi-class 'z)
       (interval 0))
     ((eq yi +empty-interval+)
       (error "Division by empty interval"))
     ((eq xi-class 'p0)
       (cond
        ((eq yi-class 'p0)
          (interval 0 +pos-infinity+))
        ((eq yi-class 'p1)
          (interval 0 (/e b c)))
        ((eq yi-class 'm)
          (whole))
        ((eq yi-class 'n0)
          (interval +neg-infinity+ 0))
        ((eq yi-class 'n1)
          (interval (/e b d) 0))))
     ((eq xi-class 'p1)
       (cond
        ((eq yi-class 'p0)
          (extended-interval '(0)
            (cons-interval (/e a d) +pos-infinity+)))
        ((eq yi-class 'p1)
          (extended-interval '(0)
            (cons-interval (/e a d) (/e b c))))
        ((eq yi-class 'm)
          (extended-interval '(0)
            (cons-interval +neg-infinity+ (/e a c))
            (cons-interval (/e a d) +pos-infinity+)))
        ((eq yi-class 'n0)
          (extended-interval '(0)
            (cons-interval +neg-infinity+ (/e a c))))
        ((eq yi-class 'n1)
          (extended-interval '(0)
            (cons-interval (/e b d) (/e a c))))))
     ((eq xi-class 'm)
       (cond
        ((eq yi-class 'p0)
          (whole))
        ((eq yi-class 'p1)
          (interval (/e a c) (/e b c)))
        ((eq yi-class 'm)
          (whole))
        ((eq yi-class 'n0)
          (whole))
        ((eq yi-class 'n1)
          (interval (/e b c) (/e a d)))))
     ((eq xi-class 'n0)
       (cond
        ((eq yi-class 'p0)
          (interval +neg-infinity+ 0))
        ((eq yi-class 'p1)
          (interval (/e a c) 0))
        ((eq yi-class 'm)
          (whole))
        ((eq yi-class 'n0)
          (interval 0 +pos-infinity+))
        ((eq yi-class 'n1)
          (interval 0 (/e a d)))))

     ((eq xi-class 'n1)
       (cond
        ((eq yi-class 'p0)
          (extended-interval '(0)
            (cons-interval +neg-infinity+ (/e b d))))
        ((eq yi-class 'p1)
          (extended-interval '(0)
            (cons-interval (/e a c) (/e b d))))
        ((eq yi-class 'm)
          (extended-interval '(0)
            (cons-interval +neg-infinity+ (/e b d))
            (cons-interval (/e b c) +pos-infinity+)))
        ((eq yi-class 'n0)
          (extended-interval '(0)
            (cons-interval (/e c c) +neg-infinity+)))
        ((eq yi-class 'n1)
          (extended-interval '(0)
            (cons-interval (/e b c) (/e a d)))))))))


;!===== PRINT AND TO STRING FUNCTIONS =====!
(defun extended-real-to-string (x &optional (unicode t))
  (cond
   ((is-extended-real x)
     (if (eq x +neg-infinity+)
         (if unicode
             (format nil "-~a" #\INFINITY)
             "-infinity")
         (if (eq x +pos-infinity+)
             (if unicode
                 (format nil "+~a" #\INFINITY)
                 "+infinity")
             x)))
   (T
     x)))

(defun exclusion-points-to-string (points &optional (unicode T))
  (if (null points)
      ""
      (let ((set-minus-symbol (if unicode #\SET_MINUS "-")))
        (format nil " ~a {~{~a~^, ~}}"
          set-minus-symbol
          (mapcar (lambda (x)
                    (extended-real-to-string x unicode))
              points)))))

(defun cons-interval-to-string (i &optional (unicode T))
  (if (not (is-cons-interval i))
      (error "The provided value ~A is not a cons interval" i))
  (let ((l (extended-real-to-string (car i) unicode))
        (h (extended-real-to-string (cdr i) unicode)))
    (format nil "[~a, ~a]" l h)))

(defun interval-to-string (i &optional (unicode T))
  (cond
   ((not (is-interval i))
     nil)
   ((is-empty i)
     (if unicode
         (format nil "∅")
         "()"))
   ((is-singleton i)
     ;! there shouldnt be any exclusion points
     (let ((l (extended-real-to-string (inf i) unicode)))
       (format nil "[~a, ~a]" l l)))
   ((is-single-interval i)
     (format nil "~a~a"
       (cons-interval-to-string (caar i) unicode)
       (exclusion-points-to-string (cdr i) unicode)))
   (T
     (let ((intervals-to-string
            (mapcar
                (lambda (interval)
                  (cons-interval-to-string interval unicode))
                (car i))))
       ;  the diplication is required because the format
       ;  doesnt allow more than 1 argument while formatting
       ;  a list
       ;  there might be a more elegant way but i coundlt find a 
       ;  format directive to achive this
       (if unicode
           (format nil "~{~a~^ ∪ ~}~a"
             intervals-to-string
             (exclusion-points-to-string (cdr i) unicode))
           (format nil "~{~a~^ U ~}~a"
             intervals-to-string
             (exclusion-points-to-string (cdr i) unicode)))))))

(defun print-interval (i &optional (unicode t) (stream T))
  (format stream "~a" (interval-to-string i unicode)))
