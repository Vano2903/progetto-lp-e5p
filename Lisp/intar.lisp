;; -*- Mode: Lisp -*-

;; Gianfarelli	Giorgio	894499
;; Lauria	Luca	900326
;; Vanoncini	Davide	903214


;; Interval Arithmetic
;; This file contains the implementation 
;; of the interval arithmetic operations

(defconstant +neg-infinity+ 'neg-infinity "Negative infinity")

(defconstant +pos-infinity+ 'pos-infinity "Positive infinity")

(defconstant +empty-interval+ () "Empty interval")

;;!===== auxiliary functions =====!
;; returns the concatenation of two lists
(defun concat-lists (l1 l2)
  (if (null l1)
      l2
      (cons
       (car l1)
       (concat-lists (cdr l1) l2))))

;; ! this function might not be needed
;; an implementation of reverse just because im not sure
;; if i can use reverse or not
(defun rev (l)
  (cond
    ((null l)
     '())
    (T
     (append (rev (cdr l)) (list (car l))))))

;;!===== validation functions =====!
;; checks if x is a real number
;; if not returns an error message
;; name can be used to specify the name of the variable
(defun validate-real-extended-number (x name)
  (if (not (is-extended-real x))
      (error "value ~a is invalid, 
            ~a must be an extended real number" x name)))

;; checks if x is a valid interval or an extended real
;; if it's none of the two returns an error message
;; if it's a valid extended real it returns the
;; singleton containing the extended real
(defun validate-parse-interval-operation-parameter (x)
  (if (not (or (is-interval x)
               (is-extended-real x)))
      (error "The provided value ~A is not a valid value, 
          must be interval or extended real" x))
  (if (is-extended-real x)
      (interval x)
      x))

;; checks if a list is empty or every item
;; is an extended real
(defun is-valid-exclusion-list (i)
  (cond
    ((null i) T)
    ((not (consp i)) nil)
    (T
     (every #'is-extended-real i))))

;; checks if the list is empty or every item
;; is a cons interval or the empty interval
;; the definition of cons interval is in the documentation
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

;; checks if i is a single interval
;; a single interval is not a disjoint interval
;; TODO: what should happen with exclusion points
(defun is-single-interval (i)
  (and (is-interval i)
       (eq (length (get-interval-list i)) 1)
       (is-cons-interval
        (car (get-interval-list i)))))

;; checks if a cons interval a is contained in b
(defun is-cons-interval-contained (a b)
  (and (or (extended-real-gt (car b) (car a))
           (extended-real-eq (car b) (car a)))
       (or (extended-real-gt (cdr a) (cdr b))
           (extended-real-eq (cdr a) (cdr b)))))
;;!===== extended reals =====!
;;!======= extended reals definitions

;; checks if x is either +neg-infinity+ or +pos-infinity+
(defun is-infinity (x)
  (or (eq x +neg-infinity+) (eq x +pos-infinity+)))

;; checks if x is a number or infinity
(defun is-extended-real (x)
  (or (numberp x) (is-infinity x)))

;;!======= extended real comparisons

;; checks if x is greater than y
;; if x or y are not extended reals returns nil
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

;; checks if x is equal to y
;; if x or y are not extended reals returns nil
(defun extended-real-eq (x y)
  (if (and (is-extended-real x) (is-extended-real y))
      (eq x y)))

;; if is-min is T it returns the lowest extended-real
;; of the list otherwise it returns the highest
;; this function suppose that the list is made of
;; extended reals
(defun extended-real-min-max-filtered (find-min x &rest xs)
  (if (null xs)
      x
      (let ((rest-min-max
             (apply #'extended-real-min-max-filtered find-min xs)))
        (if (not find-min)
            (if (extended-real-gt x rest-min-max)
                x
                rest-min-max)
            (if (extended-real-gt x rest-min-max)
                rest-min-max
                x)))))

;; the function filters any non extended-real value
;; and returns the lowest value if find-min is T
;; otherwise the highest value
;; if no value was found it returns nil
(defun extended-real-min-max (find-min x &rest xs)
  (let ((filtered-list
         (remove-if-not #'is-extended-real (cons x xs))))
    (if (null filtered-list)
        nil
        (apply #'extended-real-min-max-filtered
               find-min
               filtered-list))))

;; returns the lowest extended-real of the list
;; if no extended-real is found it returns nil
(defun extended-real-min (x &rest xs)
  (apply #'extended-real-min-max T x xs))

;; returns the highest extended-real of the list
;; if no extended-real is found it returns nil
(defun extended-real-max (x &rest xs)
  (apply #'extended-real-min-max nil x xs))

;;!======= operation tables 

;; this function return the sum of x and y 
;; considering the extended real numbers
;; if the sum is not defined it returns nil
;; if x or y are not extended reals it returns an error
;; signalling which variable is not an extended real
(defun extended-real-sum (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
  (cond
    ((and (numberp x) (numberp y))
     (+ x y))
    ((or (and (eq x +neg-infinity+)
              (eq y +pos-infinity+))
         (and (eq x +pos-infinity+)
              (eq y +neg-infinity+)))
     nil)
    ((eq x +neg-infinity+) +neg-infinity+)
    ((eq y +neg-infinity+) +neg-infinity+)
    ((eq x +pos-infinity+) +pos-infinity+)
    ((eq y +pos-infinity+) +pos-infinity+)))

;; this function return the difference of x and y 
;; considering the extended real numbers
;; if the difference is not defined it returns nil
;; if x or y are not extended reals it returns an error
;; signalling which variable is not an extended real
(defun extended-real-sub (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
  (cond
    ((and (numberp x) (numberp y))
     (- x y))
    ((or (and (eq x +neg-infinity+)
              (eq y +neg-infinity+))
         (and (eq x +pos-infinity+)
              (eq y +pos-infinity+)))
     nil)
    ((eq x +neg-infinity+) +neg-infinity+)
    ((eq y +neg-infinity+) +pos-infinity+)
    ((eq x +pos-infinity+) +pos-infinity+)
    ((eq y +pos-infinity+) +neg-infinity+)))

;; this function return the multiplication of x and y 
;; considering the extended real numbers
;; if the multiplication is not defined it returns nil
;; if x or y are not extended reals it returns an error
;; signalling which variable is not an extended real
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
     ;;  commutative property
     (extended-real-mul y x))))

;; this function return the division of x and y 
;; considering the extended real numbers
;; if the division is not defined it returns nil
;; if x or y are not extended reals it returns an error
;; signalling which variable is not an extended real
(defun extended-real-div (x y)
  (validate-real-extended-number x "X")
  (validate-real-extended-number y "Y")
  (cond
    ((eq y 0) nil)
    ((and (numberp x) (numberp y)) (/ x y))
    ((or (and (eq x +neg-infinity+)
              (eq y +neg-infinity+))
         (and (eq x +pos-infinity+)
              (eq y +pos-infinity+)))
     nil)
    ;;  it could be incorporated in the "or"
    ;;  but it's more readable this way
    ((or (and (eq x +pos-infinity+)
              (eq y +neg-infinity+))
         (and (eq x +neg-infinity+)
              (eq y +pos-infinity+)))
     nil)
    ((eq x +neg-infinity+)
     (if (> y 0)
         +neg-infinity+
         +pos-infinity+))
    ((eq x +pos-infinity+)
     (if (> y 0)
         +pos-infinity+
         +neg-infinity+))
    ((or (eq y +neg-infinity+)
         (eq y +pos-infinity+))
     0)))

;; find the complement of x by subtracting x from o
;; if the complement is not defined an error is
;; signalled 
(defun sub-reciprocal (x)
  (let ((result (extended-real-sub 0 x)))
    (if (or (null result)
            (is-infinity x))
        (error "The complement of ~a is not defined" x)
        result)))

;; find the reciprocal of x by dividing 1 by x
;; if the reciprocal is not defined an error is
;; signalled 
(defun div-reciprocal (x)
  (let ((result (extended-real-div 1 x)))
    (if (or (null result)
            (is-infinity x))
        (error "The reciprocal of ~a is not defined" x)
        result)))

;;!===== interval functions =====!

(defun sort-cons-intervals (intervals)
  (sort intervals
	(lambda (x y)
          (extended-real-gt (car y) (car x)))))

;; sort interval it sorts the interval
;; if the interval is not a disjoint interval it returns it "as is"
;; if the interval is a disjoint interval it returns the sorted interval
;; by the lower bound
;; if the interval is not an interval it returns nil
(defun sort-interval (interval)
  (cond
    ((or (null interval)
         (not (is-interval interval)))
     nil)
    ((or (is-empty interval)
         (is-single-interval interval))
     interval)
    (T
     (let ((intervals (get-interval-list interval)))
       (apply #'extended-interval
              (get-exclusion-list interval)
              (sort-cons-intervals intervals))))))

;; the function merges two cons intervals
;; if there is an overlap it returns the merged interval
;; otherwise it returns nil
(defun merge-cons-intervals (c1 c2)
  (let* ((a (car c1))
         (b (cdr c1))
         (c (car c2))
         (d (cdr c2)))

    ;; check that (a <= b and c <= d)
    ;; Swap if a > b
    (let ((a (if (extended-real-gt a b) b a))
          (b (if (extended-real-gt a b) a b))
          ;; Swap if c > d
          (c (if (extended-real-gt c d) d c))
          (d (if (extended-real-gt c d) c d)))

      (if (and (not (extended-real-gt a d)) ;; a <= d
               (not (extended-real-gt c b))) ;; c <= b
          (cons (extended-real-min a c)
                (extended-real-max b d))
          ;;no overlap
          nil))))


;; merges a list of cons intervals
;; the output might not be in order
(defun merge-list-cons-intervals
    (intervals &optional (merged-intervals '()))
  (cond
    ((null intervals)
     (reverse merged-intervals))
    ((null (cdr intervals))
     ;;  append on the left
     (reverse (cons (car intervals) merged-intervals)))
    (t
     (let* ((merged (merge-cons-intervals
                     (car intervals)
                     (cadr intervals))))
       (if (not merged)
           (merge-list-cons-intervals
            (cdr intervals)
            (cons (car intervals) merged-intervals))
           (merge-list-cons-intervals
            (cons merged (cddr intervals))
            merged-intervals))))))

;; this function handles the merging operation
;; of an interval
;; if the interval is nil or not an interval it returns nil
;; if the interval is the empty interval or a single interval it returns
;; the interval itself as it's already "merged"
;; if the interval is a disjoint interval 
;; it returns the merged and sorted interval
(defun sort-and-merge-interval (interval)
  (cond
    ((or (null interval)
         (not (is-interval interval)))
     nil)
    ((or (is-empty interval)
         (is-single-interval interval))
     interval)
    (t
     (let ((intervals (sort-cons-intervals (get-interval-list interval))))
       (apply #'extended-interval
              (get-exclusion-list interval)
              (sort-cons-intervals (merge-list-cons-intervals intervals)
                ))))))


;; checks if an interval is a well defined 
;; cons interval
;; a cons interval is defined in the documentation
;; this function returns nil if the interval is not
;; or if it's the empty interval
(defun is-cons-interval (i)
  (if (not (consp i))
      nil
      (let ((l (car i))
            (h (cdr i)))
        (and (is-extended-real l)
             (is-extended-real h)
             (or (extended-real-gt h l)
                 (extended-real-eq l h))))))

;; the function returns nil if the interval provided
;; is not a cons-interval
;; the classification is based on the following table
;; p0 = a,b | 0 <= a <= b V b != 0
;; p1 = a,b | 0 < a <= b
;; z = 0,0
;; n0 = a,b | a <= b <= 0 V a != 0
;; n1 = a,b | a <= b < 0
;; M = a,b | a < 0 < b
(defun classify-cons-interval (i exclusion-points)
  (if (is-cons-interval i)
      (let ((l (car i))
            (h (cdr i)))
        (cond
          ((extended-real-eq l h)
           ;;  todo: add exclusion points check
           ;;  if there is an exclusion point on 0
           (cond ((extended-real-eq l 0) 'z)
                 ((extended-real-gt l 0) 'p1)
                 ((extended-real-gt 0 l) 'n1)))
          ;;  l == 0 and h > 0 -> p0
          ;;  if h == 0 is a singleton or
          ;;  if h < 0 is an invalid interval
          ((extended-real-eq l 0)
           (if (member 0 exclusion-points)
               'p1
               'p0))
          ;;  l>0 and h > l -> p0
          ((extended-real-gt l 0) 'p1)
          ;;  l<0 and h == 0 -> n0
          ;;  l<0 and h > 0 -> M
          ;;  l<0 and h < 0 -> n1
          ((extended-real-gt 0 l)
           (cond
             ((extended-real-eq h 0)
              (if (member 0 exclusion-points)
                  'n1
                  'n0))
             ((extended-real-gt h 0) 'm)
             ((extended-real-gt 0 h) 'n1)))))))

;; the function classifies an interval
;; based on the classification of the cons intervals
;; the function returns nil if the interval is not
;; a single interval
(defun classify-interval (i)
  (if (is-single-interval i)
      (classify-cons-interval
       (caar i)
       (get-exclusion-list i))))

;; this function creates a cons interval
;; if h is not provided it returns a singleton interval
;; if h < l it returns the empty interval
;; otherwise it returns the cons interval
;; it returns an error if l or h are not extended reals
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

;; this function creates a complex interval
;; it can handle empty intervals, singletons, single intervals,
;; disjoint intervals and intervals with exclusion points
;; if exclusion points and intervals are nil it returns the empty interval
;; if intervals and exclusion points are not valid it returns an error
;; (checks is-valid-interval-list and is-valid-exclusion-list functions)
(defun extended-interval (exclusions &rest intervals)
  (cond
    ((and (null exclusions)
          (null intervals))
     (empty-interval))
    ;;  TODO: should add a check if the intervals with the exclusions
    ;;  make sense
    ;;  like [1,1]\{1} doesn't make sense
    ((or (not (is-valid-interval-list intervals))
         (not (is-valid-exclusion-list exclusions)))
     (error "The provided values ~A and ~A are not valid intervals"
	    intervals exclusions))
    ((and
      (eq (length intervals) 1)
      (null (car intervals)))
     (empty-interval))
    (T (cons intervals exclusions))))

;;* get-interval-list and get-exclusion-list
;;* are not necessary but they make the code more readable

;; returns the list of intervals given an interval
(defun get-interval-list (i)
  (if (is-interval i)
      (car i)
      (error "The provided value ~A is not an interval" i)))

;; returns the list of exclusion points given an interval
(defun get-exclusion-list (i)
  (if (is-interval i)
      (cdr i)
      (error "The provided value ~A is not an interval" i)))

;;!======= operation tables on cons intervals
;; returns the sorted combination of the two lists
;; and removes duplicates and possible nils
;; the lists must be containing extended reals values
(defun sort-and-merge-exclusion-lists (l1 l2)
  (remove nil
	  (remove-duplicates
           (sort (concat-lists l1 l2)
		 (lambda (x y)
                   (not (extended-real-gt x y)))))))

;; given two lists of cons intervals it applies the binary
;; operation passed as lambda and returns a list 
;; with the results
(defun combine-cons-intervals-with-operation (x y operation)
  (let ((combined-intervals
         (mapcar (lambda (xi)
                   (mapcar (lambda (yi)
                             (funcall operation xi yi))
			   y))
		 x)))
    (mapcan #'identity combined-intervals)))

;; returns the sum of two cons intervals
;; the sum table can be found in the documentation
(defun sum-cons-intervals (c1 c2)
  (cons-interval
   (+e (car c1) (car c2))
   (+e (cdr c1) (cdr c2))))

;; returns the sub of two cons intervals
;; the sub table can be found in the documentation
(defun sub-cons-intervals (c1 c2)
  (cons-interval
   (-e (car c1) (cdr c2))
   (-e (cdr c1) (car c2))))

;; return multiplication of two cons intervals
;; the multiplication table can be found in the documentation
(defun mul-cons-intervals (c1 c2)
  (cons-interval
   (extended-real-min
    (*e (car c1) (car c2))
    (*e (car c1) (cdr c2))
    (*e (cdr c1) (car c2))
    (*e (cdr c1) (cdr c2)))
   (extended-real-max
    (*e (car c1) (car c2))
    (*e (car c1) (cdr c2))
    (*e (cdr c1) (car c2))
    (*e (cdr c1) (cdr c2)))))

;; return division of two cons intervals
;; it returns the empty interval if the division is not defined
;; the division table can be found in the documentation
(defun div-cons-intervals (c1 c1-class c2 c2-class)
  (let ((a (car c1))
        (b (cdr c1))
        (c (car c2))
        (d (cdr c2)))
    (cond
      ((eq c2-class 'z)
       (error "division by zero"))
      ((eq c1-class 'z)
       (interval 0))
      ((eq c2 +empty-interval+)
       (error "Division by empty interval"))
      ((eq c1-class 'p0)
       (cond
         ((eq c2-class 'p0)
          (interval 0 +pos-infinity+))
         ((eq c2-class 'p1)
          (interval 0 (/e b c)))
         ((eq c2-class 'm)
          (whole-interval))
         ((eq c2-class 'n0)
          (interval +neg-infinity+ 0))
         ((eq c2-class 'n1)
          (interval (/e b d) 0))))
      ((eq c1-class 'p1)
       (cond
         ((eq c2-class 'p0)
          (extended-interval '(0)
			     (cons-interval (/e a d) +pos-infinity+)))
         ((eq c2-class 'p1)
          (extended-interval '(0)
			     (cons-interval (/e a d) (/e b c))))
         ((eq c2-class 'm)
          (extended-interval '(0)
			     (cons-interval +neg-infinity+ (/e a c))
			     (cons-interval (/e a d) +pos-infinity+)))
         ((eq c2-class 'n0)
          (extended-interval '(0)
			     (cons-interval +neg-infinity+ (/e a c))))
         ((eq c2-class 'n1)
          (extended-interval '(0)
			     (cons-interval (/e b d) (/e a c))))))
      ((eq c1-class 'm)
       (cond
         ((eq c2-class 'p0)
          (whole-interval))
         ((eq c2-class 'p1)
          (interval (/e a c) (/e b c)))
         ((eq c2-class 'm)
          (whole-interval))
         ((eq c2-class 'n0)
          (whole-interval))
         ((eq c2-class 'n1)
          (interval (/e b c) (/e a d)))))
      ((eq c1-class 'n0)
       (cond
         ((eq c2-class 'p0)
          (interval +neg-infinity+ 0))
         ((eq c2-class 'p1)
          (interval (/e a c) 0))
         ((eq c2-class 'm)
          (whole-interval))
         ((eq c2-class 'n0)
          (interval 0 +pos-infinity+))
         ((eq c2-class 'n1)
          (interval 0 (/e a d)))))

      ((eq c1-class 'n1)
       (cond
         ((eq c2-class 'p0)
          (extended-interval '(0)
			     (cons-interval +neg-infinity+ (/e b d))))
         ((eq c2-class 'p1)
          (extended-interval '(0)
			     (cons-interval (/e a c) (/e b d))))
         ((eq c2-class 'm)
          (extended-interval '(0)
			     (cons-interval +neg-infinity+ (/e b d))
			     (cons-interval (/e b c) +pos-infinity+)))
         ((eq c2-class 'n0)
          (extended-interval '(0)
			     (cons-interval (/e c c) +neg-infinity+)))
         ((eq c2-class 'n1)
          (extended-interval '(0)
			     (cons-interval (/e b c) (/e a d)))))))))

;; merges a list of division cons intervals as 
;; they might generate a disjoint interval or 
;; exclusion points
(defun merge-division-intervals-into-interval (lst)
  (cons
   (mapcar #'caar lst)
   (remove-duplicates (mapcar #'cadr lst))))

;;!===== API FUNCTIONS =====!

;; executes the sum of x and y if both are extended reals
;; otherwise it returns an error
;; if no arguments are provided it returns 0
;; if only one argument is provided it returns the argument
;; if the sum is not defined it returns an error
(defun +e (&optional x y)
  (cond
    ((and (null x) (null y)) 0)
    ((null y)
     (if (is-extended-real x)
         x
         (error "The provided value ~A must be an extended real" x)))
    (T (let ((result (extended-real-sum x y)))
         (if (null result)
             (error "The sum of ~a and ~a is not defined" x y)
             result)))))

;; executes the multiplication of x and y if both are extended reals
;; otherwise it returns an error
;; if no arguments are provided it returns 0
;; if only one argument is provided it returns the argument
;; if the multiplication is not defined it returns an error
(defun *e (&optional x y)
  (cond
    ((and (null x) (null y)) 1)
    ((null y)
     (if (is-extended-real x)
         x
         (error "The provided value ~A must be an extended real" x)))
    (T (let ((result (extended-real-mul x y)))
         (if (null result)
             (error "The multiplication of ~a and ~a is not defined" x y)
             result)))))

;; executes the difference of x and y if both are extended reals
;; otherwise it returns an error
;; if one argument is provided it returns the complement of the argument
;; if the difference is not defined it returns an error
(defun -e (x &optional y)
  (if (null y)
      (sub-reciprocal x)
      (let ((result (extended-real-sub x y)))
        (if (null result)
            (error "The difference of ~a and ~a is not defined" x y)
            result))))

;; executes the division of x and y if both are extended reals
;; otherwise it returns an error
;; if one argument is provided it returns the complement of the argument
;; if the division is not defined it returns an error
(defun /e (x &optional y)
  (if (null y)
      (div-reciprocal x)
      (let ((result (extended-real-div x y)))
        (if (null result)
            (error "The division of ~a and ~a is not defined" x y)
            result))))

;; returns the empty interval
(defun empty-interval () +empty-interval+)

;; creates an interval
;; if no value i provided it returns the empty interval
;; if a single value is provided is returns a singleton 
;; with that value
;; or if both values are provided it returns an interval
;; if h<l it returns the empty interval
;; both l and h must be extended reals
(defun interval (&optional l h)
  (cond
    ((and (null l) (null h))
     (empty-interval))
    (T (extended-interval nil (cons-interval l h)))))

;; returns the whole-interval set of extended reals
;; so the interval from -infinity to +infinity
(defun whole-interval ()
  (interval +neg-infinity+ +pos-infinity+))

;; checks if i is an interval
;; it can be an empty interval
;; a singleton
;; a single interval
;; a disjoint interval
;; an interval with exclusion points
;; TODO: add more checks for the exclusion points
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

;; checks if i is the empty interval
;; if the value i is not an interval it returns an error
(defun is-empty (i)
  (if (is-interval i)
      (or (eq i +empty-interval+))
      (error "The provided value ~A is not an interval" i)))

;; checks if i is a singleton interval
;; a singleton interval is an interval with the same
;; lower and upper bound
;; if the value i is not an interval it returns an error
(defun is-singleton (i)
  (cond
    ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
    ((is-empty i)
     nil)
    (t
     (and (is-single-interval i)
          (extended-real-eq (inf i) (sup i))))))

;; returns the lower bound of an interval
;; the lower bound of the empty interval does not exists
;; if the value i is not an interval it returns an error
(defun inf (i)
  (cond
    ((is-empty i)
     (error "The interval is empty"))
    ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
    (T
     (caar (get-interval-list (sort-and-merge-interval i))))))

;; returns the upper bound of an interval
;; the upper bound of the empty interval does not exists
;; if the value i is not an interval it returns an error
(defun sup (i)
  (cond
    ((is-empty i)
     (error "The interval is empty"))
    ((not (is-interval i))
     (error "The provided value ~A is not an interval" i))
    (T
     (cdar (last (car (sort-and-merge-interval i)))))))

;; contains checks if the interval i contains the value x
;; x can be an interval or an extended real
;; if x is an interval it checks if all the intervals in x
;; are contained in i
;; if x is an extended real it checks if x is contained in i
;; it will also check for exclusion points in case x is an extended real
;; if x is not an interval or an extended real it returns an error
;; if i is not an interval it returns an error
(defun contains (i x)
  (cond
    ((or (not (is-interval i)) (is-empty i))
     (error "The provided value ~A is not a valid interval" i))
    ((is-interval x)
     (let ((x-cons-intervals
            (get-interval-list (sort-and-merge-interval x)))
           (i-cons-intervals
            (get-interval-list (sort-and-merge-interval i))))
       (every (lambda (x-interval)
                (some (lambda (i-interval)
                        (or (is-cons-interval-contained
                             x-interval
                             i-interval)
                            (is-cons-interval-contained
                             i-interval
                             x-interval)))
                      i-cons-intervals))
              x-cons-intervals)))
    ((is-extended-real x)
     (let ((i-cons-intervals
            (get-interval-list (sort-and-merge-interval i)))
           (exclusion-points (sort-and-merge-exclusion-lists
                              (get-exclusion-list i) nil)))
       (some (lambda (i-interval)
               (and (or (is-cons-interval-contained
                         (cons x x)
                         i-interval)
                        (is-cons-interval-contained
                         i-interval
                         (cons x x)))
                    (not (member x exclusion-points))))
             i-cons-intervals)))
    (T
     (error "The provided value ~A is not a valid value,
          must be an interval or an extended real" x))))

;; overlap checks if two intervals overlap
;; if a disjoint interval is provided it checks
;; if at least one of the intervals in the first
;; interval overlaps with one of the intervals in the second
;; if the intervals provided are not intervals or empty intervals
;; it returns nil
(defun overlap (i1 i2)
  ;; todo: should add a check for the exclusion points
  (cond
    ((or (not (is-interval i1))
         (is-empty i1))
     (error "The provided value ~A is not a valid interval" i1))
    ((or (not (is-interval i2))
         (is-empty i2))
     (error "The provided value ~A is not a valid interval" i2))
    (T
     (let ((i1-intervals (get-interval-list i1))
           (i2-intervals (get-interval-list i2)))
       (if (some (lambda (x)
                   (some (lambda (y)
                           (merge-cons-intervals x y))
			 i2-intervals))
		 i1-intervals)
           t)))))

;; i+ returns the sum of the intervals x and y
;; if x or y are not intervals or extended reals it returns an error
;; if x or y are extended reals they will be considered as singletons
;; if no arguments are provided it will return the singelton 0
;; if only one argument is provided it will return the argument 
;; (or the equivalent singleton)
;; if the sum is not defined it returns an error
;; the definition of the following calculation
;; can be found in the documentation
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
      (t
       (sort-interval
        (apply #'extended-interval
               (sort-and-merge-exclusion-lists
		(get-exclusion-list xi)
		(get-exclusion-list yi))
               (combine-cons-intervals-with-operation
		(get-interval-list xi)
		(get-interval-list yi)
		#'sum-cons-intervals)))))))

;; i+ returns the difference of the intervals x and y
;; if x or y are not intervals or extended reals it returns an error
;; if x or y are extended reals they will be considered as singletons
;; if only one argument is provided it will return the complement 
;; of the argument (or of the equivalent singleton)
;; if the difference is not defined it returns an error
;; the definition of the following calculation
;; can be found in the documentation
(defun i- (x &optional y)
  (let ((xi (validate-parse-interval-operation-parameter x))
        (yi (validate-parse-interval-operation-parameter y)))
    (cond
      ((null y)
       (interval
        (sub-reciprocal (sup xi))
        (sub-reciprocal (inf xi))))
      (T
       (sort-interval
        (apply #'extended-interval
               (sort-and-merge-exclusion-lists
		(get-exclusion-list xi)
		(get-exclusion-list yi))
               (combine-cons-intervals-with-operation
		(get-interval-list xi)
		(get-interval-list yi)
		#'sub-cons-intervals)))))))

;; i+ returns the multiplication of the intervals x and y
;; if x or y are not intervals or extended reals it returns an error
;; if x or y are extended reals they will be considered as singletons
;; if no arguments are provided it will return the singelton 1
;; if only one argument is provided it will return the argument 
;; (or the equivalent singleton)
;; if the multiplication is not defined it returns an error
;; the definition of the following calculation
;; can be found in the documentation
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
      (t
       (sort-interval
        (apply #'extended-interval
               (sort-and-merge-exclusion-lists
		(get-exclusion-list xi)
		(get-exclusion-list yi))
               (combine-cons-intervals-with-operation
		(get-interval-list xi)
		(get-interval-list yi)
		#'mul-cons-intervals)))))))

;; i/ returns the division of the intervals x and y
;; if x or y are not intervals or extended reals it returns an error
;; if x or y are extended reals they will be considered as singletons
;; if only one argument is provided it will return the reciprocal 
;; of the argument (or of the equivalent singleton)
;; if the division is not defined it returns an error
;; the definition of the following calculation
;; can be found in the documentation
(defun i/ (x &optional y)
  (let* ((xi (validate-parse-interval-operation-parameter x))
         (yi (validate-parse-interval-operation-parameter y)))
    (cond
      ((null y)
       (if (eq (classify-interval xi) 'm)
           (extended-interval '()
			      (cons-interval +neg-infinity+ (div-reciprocal (inf xi)))
			      (cons-interval (div-reciprocal (sup xi)) +pos-infinity+))
           (interval
            (div-reciprocal (inf xi))
            (div-reciprocal (sup xi)))))
      (t
       (let ((division-interval
              (merge-division-intervals-into-interval
               (combine-cons-intervals-with-operation
                (get-interval-list xi)
                (get-interval-list yi)
                (lambda (c1 c2)
                  (let ((c1-class (classify-cons-interval
                                   c1
                                   (get-exclusion-list xi)))
                        (c2-class (classify-cons-interval
                                   c2
                                   (get-exclusion-list yi))))
                    (div-cons-intervals c1 c1-class c2 c2-class)))))))
         (sort-interval
          (apply #'extended-interval
		 (sort-and-merge-exclusion-lists
                  (sort-and-merge-exclusion-lists
                   (get-exclusion-list xi)
                   (get-exclusion-list yi))
                  (cdr division-interval))
		 (car division-interval))))))))

;;!===== PRINT AND TO STRING FUNCTIONS =====!
;; the print functions are auxiliary functions
;; to have a pretty print of the intervals as their 
;; sexp can be hard to read

;; the extended-real-to-string function returns a string
;; representation of the extended real number
;; if a value that is not an extended real is provided
;; it will just return the provided value "as is"
;; the unicode parameter is used to specify if the returned value
;; must be in ascii code or can use unicode characters
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
    (T x)))

;; returns a string representation of the exclusion points
;; if the list is empty it returns an empty string
;; otherwise it returns a string with the exclusion points
;; separated by a comma
;; the unicode parameter is used to specify if the returned value
;; must be in ascii code or can use unicode characters
(defun exclusion-points-to-string (points &optional (unicode T))
  (if (null points)
      ""
      (let ((set-minus-symbol (if unicode #\SET_MINUS "-")))
        (format nil " ~a {~{~a~^, ~}}"
		set-minus-symbol
		(mapcar (lambda (x)
			  (extended-real-to-string x unicode))
			points)))))

;; returns a string representation of a cons interval
;; if the provided value is not a cons interval it returns an error
;; the unicode parameter is used to specify if the returned value
;; must be in ascii code or can use unicode characters
(defun cons-interval-to-string (i &optional (unicode T))
  (if (not (is-cons-interval i))
      (error "The provided value ~A is not a cons interval" i))
  (let ((l (extended-real-to-string (car i) unicode))
        (h (extended-real-to-string (cdr i) unicode)))
    (format nil "[~a, ~a]" l h)))

;; returns a string representation of an interval
;; this function handles the different types of intervals
;; if the provided value is not an interval it returns nil
;; the unicode parameter is used to specify if the returned value
;; must be in ascii code or can use unicode characters
(defun interval-to-string (i &optional (unicode T))
  (cond
    ((not (is-interval i))
     nil)
    ((is-empty i)
     (if unicode
         (format nil "∅")
         "()"))
    ((is-singleton i)
     ;;! there shouldnt be any exclusion points
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
       ;;  the diplication is required because the format
       ;;  doesnt allow more than 1 argument while formatting
       ;;  a list
       ;;  there might be a more elegant way but i coundlt find a 
       ;;  format directive to achive this
       (if unicode
           (format nil "~{~a~^ ∪ ~}~a"
		   intervals-to-string
		   (exclusion-points-to-string (cdr i) unicode))
           (format nil "~{~a~^ U ~}~a"
		   intervals-to-string
		   (exclusion-points-to-string (cdr i) unicode)))))))

;; the print-interval function is a wrapper for the interval-to-string
;; function that prints the interval to the stream
;; the unicode parameter is used to specify if the returned value
;; must be in ascii code or can use unicode characters
(defun print-interval (i &optional (unicode t) (stream T))
  (format stream "~a" (interval-to-string i unicode)))
