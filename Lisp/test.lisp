(load (merge-pathnames "intar.lisp" *load-truename*))

(defun should-error (fn &rest args)
  (if (null args)
      (handler-case
          (progn (funcall fn) nil)
        (error () T))
      (handler-case
          (progn (apply fn args) nil)
        (error () T))))

;!= TEST 

;!=== ASSERTS ON EXTENDED REALS
;!===== asserts for infinity
(assert (is-infinity +neg-infinity+))
(assert (is-infinity +pos-infinity+))
(assert (not (is-infinity 0)))
;!===== asserts for extended reals
(assert (is-extended-real +neg-infinity+))
(assert (is-extended-real +pos-infinity+))
(assert (is-extended-real 0))
(assert (is-extended-real 0.1))
(assert (is-extended-real 1))
(assert (is-extended-real -1))
(assert (not (is-extended-real 'a)))
(assert (not (is-extended-real '(1 2 3))))
(assert (not (is-extended-real (cons 1 2))))

;!===== asserts comparisons on extended reals
;!======= asserts for greater than
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

;!======= asserts for equal
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

;!======= asserts for min on extended reals
(assert (equal +neg-infinity+ (extended-real-min +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-min +neg-infinity+ +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-min +neg-infinity+ +pos-infinity+)))
(assert (equal -1 (extended-real-min 1 2 3 -1)))

;!======= asserts for max on extended reals
(assert (equal +neg-infinity+ (extended-real-max +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-max +neg-infinity+ +neg-infinity+)))
(assert (equal +pos-infinity+ (extended-real-max +neg-infinity+ +pos-infinity+)))
(assert (equal 3 (extended-real-max 1 2 3 -1)))

;!===== asserts on operations with extended reals
;!======= sum asserts
; asserts for x=+neg-infinity+
(assert (equal +neg-infinity+ (extended-real-sum +neg-infinity+ +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-sum +neg-infinity+ -1)))
(assert (equal +neg-infinity+ (extended-real-sum +neg-infinity+ 0)))
(assert (equal +neg-infinity+ (extended-real-sum +neg-infinity+ 1)))
(assert (null (extended-real-sum +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (equal +neg-infinity+ (extended-real-sum -1 +neg-infinity+)))
(assert (> 0 (extended-real-sum -1 -1)))
(assert (> 0 (extended-real-sum -1 0)))
(assert (numberp (extended-real-sum -1 1)))
(assert (equal +pos-infinity+ (extended-real-sum -1 +pos-infinity+)))

; asserts for x=0
(assert (equal +neg-infinity+ (extended-real-sum 0 +neg-infinity+)))
(assert (> 0 (extended-real-sum 0 -1)))
(assert (equal 0 (extended-real-sum 0 0)))
(assert (< 0 (extended-real-sum 0 1)))
(assert (equal +pos-infinity+ (extended-real-sum 0 +pos-infinity+)))

; asserts for x=PR
(assert (equal +neg-infinity+ (extended-real-sum 1 +neg-infinity+)))
(assert (numberp (extended-real-sum 1 -1)))
(assert (< 0 (extended-real-sum 1 0)))
(assert (< 0 (extended-real-sum 1 1)))
(assert (equal +pos-infinity+ (extended-real-sum 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (null (extended-real-sum +pos-infinity+ +neg-infinity+)))
(assert (equal +pos-infinity+ (extended-real-sum +pos-infinity+ -1)))
(assert (equal +pos-infinity+ (extended-real-sum +pos-infinity+ 0)))
(assert (equal +pos-infinity+ (extended-real-sum +pos-infinity+ 1)))
(assert (equal +pos-infinity+ (extended-real-sum +pos-infinity+ +pos-infinity+)))

;!======= sub asserts
; asserts for x=+neg-infinity+
(assert (null (extended-real-sub +neg-infinity+ +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-sub +neg-infinity+ -1)))
(assert (equal +neg-infinity+ (extended-real-sub +neg-infinity+ 0)))
(assert (equal +neg-infinity+ (extended-real-sub +neg-infinity+ 1)))
(assert (equal +neg-infinity+ (extended-real-sub +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (equal +pos-infinity+ (extended-real-sub -1 +neg-infinity+)))
(assert (numberp (extended-real-sub -1 -1)))
(assert (> 0 (extended-real-sub -1 0)))
(assert (> 0 (extended-real-sub -1 1)))
(assert (equal +neg-infinity+ (extended-real-sub -1 +pos-infinity+)))

; asserts for x=0
(assert (equal +pos-infinity+ (extended-real-sub 0 +neg-infinity+)))
(assert (< 0 (extended-real-sub 0 -1)))
(assert (equal 0 (extended-real-sub 0 0)))
(assert (> 0 (extended-real-sub 0 1)))
(assert (equal +neg-infinity+ (extended-real-sub 0 +pos-infinity+)))

; asserts for x=PR
(assert (equal +pos-infinity+ (extended-real-sub 1 +neg-infinity+)))
(assert (< 0 (extended-real-sub 1 -1)))
(assert (< 0 (extended-real-sub 1 0)))
(assert (numberp (extended-real-sub 1 1)))
(assert (equal +neg-infinity+ (extended-real-sub 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (equal +pos-infinity+ (extended-real-sub +pos-infinity+ +neg-infinity+)))
(assert (equal +pos-infinity+ (extended-real-sub +pos-infinity+ -1)))
(assert (equal +pos-infinity+ (extended-real-sub +pos-infinity+ 0)))
(assert (equal +pos-infinity+ (extended-real-sub +pos-infinity+ 1)))
(assert (null (extended-real-sub +pos-infinity+ +pos-infinity+)))

;!======= mul asserts
; asserts for x=+neg-infinity+
(assert (equal +pos-infinity+ (extended-real-mul +neg-infinity+ +neg-infinity+)))
(assert (equal +pos-infinity+ (extended-real-mul +neg-infinity+ -1)))
(assert (null (extended-real-mul +neg-infinity+ 0)))
(assert (equal +neg-infinity+ (extended-real-mul +neg-infinity+ 1)))
(assert (equal +neg-infinity+ (extended-real-mul +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (equal +pos-infinity+ (extended-real-mul -1 +neg-infinity+)))
(assert (< 0 (extended-real-mul -1 -1)))
(assert (equal 0 (extended-real-mul -1 0)))
(assert (> 0 (extended-real-mul -1 1)))
(assert (equal +neg-infinity+ (extended-real-mul -1 +pos-infinity+)))

; asserts for x=0
(assert (null (extended-real-mul 0 +neg-infinity+)))
(assert (equal 0 (extended-real-mul 0 -1)))
(assert (equal 0 (extended-real-mul 0 0)))
(assert (equal 0 (extended-real-mul 0 1)))
(assert (null (extended-real-mul 0 +pos-infinity+)))

; asserts for x=PR
(assert (equal +neg-infinity+ (extended-real-mul 1 +neg-infinity+)))
(assert (> 0 (extended-real-mul 1 -1)))
(assert (equal 0 (extended-real-mul 1 0)))
(assert (< 0 (extended-real-mul 1 1)))
(assert (equal +pos-infinity+ (extended-real-mul 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (equal +neg-infinity+ (extended-real-mul +pos-infinity+ +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-mul +pos-infinity+ -1)))
(assert (null (extended-real-mul +pos-infinity+ 0)))
(assert (equal +pos-infinity+ (extended-real-mul +pos-infinity+ 1)))
(assert (equal +pos-infinity+ (extended-real-mul +pos-infinity+ +pos-infinity+)))

;!======= div asserts
; asserts for x=+neg-infinity+
(assert (null (extended-real-div +neg-infinity+ +neg-infinity+)))
(assert (equal +pos-infinity+ (extended-real-div +neg-infinity+ -1)))
(assert (null (extended-real-div +neg-infinity+ 0)))
(assert (equal +neg-infinity+ (extended-real-div +neg-infinity+ 1)))
(assert (null (extended-real-div +neg-infinity+ +pos-infinity+)))

; asserts for x=NR
(assert (equal 0 (extended-real-div -1 +neg-infinity+)))
(assert (< 0 (extended-real-div -1 -1)))
(assert (null (extended-real-div -1 0)))
(assert (> 0 (extended-real-div -1 1)))
(assert (equal 0 (extended-real-div -1 +pos-infinity+)))

; asserts for x=0
(assert (equal 0 (extended-real-div 0 +neg-infinity+)))
(assert (equal 0 (extended-real-div 0 -1)))
(assert (null (extended-real-div 0 0)))
(assert (equal 0 (extended-real-div 0 1)))
(assert (equal 0 (extended-real-div 0 +pos-infinity+)))

; asserts for x=PR
(assert (equal 0 (extended-real-div 1 +neg-infinity+)))
(assert (> 0 (extended-real-div 1 -1)))
(assert (null (extended-real-div 1 0)))
(assert (< 0 (extended-real-div 1 1)))
(assert (equal 0 (extended-real-div 1 +pos-infinity+)))

; asserts for x=+pos-infinity+
(assert (null (extended-real-div +pos-infinity+ +neg-infinity+)))
(assert (equal +neg-infinity+ (extended-real-div +pos-infinity+ -1)))
(assert (null (extended-real-div +pos-infinity+ 0)))
(assert (equal +pos-infinity+ (extended-real-div +pos-infinity+ 1)))
(assert (null (extended-real-div +pos-infinity+ +pos-infinity+)))

;!===== reciprocal asserts
(assert (should-error #'sub-reciprocal +neg-infinity+))
(assert (equal 1 (sub-reciprocal -1)))
(assert (equal 0 (sub-reciprocal 0)))
(assert (equal -1 (sub-reciprocal 1)))
(assert (should-error #'sub-reciprocal +pos-infinity+))

(assert (should-error #'sub-reciprocal +neg-infinity+))
(assert (equal -1/2 (div-reciprocal -2)))
(assert (should-error #'div-reciprocal 0)); 1/0 is not defined
(assert (equal 1/2 (div-reciprocal 2)))
(assert (should-error #'sub-reciprocal +pos-infinity+))

;!=== ASSERTS ON INTERVALS
;!===== asserts on cons intervals
; valid cons intervals
(assert (is-cons-interval (cons 0 1)))
(assert (is-cons-interval (cons +neg-infinity+ 1)))
(assert (is-cons-interval (cons 0 +pos-infinity+)))
(assert (is-cons-interval (cons +neg-infinity+ +pos-infinity+)))

; not a cons cell
(assert (not (is-cons-interval 0)))
(assert (not (is-cons-interval +neg-infinity+)))
; checks only single interval
(assert (not (is-cons-interval (list (cons 1 2) (cons 3 4)))))
; wrong order
(assert (not (is-cons-interval (cons 3 2))))
(assert (not (is-cons-interval +empty-interval+)))

;!===== asserts on interval classifications
; cannot classify empty interval or disjoint intervals
(assert (not (classify-interval (empty-interval))))
(assert (not (classify-interval (cons (list (cons 1 2) (cons 3 4)) (list)))))

; singletons
(assert (equal 'n1 (classify-interval (interval +neg-infinity+))))
(assert (equal 'n1 (classify-interval (interval -1))))
(assert (equal 'z (classify-interval (interval 0))))
(assert (equal 'p1 (classify-interval (interval 1))))
(assert (equal 'p1 (classify-interval (interval +pos-infinity+))))

; single intervals
(assert (equal 'n0 (classify-interval (interval +neg-infinity+ 0))))
(assert (equal 'n0 (classify-interval (interval -2 0))))

(assert (equal 'n1 (classify-interval (interval +neg-infinity+ -1))))
(assert (equal 'n1 (classify-interval (interval -2 -1))))

(assert (equal 'p0 (classify-interval (interval 0 +pos-infinity+))))
(assert (equal 'p0 (classify-interval (interval 0 1))))

(assert (equal 'p1 (classify-interval (interval 1 +pos-infinity+))))
(assert (equal 'p1 (classify-interval (interval 1 1))))

(assert (equal 'm (classify-interval (interval -1 1))))
(assert (equal 'm (classify-interval (whole))))


;!=== ASSERTS OF API FUNCTIONS
;!===== asserts for empty-interval
(assert (equal +empty-interval+ nil)) ; by standard definition
(assert (equal +empty-interval+ (empty-interval)))

;!===== asserts for is-empty
(assert (is-empty +empty-interval+))
(assert (is-empty nil))
(assert (not (is-empty (interval 0 1))))
(assert (not (is-empty (whole))))
(assert (should-error #'is-empty 0)) ;not an inverval

;!===== asserts for interval definition
(assert (is-empty (interval)))
(assert (is-interval (interval)))
(assert (equal (cons (list (cons 0 0)) (list)) (interval 0)))
(assert (is-interval (interval 0)))
(assert (is-singleton (interval 0)))
(assert (is-single-interval (interval 0)))
(assert (equal (cons (list (cons 0 1)) (list)) (interval 0 1)))
(assert (is-interval (interval 0 1)))
(assert (is-interval (interval +neg-infinity+ +pos-infinity+)))
(assert (equal (cons (list (cons +neg-infinity+ 0)) (list)) (interval +neg-infinity+ 0)))
(assert (is-interval (interval +neg-infinity+ 0)))
(assert (equal (cons (list (cons +pos-infinity+ +pos-infinity+)) (list)) (interval +pos-infinity+)))
(assert (equal (cons (list (cons +pos-infinity+ +pos-infinity+)) (list)) (interval +pos-infinity+ +pos-infinity+)))

(assert (equal +empty-interval+ (interval 1 0)))
(assert (equal +empty-interval+ (interval +pos-infinity+ +neg-infinity+)))
(assert (should-error #'interval 1 'a))
(assert (should-error #'interval 'b 1))
(assert (should-error #'interval 'b 'a))

;!===== asserts for whole interval
(assert (is-interval (whole)))
(assert (equal (cons (list (cons +neg-infinity+ +pos-infinity+)) (list)) (whole)))

;!===== asserts for is-singleton
(assert (is-singleton (interval 0 0)))
(assert (is-singleton (interval 0)))
(assert (not (is-singleton (interval 0 1))))
(assert (not (is-singleton (whole))))
(assert (not (is-singleton +empty-interval+)))
(assert (not (is-singleton (cons (list (cons 0 1) (cons -1 0)) (list)))))
(assert (should-error #'is-singleton 0))

;!===== asserts for is-interval
(assert (is-interval (empty-interval)))
(assert (is-interval (whole)))
(assert (is-interval (interval 0)))
(assert (is-interval (interval 0 1)))
(assert (is-interval (cons (list (cons 0 1) (cons 2 3)) (list))))
(assert (not (is-interval 'a)))
(assert (not (is-interval (cons 2 1))))
(assert (not (is-interval (list (cons 1 1) (cons 2 1)))))

;!===== asserts for inf 
(assert (equal +neg-infinity+ (inf (whole))))
(assert (equal 0 (inf (interval 0))))
(assert (equal +neg-infinity+ (inf (interval +neg-infinity+ 1))))

(assert (should-error #'inf +empty-interval+))
(assert (should-error #'inf 0))

;!===== asserts for sup
(assert (equal +pos-infinity+ (sup (whole))))
(assert (equal 0 (sup (interval 0))))
(assert (equal 1 (sup (interval +neg-infinity+ 1))))

(assert (should-error #'sup +empty-interval+))
(assert (should-error #'sup 0))

;!===== asserts for operator on intervals
;!======= sum asserts
(assert (equal (interval 0) (i+)))

(assert (equal (interval 1) (i+ 1)))
(assert (equal (interval 1) (i+ (interval 1))))
(assert (equal (interval 1 2) (i+ (interval 1 2))))

(assert (equal (interval 2 2) (i+ 1 1)))
(assert (equal (interval 0 0) (i+ 1 -1)))

(assert (equal (interval 2 2) (i+ (interval 1) (interval 1))))
(assert (equal (interval 0 0) (i+ (interval 1) (interval -1))))

(assert (equal (interval 0 2) (i+ (interval 0 1) (interval 0 1))))
(assert (equal (whole) (i+ (interval 0 1) (whole))))

(assert (equal (interval 0 2) (i+ 0 (interval 0 2))))
(assert (equal (interval 0 2) (i+ (interval 0 2) 0)))

(assert (equal (interval 0 2) (i+ 0 (interval 0 2))))
(assert (equal (interval 0 2) (i+ (interval 0 2) 0)))

(assert (equal (interval 3 +pos-infinity+) (i+ (interval 2 3) (interval 1 +pos-infinity+))))

;!======= sub asserts
(assert (equal (interval -1) (i- 1)))
(assert (equal (interval -1) (i- (interval 1))))
(assert (equal (interval -2 -1) (i- (interval 1 2))))
; (assert (equal 0 (+e)))
; (assert (equal 0 (+e)))

(print "All tests passed")