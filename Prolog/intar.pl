%%%% -*- Mode: Prolog -*- 

% Gianfarelli	Giorgio	894499
% Lauria	Luca	900326
% Vanoncini	Davide	903214

plus_e(0)   
plus_e(X, Result)
plus_e(X, Y, Result) 
/*The plus e/1 predicate is true with the unit of the summation operation.
The plus e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
The plus e/3 predicate is true when Result is the extended real sum of X and Y, which must
both be instantiated extended reals. Otherwise the predicate fails.
*/

minus_e(X, Result)
minus_e(X, Y, Result)
/*The minus e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to summation. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
The minus e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/

times_e(1)
times_e(X, Result)
times_e(X, Y, Result)
/*The times e/1 predicate is true with the unit of the summation operation.
The times e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
The times e/3 predicate is true when Result is the extended real multiplication of X and Y,
which must both be instantiated extended reals. Otherwise the predicate fails.
*/

div_e(X, Result)
div_e(X, Y, Result)
/*The div e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to multiplication. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
The div e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/



/*Interval Construction and Other Predicates. The following predicates are the basis for
the interval arithmetic operations.
*/

empty_interval([])
%This predicate is true only of the empty interval [].

interval([])
interval(X, SI)
interval(L, H, I)
/*The predicate interval/1 serves to construct an empty interval.
The predicate interval/2 constructs a singleton interval SI containing only X. X must be
instantiated and be an extended real, otherwise the predicate fails.
The predicate interval/3 constructs an interval I with L as inferior point and H as superior
point. L and H must be instantiated and be extended reals, otherwise the predicate fails. Note
that I can be the empty interval if L > H.
*/

is_interval(I)
/*The predicate is interval/1 is true if I is a term representing an interval (including the empty
interval).
*/

whole_interval(R)
%The predicate whole interval/1 is true if R is a term representing the whole interval R.

is_singleton(S)
%The predicate is singleton/1 is true if S is a term representing a singleton interval.

iinf(I, L)
%The predicate iinf/2 is true if I is a non empty interval and L is its inferior limit.

isup(I, H)
%The predicate isup/2 is true if I is a non empty interval and H is its superior limit.

icontains(I, C)
/*If I is not an interval, or if it is an empty interval, the predicate fails. Otherwise, given the
interval I it will succeed if I contains X. X can be a number or another interval.
*/

ioverlap(I1, I2)
/*The predicate ioverlaps succeeds if the two intervals I1 and I2 “overlap”. The predicate fails
if either I1 or I2 is not an interval.
*/



/*Interval Arithmetic Predicates. The following predicates implement the interval arithmetic
operations.
*/

iplus(ZI)
iplus(X, R)
iplus(X, Y, R)
/*The predicate iplus/1 is true if ZI is a non empty interval.
The predicate iplus/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X.
The predicate iplus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the summation table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

iminus(X, R)
iminus(X, Y, R)
/*The predicate iminus/2 is true if X is an instantiated non empty interval and R unifies with
its reciprocal with respect to the summation operation. If X is an extended real then it is first
transformed into a singleton interval.
The predicate iminus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the subtraction table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

itimes(ZI)
itimes(X, R)
itimes(X, Y, R)
/*The predicate itimes/1 is true if ZI is a non empty interval.
The predicate itimes/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X.
The predicate itimes/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the multiplication table for two non empty intervals. If either
X or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

idiv(X, R)
idiv(X, Y, R)
/*The predicate idiv/2 is true if X is an instantiated non empty interval and R unifies with its
reciprocal with respect to the summation operation. If X is an extended real then it is first
transformed into a singleton interval.
The predicate idiv/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the division table for two non empty intervals. If either X or
Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/