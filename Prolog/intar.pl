%%%% -*- Mode: Prolog -*- 

% Gianfarelli	Giorgio	894499
% Lauria	Luca	900326
% Vanoncini	Davide	903214

% definizione di reale esteso.


/* CODICE MODIFICATO
    1) avendo definito extended_real(X) non ha senso usare number(X)
    2) iinf isup non è richiesto che sia istanziato (infatit il meotdo serve anche a trova il L o H) 
    3)
*/


% extended_real(infinity).
extended_real(nil).         
extended_real(pos_infinity).
extended_real(neg_infinity).
extended_real(X) :- 
    number(X).

% logica aritmetica.
extended_real_sum(pos_infinity, neg_infinity, nil) :- !.
extended_real_sum(neg_infinity, pos_infinity, nil) :- !.
extended_real_sum(pos_infinity, _, pos_infinity):- !.
extended_real_sum(_, pos_infinity, pos_infinity):- !.
extended_real_sum(neg_infinity, _, neg_infinity):- !.
extended_real_sum(_, neg_infinity, neg_infinity):- !.
extended_real_sum(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X + Y, !.
% extended_real_sum(_, _, _) :- 
%    !, fail.

extended_real_subtraction(neg_infinity, neg_infinity, nil) :- !. 
extended_real_subtraction(pos_infinity, pos_infinity, nil) :-!.
extended_real_subtraction(pos_infinity, _, pos_infinity):- !.
extended_real_subtraction(neg_infinity, _, neg_infinity):- !.
extended_real_subtraction(_, pos_infinity, neg_infinity):- !.
extended_real_subtraction(_, neg_infinity, pos_infinity):- !.
extended_real_subtraction(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y),
    Result is X - Y.

extended_real_multiplication(pos_infinity, 0, nil) :- !.
extended_real_multiplication(0, pos_infinity, nil) :- !.
extended_real_multiplication(neg_infinity, 0, nil) :- !.
extended_real_multiplication(0, neg_infinity, nil) :- !.
extended_real_multiplication(pos_infinity, neg_infinity, neg_infinity) :- !.
extended_real_multiplication(neg_infinity, pos_infinity, neg_infinity) :- !.
extended_real_multiplication(pos_infinity, pos_infinity, pos_infinity) :- !.
extended_real_multiplication(neg_infinity, neg_infinity, pos_infinity) :- !.
extended_real_multiplication(pos_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = neg_infinity, !.
extended_real_multiplication(pos_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, !.
extended_real_multiplication(neg_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, !.
extended_real_multiplication(neg_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, !.
extended_real_multiplication(X, pos_infinity, Result) :-
    extended_real(X),
    X < 0,
    Result = neg_infinity, !.
extended_real_multiplication(X, pos_infinity, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, !.
extended_real_multiplication(X, neg_infinity, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, !.
extended_real_multiplication(X, neg_infinity, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, !.
extended_real_multiplication(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X * Y.

extended_real_division(_, 0, nil) :- !.
extended_real_division(0, _, 0):- !.
extended_real_division(neg_infinity, neg_infinity, nil) :- !.
extended_real_division(neg_infinity, pos_infinity, nil) :- !.
extended_real_division(pos_infinity, pos_infinity, nil) :- !.
extended_real_division(pos_infinity, neg_infinity, nil) :- !.
extended_real_division(pos_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = neg_infinity, !.
extended_real_division(pos_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, !.
extended_real_division(neg_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, !.
extended_real_division(neg_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, !.
extended_real_division(X, pos_infinity, 0) :- 
    extended_real(X), !.
extended_real_division(X, neg_infinity, 0) :- 
    extended_real(X), !.
extended_real_division(X, Y, Result) :-
    extended_real(X),
    extended_real(Y),
    Result is X / Y.

minus_reciprocal(pos_infinity, nil) :- !.
minus_reciprocal(neg_infinity, nil) :- !.
minus_reciprocal(X, Result) :- 
    extended_real(X), 
    Result is - X.

div_reciprocal(pos_infinity, nil) :- !.
div_reciprocal(neg_infinity, nil) :- !.
div_reciprocal(0, nil) :- !.
div_reciprocal(X, Result) :-
    extended_real(X),
    Result is 1 / X.

% fine logica aritmetica.



% predicati aritmetici.

/* The plus e/1 predicate is true with the unit of the summation operation.
The plus e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
The plus e/3 predicate is true when Result is the extended real sum of X and Y, which must
both be instantiated extended reals. Otherwise the predicate fails.
*/
plus_e(0).
plus_e(X, Result) :- 
    nonvar(X),
    extended_real(X),
    Result = X,
    extended_real(Result), !. % non servirebbe controllare se Result è un extended real dato che è già controllato per X.
plus_e(_, _) :- 
    fail. 
plus_e(X, Y, Result) :- 
    nonvar(X),
    nonvar(Y),
    extended_real(X), 
    extended_real(Y),
    extended_real_sum(X, Y, Result),
    extended_real(Result), !.
plus_e(_, _, _) :-
    fail.


/* The minus e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to summation. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
The minus e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/
minus_e(X, Result) :- % minus_e(pos_infinity, Result) Result=nil
    nonvar(X),
    extended_real(X),
    minus_reciprocal(X, Result), 
    extended_real(Result), !.
minus_e(_, _) :- 
    fail.
minus_e(X, Y, Result) :- 
    nonvar(X),
    nonvar(Y),
    extended_real(X),
    extended_real(Y),
    extended_real_subtraction(X, Y, Result), 
    extended_real(Result), !.
minus_e(_, _, _) :-
    fail.

/* The times e/1 predicate is true with the unit of the summation operation.
The times e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
The times e/3 predicate is true when Result is the extended real multiplication of X and Y,
which must both be instantiated extended reals. Otherwise the predicate fails.
*/
times_e(1).
times_e(X, Result):- 
    nonvar(X),
    extended_real(X),
    Result = X,
    extended_real(Result), !.
times_e(_, _) :-
    fail.
times_e(X, Y, Result) :- 
    nonvar(X),
    nonvar(Y),
    extended_real(X), 
    extended_real(Y),
    extended_real_multiplication(X, Y, Result), 
    extended_real(Result), !.
times_e(_, _, _) :-
    fail.

/* The div e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to multiplication. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
The div e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/
div_e(X, Result) :- 
    nonvar(X),
    extended_real(X),
    div_reciprocal(X, Result), 
    extended_real(Result), !.
div_e(_, _) :-
    fail.
div_e(X, Y, Result) :-
    nonvar(X),
    nonvar(Y),
    extended_real(X),
    extended_real(Y),
    extended_real_division(X, Y, Result),
    extended_real(Result), !.
div_e(_, _, _) :-
    fail.

% fine predicati aritmetici.



/* Interval Construction and Other Predicates. The following predicates are the basis for
the interval arithmetic operations.
*/

empty_interval([]).
% This predicate is true only of the empty interval [].

interval([]).
interval(neg_infinity, SI) :- 
    var(neg_infinity), %necessario altrimenti ?-interval(L, SI) fa L=neg_infinity SI=[neg_inf, neg_inf]
    SI = [neg_infinity, neg_infinity],
    !.
interval(pos_infinity, SI) :- 
    var(pos_infinity),
    SI = [pos_infinity, pos_infinity],
    !.
interval(X, SI) :- 
    nonvar(X),
    extended_real(X),
    SI = [X, X],
    !.
interval(neg_infinity, H, I) :-
    var(neg_infinity),
    nonvar(H),
    extended_real(H),
    I = [neg_infinity, H],
    !.
interval(L, pos_infinity, I) :-
    nonvar(L),
    var(pos_infinity),
    extended_real(L),
    I = [L, pos_infinity],
    !.
interval(L, neg_infinity, I) :-
    nonvar(L),
    var(neg_infinity),
    extended_real(L),
    I = [],
    !.
interval(pos_infinity, H, I) :-
    var(pos_infinity),
    nonvar(H),
    extended_real(H),
    I = [],
    !.
interval(L, H, I) :-  %interval(L, 5, I) dovrebbe fail 
    nonvar(L),
    nonvar(H),
    extended_real(L),
    extended_real(H),
    (   L =< H -> 
        I = [L, H] % Intervallo valido se L <= H
    ;   I = [] % Intervallo vuoto se L > H  
    ),
    !.
interval(_, _, _) :-
    fail.
/* The predicate interval/1 serves to construct an empty interval.
The predicate interval/2 constructs a singleton interval SI containing only X. X must be
instantiated and be an extended real, otherwise the predicate fails.
The predicate interval/3 constructs an interval I with L as inferior point and H as superior
point. L and H must be instantiated and be extended reals, otherwise the predicate fails. Note
that I can be the empty interval if L > H.
*/

is_interval([X, _]) :- 
    var(X),
    !, fail.
is_interval([_, Y]) :-          % in questo modo se X/Y è una variabile fallisce.
    var(Y),
    !, fail.
is_interval([]) :- !.
is_interval([neg_infinity, neg_infinity]) :- !.
is_interval([pos_infinity, pos_infinity]) :- !.
is_interval([neg_infinity, pos_infinity]) :- !.
is_interval([neg_infinity, H]) :- 
    extended_real(H),
    % extended_real(H),   % un po implicita come cosa, secondo me si può togliere.
    !.
is_interval([L, pos_infinity]) :-
    extended_real(L),
    % extended_real(L),
    !.

is_interval([L, H]) :- 
    extended_real(L),
    extended_real(H),
    % extended_real(L),    
    % extended_real(H),
    L =< H,
    !.
/* The predicate is interval/1 is true if I is a term representing an interval (including the empty
interval).
*/

whole_interval([neg_infinity, pos_infinity]).  %%% in questo modo inserendo una variabile unifica con [neg_infinity, pos_infinity]... non so se è corretto.
% The predicate whole interval/1 is true if R is a term representing the whole interval R.

is_singleton([X, X]) :-
    extended_real(X), !.
% The predicate is singleton/1 is true if S is a term representing a singleton interval.

iinf([], _) :- 
    !, fail.
iinf(I, L) :-
    is_interval(I),
    I = [L, _],
    !.
% The predicate iinf/2 is true if I is a non empty interval and L is its inferior limit.

isup([], _) :- 
    !, fail.
isup(I, H) :- 
    is_interval(I),
    I = [_, H],
    !.
% The predicate isup/2 is true if I is a non empty interval and H is its superior limit.
icontains([], _) :- 
    !, fail. 
icontains([neg_infinity, neg_infinity], _) :- !, fail. %?- icontains([neg_infinity, neg_infinity], H).    false.
icontains([pos_infinity, pos_infinity], _) :- !, fail.
icontains([neg_infinity, pos_infinity], X) :- extended_real(X),!.
icontains([neg_infinity, H], X) :-  
    extended_real(H), 
    ( extended_real(X) ->
        X =< H
    ; is_interval(X) ->    
        iinf(X, L2),
        H >= L2
),
!.

icontains([L, pos_infinity], X) :-  
    extended_real(L), 
    ( extended_real(X) ->
        X >= L
    ; is_interval(X) ->    
        isup(X, H2),
        L =< H2
),
!.
icontains([L1, H1], X) :-  
    is_interval([L1, H1]),
    ( extended_real(X) ->
        X >= L1,
        X =< H1
    ; is_interval(X) ->
        
        iinf(X, L2), 
        isup(X, H2),
        L1 =< L2,
        H1 >= L2,
        H1 >= H2
),
!.
%% TODO: verificare la parte tra 2 intervalli con i segni infiniti
/*
icontains([1, pos_infinity], [2, 4]).  ok
icontains([neg_infinity, 5], [2, 4]).  ok
icontains([neg_infinity, pos_infinity], [0, 1]).
icontains([1, pos_infinity], [3, 5]). ok
true


?- icontains([neg_infinity, 0], [1, 2]).
?- icontains([neg_infinity, pos_infinity], [-1, 1]).
*/

/* If I is not an interval, or if it is an empty interval, the predicate fails. Otherwise, given the
interval I it will succeed if I contains X. X can be a number or another interval.
*/

%% dal forum (-inf, n1) e (-inf, n2) n1<n2 

ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    iinf(I1, L1),               % fatto da copilot, non ancora verificato o ragionato.
    isup(I1, H1),
    iinf(I2, L2),
    isup(I2, H2),
    (L1 =< H2, H1 >= L2), !.
/* The predicate ioverlap succeeds if the two intervals I1 and I2 “overlap”. The predicate fails
if either I1 or I2 is not an interval.
*/

% end of interval construction


/* Interval Arithmetic Predicates. The following predicates implement the interval arithmetic
operations.
*/

iplus(ZI) :- empty_interval(ZI), !, fail.
iplus(X, R).
iplus(X, Y, R).
/* The predicate iplus/1 is true if ZI is a non empty interval.
The predicate iplus/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X.
The predicate iplus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the summation table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

iminus(X, R).
iminus(X, Y, R).
/* The predicate iminus/2 is true if X is an instantiated non empty interval and R unifies with
its reciprocal with respect to the summation operation. If X is an extended real then it is first
transformed into a singleton interval.
The predicate iminus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the subtraction table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

itimes(ZI).
itimes(X, R).
itimes(X, Y, R).
/* The predicate itimes/1 is true if ZI is a non empty interval.
The predicate itimes/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X.
The predicate itimes/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the multiplication table for two non empty intervals. If either
X or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

idiv(X, R). % summation?
idiv(X, Y, R).
/* The predicate idiv/2 is true if X is an instantiated non empty interval and R unifies with its
reciprocal with respect to the ''''''summation''''''' operation. If X is an extended real then it is first
transformed into a singleton interval.
The predicate idiv/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the division table for two non empty intervals. If either X or
Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/