%%%% -*- Mode: Prolog -*- 

% Gianfarelli	Giorgio	894499
% Lauria	Luca	900326
% Vanoncini	Davide	903214



% definizione di reale esteso.
extended_real(pos_infinity) :- !.

extended_real(neg_infinity) :- !.

extended_real(X) :- 
    number(X).

% predicati aritmetici.

% The plus e/1 predicate is true with the unit of the summation operation.
plus_e(0).

/* The plus e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails. 
*/
% gestione delle variabili libere.
plus_e(X, _) :- 
    var(X),
    !, fail.

plus_e(X, Result) :- 
    extended_real(X),
    Result = X.

/* The plus e/3 predicate is true when Result is the extended real sum of X and Y, which must
both be instantiated extended reals. Otherwise the predicate fails.
*/
% gestione delle variabili libere.
plus_e(X, _, _) :- 
    var(X), 
    !, fail.

plus_e(_, Y, _) :-
    var(Y), 
    !, fail.

plus_e(pos_infinity, neg_infinity, _) :- 
    !, fail.

plus_e(neg_infinity, pos_infinity, _) :- 
    !, fail.

plus_e(pos_infinity, _, pos_infinity):- !.

plus_e(neg_infinity, _, neg_infinity):- !.

plus_e(_, pos_infinity, pos_infinity):- !.

plus_e(_, neg_infinity, neg_infinity):- !.

plus_e(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X + Y.

/* The minus e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to summation. X must be instantiated and must be an extended real. Otherwise the
predicate fails. 
*/
% gestione delle variabili libere.
minus_e(X, _) :- 
    var(X), 
    !, fail.

minus_e(pos_infinity, _) :- 
    !, fail.

minus_e(neg_infinity, _) :-
    !, fail.

minus_e(X, Result) :- 
    extended_real(X), 
    Result is - X.


/* The minus e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/
% gestione delle variabili libere.
minus_e(X, _, _) :-
    var(X), 
    !, fail.

minus_e(_, Y, _) :-
    var(Y), 
    !, fail.

minus_e(neg_infinity, neg_infinity, _) :- 
    !, fail.

minus_e(pos_infinity, pos_infinity, _) :-
    !, fail.

minus_e(pos_infinity, _, pos_infinity):- !.

minus_e(neg_infinity, _, neg_infinity):- !.

minus_e(_, pos_infinity, neg_infinity):- !.

minus_e(_, neg_infinity, pos_infinity):- !.

minus_e(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y),
    Result is X - Y.

% The times e/1 predicate is true with the unit of the summation operation.
times_e(1).

/* The times e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
*/
times_e(X, Y) :- 
    plus_e(X, Y).

/* The times e/3 predicate is true when Result is the extended real multiplication of X and Y,
which must both be instantiated extended reals. Otherwise the predicate fails.
*/
% gestione delle variabili libere.
times_e(X, _, _) :-
    var(X), 
    !, fail.

times_e(_, Y, _) :-
    var(Y), 
    !, fail.

times_e(pos_infinity, 0, _) :- 
    !, fail.

times_e(0, pos_infinity, _) :-
    !, fail.

times_e(neg_infinity, 0, _) :- 
    !, fail.

times_e(0, neg_infinity, _) :- 
    !, fail.

times_e(pos_infinity, neg_infinity, neg_infinity) :- !.

times_e(neg_infinity, pos_infinity, neg_infinity) :- !.

times_e(pos_infinity, pos_infinity, pos_infinity) :- !.

times_e(neg_infinity, neg_infinity, pos_infinity) :- !.

times_e(pos_infinity, X, neg_infinity) :-
    er_min(X, 0, X),
    !.

times_e(pos_infinity, X, pos_infinity) :-
    er_max(X, 0, X),
    !.

times_e(neg_infinity, X, pos_infinity) :-
    er_min(X, 0, X),
    !.

times_e(neg_infinity, X, neg_infinity) :-
    er_max(X, 0, X),
    !.

times_e(X, pos_infinity, neg_infinity) :-
    er_min(X, 0, X),
    !.

times_e(X, pos_infinity, pos_infinity) :-
    er_max(X, 0, X),
    !.

times_e(X, neg_infinity, pos_infinity) :-
    er_min(X, 0, X),
    !.

times_e(X, neg_infinity, neg_infinity) :-
    er_max(X, 0, X),
    !.

times_e(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X * Y.

/* The div e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to multiplication. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
*/
% gestione delle variabili libere.
div_e(X, _) :-
    var(X), 
    !, fail.

div_e(pos_infinity, _) :- 
    !, fail.

div_e(neg_infinity, _) :- 
    !, fail.

div_e(0, _) :- 
    !, fail.

div_e(X, Result) :-
    extended_real(X),
    Result is 1 / X.

/* The div e/3 predicate is true when Result is the extended real subtraction of Y from X, which
must both be instantiated extended reals. Otherwise the predicate fails.
*/
% gestione delle variabili libere.
div_e(X, _, _) :-
    var(X), 
    !, fail.

div_e(_, Y, _) :-
    var(Y), 
    !, fail.

div_e(_, 0, _) :- 
    !, fail.

div_e(0, _, 0):- !.

div_e(neg_infinity, neg_infinity, _) :- 
    !, fail.

div_e(neg_infinity, pos_infinity, _) :- 
    !, fail.

div_e(pos_infinity, pos_infinity, _) :-
    !, fail.

div_e(pos_infinity, neg_infinity, _) :- 
    !, fail.

div_e(pos_infinity, X, neg_infinity) :-
    er_min(X, 0, X),
    !.

div_e(pos_infinity, X, pos_infinity) :-
    er_max(X, 0, X),
    !.

div_e(neg_infinity, X, pos_infinity) :-
    er_min(X, 0, X),
    !.

div_e(neg_infinity, X, neg_infinity) :-
    er_max(X, 0, X),
    !.

div_e(X, pos_infinity, 0) :- 
    extended_real(X), 
    !.

div_e(X, neg_infinity, 0) :- 
    extended_real(X), 
    !.

div_e(X, Y, Result) :-
    extended_real(X),
    extended_real(Y),
    Result is X / Y.

% fine predicati aritmetici.

%logica estremo intervallo disgiunto
% Caso base: 
extract_estremo(_, [], []). 
%1=L, 2=H
extract_estremo(1, [[L, _] | T], [L | R]) :-
    extract_estremo(1, T, R).
extract_estremo(2, [[_, H] | T], [H | R]) :-
    extract_estremo(2, T, R).

% logica intervallare.
% gestione intervallo vuoto.
extended_real_min_list([], _) :- 
    !, fail.

% gestione delle variabili libere.
extended_real_min_list([X], _) :-
    var(X),
    !, fail.

extended_real_min_list([X | _], _) :-
    var(X),
    !, fail.

% Caso base.
extended_real_min_list([X], X) :-
    extended_real(X).

% Caso ricorsivo con pos_infinity.
extended_real_min_list([pos_infinity | Xs], Min) :-
    extended_real_min_list(Xs, Min), !.

% Caso ricorsivo.
extended_real_min_list([X | Xs], Min) :-
    extended_real(X),
    extended_real_min_list(Xs, MinXs),
    er_min(X, MinXs, Min), 
    !.


% calcolo del minimo.
er_min(X, X, X) :- 
    extended_real(X), !.

er_min(X, pos_infinity, X) :- 
    number(X), !.

er_min(pos_infinity, X, X) :- 
    number(X), !.

er_min(X, neg_infinity, neg_infinity) :-  
    extended_real(X), !.

er_min(neg_infinity, X, neg_infinity) :- 
    extended_real(X), !.

er_min(X, MinXs, X) :- 
    number(X), 
    number(MinXs),
    X =< MinXs,
    !.

er_min(X, MinXs, MinXs) :- 
    number(X), 
    number(MinXs),
    X > MinXs,
    !.

% gestione intervallo vuoto.
extended_real_max_list([], _) :- 
    !, fail.

% gestione delle variabili libere.
extended_real_max_list([X], _) :-
    var(X),
    !, fail.

extended_real_max_list([X | _], _) :-
    var(X),
    !, fail.

% Caso base.
extended_real_max_list([X], X) :-
    extended_real(X).

% Caso ricorsivo con neg_infinity.
extended_real_max_list([neg_infinity | Xs], Max) :-
    extended_real_max_list(Xs, Max), !.

% Caso ricorsivo.
extended_real_max_list([X | Xs], Max) :-
    extended_real(X),
    extended_real_max_list(Xs, MaxXs),
    er_max(X, MaxXs, Max), 
    !.

% calcolo del massimo.
er_max(X, X, X) :- 
    extended_real(X), !.

er_max(X, neg_infinity, X) :- 
    number(X), !.  

er_max(neg_infinity, X, X) :-
    number(X), !.

er_max(X, pos_infinity, pos_infinity) :- 
    extended_real(X), !.

er_max(pos_infinity, X, pos_infinity) :- 
    extended_real(X), !.

er_max(X, MaxXs, X) :- 
    number(X),
    number(MaxXs), 
    X >= MaxXs,
    !.

er_max(X, MaxXs, MaxXs) :- 
    number(X),
    number(MaxXs), 
    X < MaxXs,
    !.

% fine logica intervallare.




/* Interval Construction and Other Predicates. The following predicates are the basis for
the interval arithmetic operations.
*/

% This predicate is true only of the empty interval [].
empty_interval([]).

% The predicate interval/1 serves to construct an empty interval.
interval([]).

/* The predicate interval/2 constructs a singleton interval SI containing only X. X must be
instantiated and be an extended real, otherwise the predicate fails.
*/
% gestione delle variabili libere.
interval(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

% gestione dell'infinito per i singleton.
/*interval(neg_infinity, _) :- 
    !, fail.
interval(pos_infinity, _) :- 
    !, fail.*/

% creazione di un intervallo singleton.
interval(X, SI) :- 
    extended_real(X),
    !,
    SI = [X, X].

/* The predicate interval/3 constructs an interval I with L as inferior point and H as superior
point. L and H must be instantiated and be extended reals, otherwise the predicate fails. Note
that I can be the empty interval if L > H.
*/ 
% gestione delle variabili libere.
interval(L, _, _) :-
    var(L),
    !, fail. 

interval(_, H, _) :-
    var(H),
    !, fail.

% creazione di un intervallo singleton specificando 3 argomenti.
interval(X, X, SI) :- 
    interval(X, SI), !.

% creazione di un intervallo standard con infinito.
interval(neg_infinity, H, I) :-
    extended_real(H),
    !,
    I = [neg_infinity, H].

interval(L, pos_infinity, I) :-
    extended_real(L),
    !,
    I = [L, pos_infinity].

% creazione di un intervallo vuoto (L > H).
interval(L, neg_infinity, I) :-
    extended_real(L),
    !,
    I = [].

interval(pos_infinity, H, I) :-
    extended_real(H),
    !,
    I = [].

interval(L, H, I) :- 
    extended_real(L),
    extended_real(H),
    L > H,
    !,
    I = [].

% creazione di un intervallo standard con numeri reali.
interval(L, H, I) :- 
    extended_real(L),
    extended_real(H),
    I = [L, H].

/* The predicate is interval/1 is true if I is a term representing an interval (including the empty
interval).
*/

% gestione delle variabili libere.
is_interval([X, _]) :- 
    var(X),
    !, fail.

is_interval([_, Y]) :-
    var(Y),
    !, fail.
% fine gestione delle variabili libere.

is_interval([neg_infinity, neg_infinity]) :- !.

is_interval([pos_infinity, pos_infinity]) :- !.

is_interval([pos_infinity, _]) :- 
    !, fail.

is_interval([_, neg_infinity]) :- 
    !, fail.

is_interval([]) :- !.

is_interval([L, H]) :- 
    extended_real(L),
    extended_real(H),
    er_min(L, H, L),
    !.

%intervalli disgiunti
is_interval([I1 | I]) :-  
    is_interval(I1), 
    is_interval(I),
    !.

% The predicate whole interval/1 is true if R is a term representing the whole interval R.
whole_interval([neg_infinity, pos_infinity]). 

% The predicate is singleton/1 is true if S is a term representing a singleton interval.
is_singleton([X, X]) :-
    is_interval([X, X]),
    !.

% The predicate iinf/2 is true if I is a non empty interval and L is its inferior limit.
iinf([], _) :- 
    !, fail.
iinf([I1 |Is], X) :- 
    is_interval([I1 | Is]),
    extract_estremo(1, [I1 | Is], LList),
    extended_real_min_list(LList, X),
    !.

iinf(I, L) :-
    is_interval(I),
    I = [L, _].

% The predicate isup/2 is true if I is a non empty interval and H is its superior limit.
isup([], _) :- 
    !, fail.

% gestione intervalli disgiunti.
isup([I1|Is], X) :- 
    is_interval([I1 | Is]),
    extract_estremo(2, [I1 | Is], HList),
    extended_real_max_list(HList, X),
    !.

isup(I, H) :- 
    is_interval(I),
    I = [_, H].

/* If I is not an interval, or if it is an empty interval, the predicate fails. Otherwise, given the
interval I it will succeed if I contains X. X can be a number or another interval.
*/ 
% fail cases
icontains([], _) :- 
    !, fail.

icontains(X, _) :-
    extended_real(X),
    !, fail.

icontains(_, X) :- 
    var(X), 
    !, fail.

% gestione intervalli disgiunti.
icontains([I1, I2], X) :- 
    is_interval([I1, I2]),
    icontains(I1, X),
    !.
                                % non gestisce se entrambi disgiunti
icontains([I1, I2], X) :- 
    is_interval([I1, I2]),
    icontains(I2, X),
    !.

% reale esteso
icontains([L, H], X) :- 
    is_interval([L, H]),
    extended_real(X),
    !,
    er_min(L, X, L),
    er_max(H, X, H).

% intervallo
icontains([L1, H1], [L2, H2]) :- 
    is_interval([L1, H1]),
    is_interval([L2, H2]),
    er_min(L1, L2, L1),
    er_max(H1, H2, H1).

/* The predicate ioverlap succeeds if the two intervals I1 and I2 “overlap”. The predicate fails
if either I1 or I2 is not an interval.
*/
% gestione intervalli disgiunti. 
ioverlap([I1, I2], X) :- 
    is_interval([I1, I2]),
    ioverlap(I1, X),
    !.
                                % manca da gestire il caso in cui entrambi gli int sono disgiunti.
                                % ?- ioverlap([[-1,0], [1,5]], [[neg_infinity,2], [2,4]]). 
                                % false. invece di true.
ioverlap([I1, I2], X) :-
    is_interval([I1, I2]),
    ioverlap(I2, X),
    !.

ioverlap([L1, H1], [L2, H2]) :- 
    is_interval([L1, H1]),
    is_interval([L2, H2]),
    er_min(L1, H2, L1),
    er_max(H1, L2, H1).

/* Interval Arithmetic Predicates. The following predicates implement the interval arithmetic
operations.
*/

% The predicate iplus/1 is true if ZI is a non empty interval.
iplus([]) :- !, fail.

iplus(ZI) :- !, is_interval(ZI).
/*
The predicate iplus/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X.
*/

% gestione delle variabili libere.
iplus(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

iplus(X, R) :- 
    iplus(X),
    !,
    R = X.

iplus(X, R) :-                  
    extended_real(X),
    is_singleton(R),
    !,
    iinf(R, L1),
    X = L1.

/* 
The predicate iplus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the summation table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

% somma intervalli
iplus([L1, H1], [L2, H2], [Result1, Result2]) :- 
    iplus([L1, H1]), 
    iplus([L2, H2]),
    !,
    plus_e(L1, L2, Result1),
    plus_e(H1, H2, Result2).
% somma reale ed intervallo
iplus(X, [L2, H2], [Result1, Result2]) :- 
    interval(X, SI),
    iplus(SI, [L2, H2], [Result1, Result2]).

iplus([L1, H1], Y, [Result1, Result2]) :- 
    interval(Y, SI),
    iplus([L1, H1], SI, [Result1, Result2]).

% somma reale reale
iplus(X, Y, Result) :- 
    interval(X, SI1),
    interval(Y, SI2),
    iplus(SI1, SI2, Result).


/* The predicate iminus/2 is true if X is an instantiated non empty interval and R unifies with
its reciprocal with respect to the summation operation. If X is an extended real then it is first
transformed into a singleton interval. */

% gestione delle variabili libere.
iminus(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

% extended real
iminus(X, R) :- 
    extended_real(X),
    interval(X, SI),  
    !,
    iminus(SI, R).

% intervalli finiti
iminus([L1, H1], R) :- 
    iplus([L1, H1]), 
    minus_e(L1, L2),  
    minus_e(H1, H2),
    R = [H2, L2],
    iplus(R).

/* The predicate iminus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the subtraction table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

% differenza intervalli finiti
iminus([L1, H1], [L2, H2], [Result1, Result2]) :- 
    iplus([L1, H1]), 
    iplus([L2, H2]),
    !,
    minus_e(L1, H2, Result1), 
    minus_e(H1, L2, Result2).

% X or Y extended real
iminus(X, [L2, H2], [Result1, Result2]) :- 
    interval(X, SI),
    iminus(SI, [L2, H2], [Result1, Result2]).

iminus([L1, H1], Y, [Result1, Result2]) :- 
    interval(Y, SI),
    iminus([L1, H1], SI, [Result1, Result2]).

% X and Y extended real
iminus(X, Y, Result) :- 
    interval(X, SI1),
    interval(Y, SI2),
    iminus(SI1, SI2, Result).

% The predicate itimes/1 is true if ZI is a non empty interval.
itimes([]):- !, fail.
itimes(ZI):- !, is_interval(ZI).

/* The predicate itimes/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X. */
itimes(X, _) :- 
    var(X),
    !, fail.

itimes(X, R) :- 
    itimes(X),
    !,
    R = X.

itimes(X, R) :-
    extended_real(X),
    is_singleton(R),
    !,
    iinf(R, L1),
    X = L1.

/* The predicate itimes/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the multiplication table for two non empty intervals. If either
X or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/
itimes([L1, H1], [L2, H2], [Result1, Result2]) :- 
    itimes([L1, H1]), 
    itimes([L2, H2]),
    !,
    times_e(L1, L2, S1),
    times_e(L1, H2, S2),
    times_e(H1, L2, S3),
    times_e(H1, H2, S4),
    extended_real_min_list([S1, S2, S3, S4], Min),
    extended_real_max_list([S1, S2, S3, S4], Max),
    Result1 = Min,
    Result2 = Max.


% X or Y extended real
itimes(X, [L2, H2], [Result1, Result2]) :- 
    interval(X, SI),
    itimes(SI, [L2, H2], [Result1, Result2]).

itimes([L1, H1], Y, [Result1, Result2]) :- 
    interval(Y, SI),
    itimes([L1, H1], SI, [Result1, Result2]).

% X and Y extended real
itimes(X, Y, Result) :- 
    interval(X, SI1),
    interval(Y, SI2),
    itimes(SI1, SI2, Result).

/* The predicate idiv/2 is true if X is an instantiated non empty interval and R unifies with its
reciprocal with respect to the division operation. If X is an extended real then it is first
transformed into a singleton interval. */

% gestione delle variabili libere
idiv(X, _) :- 
    var(X),
    !, fail.

% extended_real
idiv(X, R) :- 
    extended_real(X),
    !,
    interval(X, SI),  
    idiv(SI, R).

% intervalli finiti
idiv([L1, H1], R) :- 
    itimes([L1, H1]), 
    !,
    div_e(L1, L2),  
    div_e(H1, H2),
    R = [H2, L2], % controllare 
    iplus(R).

% esempio [-2, 4] da false perche l'intervallo contiene 0 che non è invertibile
% bisogna ritornare nil??
idiv(_, _):- 
    !, fail.

/* The predicate idiv/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the division table for two non empty intervals. If either X or
Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

% gestione delle variabili libere.
idiv(X, _, _) :- 
    var(X),
    !, fail.

idiv(_, Y, _) :-
    var(Y),
    !, fail.

% gestione intervalli Z.
idiv(_, [0, 0], _) :- 
    !, fail.

idiv([0, 0], _, [0, 0]) :- 
    !.

% gestione eccezione C = 0, P1/P e P0/P.
idiv([A, B], [0, D], [Result1, Result2]) :- 
    er_max(A, 0, A),
    itimes([A, B]), 
    itimes([0, D]),
    !,
    div_e(A, D, S1),
    Result1 = S1,
    Result2 = pos_infinity.

% gestione eccezione C = 0, N1/P e N0/P.
idiv([A, B], [0, D], [Result1, Result2]) :- 
    er_min(B, 0, B),
    itimes([A, B]), 
    itimes([0, D]),
    !,
    div_e(B, D, S1),
    Result1 = neg_infinity,
    Result2 = S1.

% gestione eccezione C = 0, M/P.
idiv([A, B], [0, D], [Result1, Result2]) :- 
    er_min(A, 0, A),
    er_max(B, 0, B),
    itimes([A, B]), 
    itimes([0, D]),
    !,
    Result1 = neg_infinity,
    Result2 = pos_infinity.

% gestione eccezione D = 0, P1/N e P0/N.
idiv([A, B], [C, 0], [Result1, Result2]) :- 
    er_max(A, 0, A),
    itimes([A, B]), 
    itimes([C, 0]),
    !,
    div_e(A, C, S1),
    Result1 = neg_infinity,
    Result2 = S1.

% gestione eccezione D = 0, N1/N e N0/N.
idiv([A, B], [C, 0], [Result1, Result2]) :- 
    er_min(B, 0, B),
    itimes([A, B]), 
    itimes([C, 0]),
    !,
    div_e(B, C, S1),
    Result1 = S1,
    Result2 = pos_infinity.

% gestione eccezione D = 0, M/N.
idiv([A, B], [C, 0], [Result1, Result2]) :- 
    er_min(A, 0, A),
    er_max(B, 0, B),
    itimes([A, B]), 
    itimes([C, 0]),
    !,
    Result1 = neg_infinity,
    Result2 = pos_infinity.



% gestione Id = M -> c<0 d>0

% In = [-X, pos_infinity], Id = [Neg_infinity, Y]
idiv([A, pos_infinity], [neg_infinity, D], [Result1, Result2]) :- 
    er_min(A, 0, A),
    er_min(D, 0, D),
    itimes([A, pos_infinity]), 
    itimes([neg_infinity, D]),
    !,
    div_e(pos_infinity, D, S1),
    div_e(A, D, S2),
    Result1 = S1,
    Result2 = S2.

% In = [-X, pos_infinity], Id = [Y, pos_infinity]
idiv([A, pos_infinity], [C, pos_infinity], [Result1, Result2]) :- 
    er_min(A, 0, A),
    er_max(C, 0, C),
    itimes([A, pos_infinity]), 
    itimes([C, pos_infinity]),
    !,
    div_e(A, C, S1),
    div_e(pos_infinity, C, S2),
    Result1 = S1,
    Result2 = S2.

% In = p0
idiv([0, B], [C, D], [Result1, Result2]) :- 
    er_min(C, 0, C),                 
    er_max(D, 0, D),
    itimes([0, B]),                          
    itimes([C, D]),   
    !,             
    Result1 = neg_infinity,
    Result2 = pos_infinity.

% In = n0
idiv([A, 0], [C, D], [Result1, Result2]) :- 
    er_min(C, 0, C),
    er_max(D, 0, D),
    itimes([A, 0]),                          
    itimes([C, D]),
    !,
    Result1 = neg_infinity,
    Result2 = pos_infinity.

% In = M
idiv([A, B], [C, D], [Result1, Result2]) :- 
    er_min(C, 0, C),
    er_max(D, 0, D),
    er_min(A, 0, A),
    er_max(B, 0, B),
    itimes([A, B]),                          
    itimes([C, D]),
    !,
    Result1 = neg_infinity,
    Result2 = pos_infinity.

% In = P1
idiv([A, B], [C, D], [I1, I2]) :- 
    er_min(C, 0, C),
    er_max(D, 0, D),
    er_max(A, 0, A),
    itimes([A, B]),                          
    itimes([C, D]),
    !,
    idiv([A, B], [C, 0], I1),
    idiv([A, B], [0, D], I2).

% In = N1
idiv([A, B], [C, D], [I1, I2]) :- 
    er_min(C, 0, C),
    er_max(D, 0, D),
    er_min(B, 0, B),
    itimes([A, B]),                          
    itimes([C, D]),
    !,
    idiv([A, B], [0, D], I1),
    idiv([A, B], [C, 0], I2).

% Intervalli infiniti (ID = P)

% In = P
idiv([A, pos_infinity], [C, pos_infinity], [Result1, Result2]) :- 
    er_max(A, 0, A),
    er_max(C, 0, C),
    !,
    div_e(A, pos_infinity, S1),
    div_e(pos_infinity, C, S3),
    Result1 = S1,
    Result2 = S3.

% In = M
idiv([neg_infinity, B], [C, pos_infinity], [Result1, Result2]) :- 
    er_max(B, 0, B),
    er_max(C, 0, C),
    !,
    div_e(neg_infinity, C, S1),
    div_e(B, C, S3),
    Result1 = S1,
    Result2 = S3.

% In = N
idiv([neg_infinity, B], [C, pos_infinity], [Result1, Result2]) :- 
    er_min(B, 0, B),
    er_max(C, 0, C),
    !,
    div_e(neg_infinity, C, S1),
    div_e(B, pos_infinity, S4),
    Result1 = S1,
    Result2 = S4.

% Intervalli infiniti (ID = N)
% In = P
idiv([A, pos_infinity], [neg_infinity, D], [Result1, Result2]) :- 
    er_max(A, 0, A),
    er_min(D, 0, D),
    !,
    div_e(pos_infinity, D, S1),
    div_e(A, neg_infinity, S3),
    Result1 = S1,
    Result2 = S3.

% In = N
idiv([neg_infinity, B], [neg_infinity, D], [Result1, Result2]) :- 
    er_min(B, 0, B),
    er_min(D, 0, D),
    !,
    div_e(B, neg_infinity, S1),
    div_e(neg_infinity, D, S4),
    Result1 = S1,
    Result2 = S4.

% In = M
idiv([neg_infinity, B], [neg_infinity, D], [Result1, Result2]) :- 
    er_max(B, 0, B),
    er_min(D, 0, D),
    !,
    div_e(B, D, S1),
    div_e(neg_infinity, D, S3), % qui fa fail
    Result1 = S1,
    Result2 = S3.

% caso "base".
idiv([A, B], [C, D], [Result1, Result2]) :- 
    itimes([A, B]),                          
    itimes([C, D]),
    div_e(A, C, S1),
    div_e(A, D, S2),
    div_e(B, C, S3),
    div_e(B, D, S4),
    extended_real_min_list([S1, S2, S3, S4], Min),
    extended_real_max_list([S1, S2, S3, S4], Max),
    Result1 = Min,
    Result2 = Max,
    !.

% X or Y extended real
idiv(X, [C, D], [Result1, Result2]) :- 
    interval(X, SI),
    !,
    idiv(SI, [C, D], [Result1, Result2]).

idiv([A, B], Y, [Result1, Result2]) :- 
    interval(Y, SI),
    !,
    idiv([A, B], SI, [Result1, Result2]).
% X and Y extended real
idiv(X, Y, Result) :- 
    interval(X, SI1),
    interval(Y, SI2),
    !,
    idiv(SI1, SI2, Result).

% idiv(_, _ , _) :- !, fail.
%%%% end of file -- intar.pl --