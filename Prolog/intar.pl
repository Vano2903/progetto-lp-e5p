%%%% -*- Mode: Prolog -*- 

% Gianfarelli	Giorgio	894499
% Lauria	Luca	900326
% Vanoncini	Davide	903214



% definizione di reale esteso.

% extended_real(infinity).
extended_real(nil).         

extended_real(pos_infinity).

extended_real(neg_infinity).

extended_real(X) :- 
    number(X).

% logica aritmetica.

% gestione delle variabili libere.
extended_real_sum(X, _, _) :- 
    var(X), 
    !, fail.

extended_real_sum(_, Y, _) :-
    var(Y), 
    !, fail.
% fine gestione delle variabili libere.

extended_real_sum(nil, nil, nil) :- !.

extended_real_sum(pos_infinity, neg_infinity, nil) :- !.

extended_real_sum(neg_infinity, pos_infinity, nil) :- !.

extended_real_sum(pos_infinity, _, pos_infinity):- !.

extended_real_sum(neg_infinity, _, neg_infinity):- !.

extended_real_sum(_, pos_infinity, pos_infinity):- !.

extended_real_sum(_, neg_infinity, neg_infinity):- !.

extended_real_sum(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X + Y, 
    !.

% gestione delle variabili libere.
extended_real_subtraction(X, _, _) :-
    var(X), 
    !, fail.

extended_real_subtraction(_, Y, _) :-
    var(Y), 
    !, fail.
% fine gestione delle variabili libere.

extended_real_subtraction(nil, nil, nil) :- !.

extended_real_subtraction(neg_infinity, neg_infinity, nil) :- !.

extended_real_subtraction(pos_infinity, pos_infinity, nil) :-!.

extended_real_subtraction(pos_infinity, _, pos_infinity):- !.

extended_real_subtraction(neg_infinity, _, neg_infinity):- !.

extended_real_subtraction(_, pos_infinity, neg_infinity):- !.

extended_real_subtraction(_, neg_infinity, pos_infinity):- !.

extended_real_subtraction(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y),
    Result is X - Y, 
    !.

% gestione delle variabili libere.
extended_real_multiplication(X, _, _) :-
    var(X), 
    !, fail.

extended_real_multiplication(_, Y, _) :-
    var(Y), 
    !, fail.
% fine gestione delle variabili libere.

extended_real_multiplication(nil, nil, nil) :- !.

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
    Result = neg_infinity, 
    !.

extended_real_multiplication(pos_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, 
    !.

extended_real_multiplication(neg_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, 
    !.

extended_real_multiplication(neg_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, 
    !.

extended_real_multiplication(X, pos_infinity, Result) :-
    extended_real(X),
    X < 0,
    Result = neg_infinity, 
    !.

extended_real_multiplication(X, pos_infinity, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, 
    !.

extended_real_multiplication(X, neg_infinity, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, 
    !.

extended_real_multiplication(X, neg_infinity, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, 
    !.

extended_real_multiplication(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y), 
    Result is X * Y.

% gestione delle variabili libere.
extended_real_division(X, _, _) :-
    var(X), 
    !, fail.

extended_real_division(_, Y, _) :-
    var(Y), 
    !, fail.
% fine gestione delle variabili libere.

extended_real_division(nil, 0, nil) :- !.

extended_real_division(_, 0, nil) :- !.

extended_real_division(0, _, 0):- !.

extended_real_division(neg_infinity, neg_infinity, nil) :- !.

extended_real_division(neg_infinity, pos_infinity, nil) :- !.

extended_real_division(pos_infinity, pos_infinity, nil) :- !.

extended_real_division(pos_infinity, neg_infinity, nil) :- !.

extended_real_division(pos_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = neg_infinity, 
    !.

extended_real_division(pos_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = pos_infinity, 
    !.

extended_real_division(neg_infinity, X, Result) :-
    extended_real(X),
    X < 0,
    Result = pos_infinity, 
    !.

extended_real_division(neg_infinity, X, Result) :-
    extended_real(X),
    X > 0,
    Result = neg_infinity, 
    !.

extended_real_division(X, pos_infinity, 0) :- 
    extended_real(X), 
    !.

extended_real_division(X, neg_infinity, 0) :- 
    extended_real(X), 
    !.

extended_real_division(X, Y, Result) :-
    extended_real(X),
    extended_real(Y),
    Result is X / Y.

% gestione delle variabili libere.
minus_reciprocal(X, _) :- 
    var(X), 
    !, fail.
% fine gestione delle variabili libere.

minus_reciprocal(nil, nil) :- !. % o fail.

minus_reciprocal(pos_infinity, nil) :- !.

minus_reciprocal(neg_infinity, nil) :- !.

minus_reciprocal(X, Result) :- 
    extended_real(X), 
    Result is - X.

% gestione delle variabili libere.
div_reciprocal(X, _) :-
    var(X), 
    !, fail.
% fine gestione delle variabili libere.

div_reciprocal(nil, nil) :- !. % o fail.

div_reciprocal(pos_infinity, nil) :- !.

div_reciprocal(neg_infinity, nil) :- !.

div_reciprocal(0, nil) :- !.

div_reciprocal(X, Result) :-
    extended_real(X),
    Result is 1 / X.

/* extended_real_min_List(neg_infinity, _, _ , _, Min):- 
    Min = neg_infinity,
    !.
extended_real_min_List(_, neg_infinity, _ , _, Min):- 
    Min = neg_infinity,
    !.
extended_real_min_List(_, _, neg_infinity, _, Min):- 
    Min = neg_infinity,
    !.
extended_real_min_List(_, _, _, neg_infinity, Min):- 
    Min = neg_infinity,
    !.
*/

% dovrei sistemare ma ho sonno ciao
extended_real_min_List([X], X):- !. 
extended_real_min_List([neg_infinity | Xs], Min) :- 
    Min is neg_infinity,
    !.
% extended_real_min_List(_, neg_infinity):- !.
extended_real_min_List([pos_infinity | Xs], Min) :- 
    extended_real_min_List(Xs, MinRest),
    Min is MinRest,
    !.
extended_real_min_List([X | Xs], Min) :- 
    extended_real_min_List(Xs, MinRest),
    Min is min(X, MinRest),
    !.

% fine logica aritmetica.



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
% fine gestione delle variabili libere.

plus_e(X, Result) :- 
    extended_real(X),
    Result = X,
    !.

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
% fine gestione delle variabili libere.

plus_e(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y),
    extended_real_sum(X, Y, Result),
    !.

/* The minus e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to summation. X must be instantiated and must be an extended real. Otherwise the
predicate fails. 
*/
% gestione delle variabili libere.
minus_e(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

minus_e(X, Result) :- 
    extended_real(X),
    minus_reciprocal(X, Result), 
    !.

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
% fine gestione delle variabili libere.

minus_e(X, Y, Result) :- 
   extended_real(X),
    extended_real(Y),
    extended_real_subtraction(X, Y, Result), 
    !.

% The times e/1 predicate is true with the unit of the summation operation.
times_e(1).

/* The times e/2 predicate is true when Result is an extended real that unifies with X. X must be
instantiated and must be an extended real. Otherwise the predicate fails.
*/
% gestione delle variabili libere.
times_e(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

times_e(X, Result):- 
    extended_real(X),
    Result = X,
    !.

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
% fine gestione delle variabili libere.

times_e(X, Y, Result) :- 
    extended_real(X), 
    extended_real(Y),
    extended_real_multiplication(X, Y, Result), 
    !.

/* The div e/2 predicate is true when Result is an extended real that is the reciprocal of X with
respect to multiplication. X must be instantiated and must be an extended real. Otherwise the
predicate fails.
*/
% gestione delle variabili libere.
div_e(X, _) :- 
    var(X),
    !, fail.
% fine gestione delle variabili libere.

div_e(X, Result) :- 
    extended_real(X),
    div_reciprocal(X, Result), 
    !.

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
% fine gestione delle variabili libere.

div_e(X, Y, Result) :-
    extended_real(X),
    extended_real(Y),
    extended_real_division(X, Y, Result),
    !.

% fine predicati aritmetici.



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
interval(neg_infinity, _) :- 
    !, fail.
interval(pos_infinity, _) :- 
    !, fail.
% creazione di un intervallo singleton.
interval(X, SI) :- 
    extended_real(X),
    SI = [X, X],
    !.

% gestione intervalli con nil.
interval(nil, Result) :- 
    Result = nil,
    !.
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
% fine gestione delle variabili libere.

% creazione di un intervallo singleton specificando 3 argomenti.
interval(X, X, SI) :- 
    !, interval(X, SI).

% creazione di un intervallo standard con infinito.
interval(neg_infinity, H, I) :-
    extended_real(H),
    I = [neg_infinity, H],
    !.
interval(L, pos_infinity, I) :-
    extended_real(L),
    I = [L, pos_infinity],
    !.

% creazione di un intervallo vuoto (L > H).
interval(L, neg_infinity, I) :-
    extended_real(L),
    I = [],
    !.
interval(pos_infinity, H, I) :-
    extended_real(H),
    I = [],
    !.
interval(L, H, I) :- 
    extended_real(L),
    extended_real(H),
    L > H,
    I = [],
    !.
% creazione di un intervallo standard con numeri reali.
interval(L, H, I) :- 
    extended_real(L),
    extended_real(H),
    I = [L, H],
    !.

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

is_interval([neg_infinity, neg_infinity]) :- 
    !, fail.

is_interval([pos_infinity, pos_infinity]) :- 
    !, fail.

is_interval([pos_infinity, _]) :- 
    !, fail.

is_interval([_, neg_infinity]) :- 
    !, fail.

is_interval([]) :- !.

is_interval([neg_infinity, pos_infinity]) :- 
    !.

is_interval([neg_infinity, H]) :- 
    extended_real(H),
    !.

is_interval([L, pos_infinity]) :-
    extended_real(L),
    !.

is_interval([L, H]) :- 
    extended_real(L),
    extended_real(H),
    L =< H,
    !.

/* is_numinterval(I) :- 
    is_interval(I),
    iinf(I, L),
    isup(I, H),
    number(L),
    number(H).
*/

% The predicate whole interval/1 is true if R is a term representing the whole interval R.
whole_interval([neg_infinity, pos_infinity]). 

% The predicate is singleton/1 is true if S is a term representing a singleton interval.

/*
is_singleton([X, _]) :- 
    var(X),
    !, fail.

is_singleton([_, X]) :-
    var(X),
    !, fail.
*/

is_singleton([X, X]) :-
    is_interval([X, X]),
    !.

% The predicate iinf/2 is true if I is a non empty interval and L is its inferior limit.
iinf([], _) :- 
    !, fail.

iinf(I, L) :-
    is_interval(I),
    I = [L, _],
    !.

% The predicate isup/2 is true if I is a non empty interval and H is its superior limit.
isup([], _) :- 
    !, fail.

isup(I, H) :- 
    is_interval(I),
    I = [_, H],
    !.

/* If I is not an interval, or if it is an empty interval, the predicate fails. Otherwise, given the
interval I it will succeed if I contains X. X can be a number or another interval.
*/ 
% fail cases
icontains([], _) :- 
    !, fail.

icontains(X, _) :-
    extended_real(X),
    !, fail.

icontains(_, [neg_infinity, neg_infinity]) :- 
    !, fail.

icontains(_, [pos_infinity, pos_infinity]) :- 
    !, fail.

icontains([neg_infinity, neg_infinity], _) :- 
    !, fail.

icontains([pos_infinity, pos_infinity], _) :- 
    !, fail.

% intervalli uguali
icontains(I, I) :-
    is_interval(I), 
    !.

% singleton
icontains(I, X) :-
    extended_real(X),   
    is_singleton(I),    
    iinf(I, X),
    !.

% intervalli con infinito
icontains([neg_infinity, pos_infinity], X) :- 
    extended_real(X), 
    !.

icontains([neg_infinity, pos_infinity], I) :- 
    is_interval(I), 
    !.

% icontains([_, _], [neg_infinity, pos_infinity]) :- 
%    !, fail.

icontains([neg_infinity, H], neg_infinity) :- 
    extended_real(H), 
    !.           

icontains([L, pos_infinity], pos_infinity) :- 
    extended_real(L), 
    !.          

icontains([neg_infinity, H], X) :-
    number(X),
    number(H),      
    X =< H, 
    !.

icontains([neg_infinity, H], I2) :-
    is_interval(I2),
    number(H),  
    isup(I2, H2),
    number(H2),        
    H >= H2, 
    !.

icontains([L, pos_infinity], X) :-
    number(X),
    number(L),      
    X >= L, 
    !.

icontains([L, pos_infinity], I2) :-
    is_interval(I2),
    number(L),
    iinf(I2, L2),
    number(L2),     
    L =< L2, 
    !.

% intervalli finiti
icontains(I, X) :-
    is_interval(I),
    number(X),
    iinf(I, L),
    number(L),
    isup(I, H),
    number(H),
    X >= L,
    X =< H, 
    !.

icontains(I, I2) :-
    is_interval(I),
    is_interval(I2),
    iinf(I, L1),
    number(L1),
    isup(I, H1),
    number(H1),
    iinf(I2, L2),
    number(L2),
    isup(I2, H2),
    number(H2),
    L1 =< L2,
    H1 >= H2, 
    !.

/* The predicate ioverlap succeeds if the two intervals I1 and I2 “overlap”. The predicate fails
if either I1 or I2 is not an interval.
*/ 

%% dal forum (-inf, n1) e (-inf, n2) n1<n2 true

ioverlap(I, I2) :-   % per definizione si overlappano (ha confermato sul forum).
    icontains(I, I2), !.

% fail cases
ioverlap([neg_infinity, neg_infinity], _) :- !, fail.   

ioverlap([pos_infinity, pos_infinity], _) :- !, fail.

ioverlap([pos_infinity, neg_infinity], _) :- !, fail.

ioverlap(_, [neg_infinity, neg_infinity]) :- !, fail.

ioverlap(_, [pos_infinity, pos_infinity]) :- !, fail.

ioverlap(_, [pos_infinity, neg_infinity]) :- !, fail.

% both interval infinity

% whole interval always overlap
ioverlap(I1, [neg_infinity, pos_infinity]) :- is_interval(I1), !.

ioverlap([neg_infinity, pos_infinity], I2) :- is_interval(I2), !.

% I1 finito I2 infinito
ioverlap(I1, [neg_infinity, H2]) :-
    is_interval(I1), 
    iinf(I1, L1),
    L1 =< H2,
    !.

ioverlap(I1, [L2, pos_infinity]) :-
    is_interval(I1), 
    isup(I1, H1),
    H1 >= L2,
    !.

% I1 infinito I2 intervallo finito
ioverlap([neg_infinity, H1], I2) :- 
    is_interval(I2), 
    iinf(I2, L2),
    H1 >= L2,
    !.

ioverlap([L1, pos_infinity], I2) :- 
    is_interval(I2), 
    isup(I2, H2),
    L1 =< H2,
    !.

%% intervalli finiti
ioverlap(I1, I2) :-
    is_interval(I1),
    is_interval(I2),
    iinf(I1, L1),               
    isup(I1, H1),
    iinf(I2, L2),
    isup(I2, H2),
    (L1 =< H2, H1 >= L2), !.

% end of interval construction


/* Interval Arithmetic Predicates. The following predicates implement the interval arithmetic
operations.
*/

% The predicate iplus/1 is true if ZI is a non empty interval.
iplus([]) :- !, fail.

iplus([neg_infinity, neg_infinity]) :- 
    % is_interval([neg_infinity, neg_infinity]),  % non ha senso.
    !, fail.

iplus([pos_infinity, pos_infinity]) :- 
    % is_interval([pos_infinity, pos_infinity]), 
    !, fail.

iplus([pos_infinity, _]) :- 
    % is_interval([pos_infinity, _]), 
    !, fail.

iplus([_, neg_infinity]) :- 
    % is_interval([_, neg_infinity]), 
    !, fail.

iplus([neg_infinity, pos_infinity]).
    % :- is_interval([neg_infinity, pos_infinity]), !. % per definizione

iplus([neg_infinity, H]) :- 
    is_interval([neg_infinity, H]), 
    !.

iplus([L, pos_infinity]) :- 
    is_interval([L, pos_infinity]), 
    !.

iplus(ZI) :- 
    !, is_interval(ZI). % in questo modo se utilizzi una variabile esegue il controllo in modo giusto e da errore.

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
    iplus(X), % verifica non empty interval.
    R = X,
    !.
iplus(X, R) :-                  %%%  ?- iplus(10, [10,X]).  X = 10. da correggere? leggere README
    extended_real(X),
    is_singleton(R),
    iinf(R, L1),
    X = L1, 
    !.

% iplus(_, _) :- fail.

/* 
The predicate iplus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the summation table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

% somma intervalli
iplus([L1, H1], [L2, H2], [Result1, Result2]) :- 
    iplus([L1, H1]), % no [], verifica pure se sono extended real
    iplus([L2, H2]),
    extended_real_sum(L1, L2, Result1),
    extended_real_sum(H1, H2, Result2),
    !.

% somma reale ed intervallo
iplus(X, [L2, H2], [Result1, Result2]) :- 
    extended_real(X),
    iplus([X, X], [L2, H2], [Result1, Result2]),
    !.

iplus([L1, H1], Y, [Result1, Result2]) :- 
    extended_real(Y),
    iplus([L1, H1], [Y, Y], [Result1, Result2]),
    !.

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
    interval(X, SI),  % da rivedere non funziona con nil: iminus(nil, R) da errore.
    iminus(SI, R),
    !.

/*
% gestione intervalli infiniti e nil
iminus([L1, H1], R) :- 
    iplus([L1, H1]), % verifica non empty interval.
    minus_reciprocal(L1, nil),  
    R = nil,
    !.

iminus([L1, H1], R) :- 
    iplus([L1, H1]), % verifica non empty interval.
    minus_reciprocal(H1, nil),  
    R = nil,
    !.
*/

% intervalli finiti
iminus([L1, H1], R) :- 
    iplus([L1, H1]), % verifica non empty interval.
    minus_reciprocal(L1, L2),  
    minus_reciprocal(H1, H2),
    R = [H2, L2],
    iplus(R),
    !.

/* The predicate iminus/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the subtraction table for two non empty intervals. If either X
or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/

% differenza intervalli finiti
iminus([L1, H1], [L2, H2], [Result1, Result2]) :- 
    iplus([L1, H1]), % no [], verifica pure se sono extended real
    iplus([L2, H2]),
    extended_real_subtraction(L1, H2, Result1), % [a-d, b-c]
    extended_real_subtraction(H1, L2, Result2),
    !.

% differenza reale ed intervallo
iminus(X, [L2, H2], [Result1, Result2]) :- 
    extended_real(X),
    iminus([X, X], [L2, H2], [Result1, Result2]),
    !.

iminus([L1, H1], Y, [Result1, Result2]) :- 
    extended_real(Y),
    iminus([L1, H1], [Y, Y], [Result1, Result2]),
    !.

% The predicate itimes/1 is true if ZI is a non empty interval.
itimes([]):- !, fail.
itimes(ZI):- !, is_interval(ZI).
/* The predicate itimes/2 is true if X is an instantiated non empty interval and R unifies with it,
or if X is an instantiated extended real and R is a singleton interval containing only X. */

itimes(X, _) :- 
    var(X),
    !, fail.

itimes([L, H], R) :- 
    itimes([L, H]), % verifica non empty interval.
    L2 is L*2,
    H2 is H*2,
    R = [L2, H2],
    !.
itimes(X, R) :-
    extended_real(X),
    is_singleton(R),
    iinf(R, L1),
    X = L1, 
    !.
/* The predicate itimes/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the multiplication table for two non empty intervals. If either
X or Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/
itimes([L1, H1], [L2, H2], [Result1, Result2]) :- % somma intervalli finiti
    itimes([L1, H1]), % no [], verifica pure se sono extended real
    itimes([L2, H2]),
    extended_real_multiplication(L1, L2, S1),
    extended_real_multiplication(L1, H2, S2),
    extended_real_multiplication(H1, L2, S3),
    extended_real_multiplication(H1, H2, S4),
    % ora dovrei prendere il minimo e il massimo il problema che devo gestire pos_infinity e min_infinity
    % devo creare la funzione
    extended_real_min_List([S1, S2, S3, S4], Min),
    Result1 = Min,
    Result2 = Max,
    !.
itimes(X, [L2, H2], [Result1, Result2]) :- % X in SI
    number(X),
    itimes([X, X], [L2, H2], R),
    !.

itimes([L1, H1], Y, [Result1, Result2]) :- % Y in SI
    number(Y),
    itimes([L1, H1], [Y, Y], [Result1, Result2]),
    !.

/* The predicate idiv/2 is true if X is an instantiated non empty interval and R unifies with its
reciprocal with respect to the division operation. If X is an extended real then it is first
transformed into a singleton interval. */
idiv(X, R). 

/* The predicate idiv/3 is true if X and Y are instantiated non empty intervals and R is the
interval constructed according to the division table for two non empty intervals. If either X or
Y are instantiated extended reals, they are first transformed into singleton intervals.
In all other cases the predicates fail.
*/
idiv(X, Y, R).

%%%% end of file -- intar.pl --