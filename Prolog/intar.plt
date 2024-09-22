:- begin_tests(intar).
:- include(intar).
:- use_module(library(random)).


%%% per ogni predicato bisogna controllare TUTTI i casi possibili:
%%% - successo
%%% - fallimento
%%% - variabili istanziate
%%% - variabili non istanziate

% exteneded_real
test(er_fail, fail) :-
    extended_real(a).
test(er_check) :-
    extended_real(neg_infinity),
    extended_real(pos_infinity),
    random(A),
    extended_real(A).

% plus_e/2
test('plus_e_fail/2', fail) :-
    plus_e(a, _).
test('plus_e_fail/2', fail) :-
    plus_e(_, _).
test('plus_e_fail/2', fail) :-
    plus_e(0, 1).
test('plus_e/2') :-
    plus_e(pos_infinity, pos_infinity).
test('plus_e/2') :-
    plus_e(neg_infinity, neg_infinity).
test('plus_e/2') :-
    random(A),
    plus_e(A, A).

% plus_e/3 
test('plus_e_fail/3', fail) :-
    plus_e(a, _, _).
test('plus_e_fail/3', fail) :-
    plus_e(_, a, _).
test('plus_e_fail/3', fail) :-
    plus_e(_, 1, _).
test('plus_e_fail/3', fail) :-
    plus_e(1, _, _).
test('plus_e_fail/3', fail) :-
    plus_e(0, 2, 1.9999).
test('plus_e_fail/3', fail) :-
    plus_e(pos_infinity, neg_infinity, _).
test('plus_e_fail/3', fail) :-
    plus_e(neg_infinity, pos_infinity, _).
test('plus_e/3') :-
    random(A),
    random(B),
    C is (A + B),
    plus_e(A, B, C).
test('plus_e/3') :-
    random(A),
    plus_e(neg_infinity, A, neg_infinity).
test('plus_e/3') :-
    random(A),
    plus_e(A, pos_infinity, pos_infinity).
test('plus_e/3') :-
    plus_e(neg_infinity, neg_infinity, neg_infinity).
test('plus_e/3') :-
    plus_e(pos_infinity, pos_infinity, pos_infinity).

% minus_e/2 
test('minus_e_fail/2', fail) :-
    minus_e(a, 0).
test('minus_e_fail/2', fail) :-
    minus_e(_, _).
test('minus_e_fail/2', fail) :-
    minus_e(0, 1).
test('minus_e_fail/2', fail) :-
    minus_e(pos_infinity, _).
test('minus_e_fail/2', fail) :-
    minus_e(neg_infinity, _).
test('minus_e/2') :-
    random(A),
    B is -A,
    minus_e(A, B).

% minus_e/3 
test('minus_e_fail/3', fail) :-
    minus_e(a, _, _).
test('minus_e_fail/3', fail) :-
    minus_e(_, a, _).
test('minus_e_fail/3', fail) :-
    minus_e(_, 1, _).
test('minus_e_fail/3', fail) :-
    minus_e(1, _, _).
test('minus_e_fail/3', fail) :-
    minus_e(2, 0, 0.11111).
test('minus_e_fail/3', fail) :-
    minus_e(pos_infinity, pos_infinity, _).
test('minus_e_fail/3', fail) :-
    minus_e(neg_infinity, neg_infinity, _).
test('minus_e/3') :-
    random(A),
    random(B),
    C is (A - B),
    minus_e(A, B, C).
test('minus_e/3') :-
    random(A),
    minus_e(neg_infinity, A, neg_infinity).
test('minus_e/3') :-
    random(A),
    minus_e(A, pos_infinity, neg_infinity).

% times_e/2
test('times_e_fail/2', fail) :-
    times_e(a, 0).
test('times_e_fail/2', fail) :-
    times_e(_, _).
test('times_e_fail/2', fail) :-
    times_e(0, 1).
test('times_e/2') :-
    times_e(pos_infinity, pos_infinity).
test('times_e/2') :-
    times_e(neg_infinity, neg_infinity).
test('times_e/2') :-
    random(A),
    times_e(A, A).

% times_e/3 
test('times_e_fail/3', fail) :-
    times_e(a, _, _).
test('times_e_fail/3', fail) :-
    times_e(_, a, _).
test('times_e_fail/3', fail) :-
    times_e(_, 1, _).
test('times_e_fail/3', fail) :-
    times_e(1, _, _).
test('times_e_fail/3', fail) :-
    times_e(1, 2, 1.9999).
test('times_e_fail/3', fail) :-
    times_e(pos_infinity, 0, _).
test('times_e_fail/3', fail) :-
    times_e(0, pos_infinity, _).
test('times_e_fail/3', fail) :-
    times_e(neg_infinity, 0, _).
test('times_e_fail/3', fail) :-
    times_e(0, neg_infinity, _).
test('times_e/3') :-
    random(A),
    random(B),
    C is (A * B),
    times_e(A, B, C).
test('times_e/3') :-
    times_e(neg_infinity, 10, neg_infinity).
test('times_e/3') :-
    times_e(10, neg_infinity, neg_infinity).
test('times_e/3') :-
    times_e(pos_infinity, 10, pos_infinity).
test('times_e/3') :-
    times_e(10, pos_infinity, pos_infinity).
test('times_e/3') :-
    times_e(neg_infinity, -10, pos_infinity).
test('times_e/3') :-
    times_e(pos_infinity, -10, neg_infinity).
test('times_e/3') :-
    times_e(-10, pos_infinity, neg_infinity).
test('times_e/3') :-
    times_e(-10, neg_infinity, pos_infinity).
test('times_e/3') :-
    times_e(neg_infinity, neg_infinity, pos_infinity).
test('times_e/3') :-
    times_e(pos_infinity, pos_infinity, pos_infinity).

% div_e/2 
test('div_e_fail/2', fail) :-
    div_e(a, 0).
test('div_e_fail/2', fail) :-
    div_e(_, _).
test('div_e_fail/2', fail) :-
    div_e(0, 1).
test('div_e_fail/2', fail) :-
    div_e(pos_infinity, _).
test('div_e_fail/2', fail) :-
    div_e(neg_infinity, _).
test('div_e/2') :-
    random(A),
    B is 1 / A,
    div_e(A, B).

% div_e/3
test('div_e_fail/3', fail) :-
    div_e(a, _, _).
test('div_e_fail/3', fail) :-
    div_e(_, 1, _).
test('div_e_fail/3', fail) :-
    div_e(1, _, _).
test('div_e_fail/3', fail) :-
    div_e(2, 2, 0.999999).
test('div_e_fail/3', fail) :-
    div_e(pos_infinity, pos_infinity, _).
test('div_e_fail/3', fail) :-
    div_e(neg_infinity, neg_infinity, _).
test('div_e_fail/3', fail) :-
    div_e(pos_infinity, neg_infinity, _).
test('div_e_fail/3', fail) :-
    div_e(neg_infinity, pos_infinity, _).
test('div_e_fail/3', fail) :-
    div_e(neg_infinity, 0, _).
test('div_e_fail/3', fail) :-
    div_e(pos_infinity, 0, _).
test('div_e_fail/3', fail) :-
    div_e(0, 0, _).
test('div_e_fail/3', fail) :-
    random(A),
    div_e(A, 0, _).
test('div_e/3') :-
    random(A),
    random(B),
    C is (A / B),
    div_e(A, B, C).
test('div_e/3') :-
    div_e(neg_infinity, 10, neg_infinity).
test('div_e/3') :-
    div_e(pos_infinity, 10, pos_infinity).
test('div_e/3') :-
    random(A),
    div_e(A, neg_infinity, 0).
test('div_e/3') :-
    random(A),
    div_e(A, pos_infinity, 0).
test('div_e/3') :-
    div_e(neg_infinity, -10, pos_infinity).
test('div_e/3') :-
    div_e(pos_infinity, -10, neg_infinity).
test('div_e/3') :-  
    div_e(0, pos_infinity, 0).
test('div_e/3') :-
    div_e(0, neg_infinity, 0).
test('div_e/3') :-
    random(A),
    div_e(0, A, 0).

% er_min
test(er_min_fail, fail) :-
    er_min(a, _, _).
test(er_min_fail, fail) :-
    er_min(neg_infinity, pos_infinity, pos_infinity).
test(er_min):-
    er_min(neg_infinity, pos_infinity, neg_infinity).
test(er_min):-
    er_min(0, 0, 0).
test(er_min):-
    er_min(0, pos_infinity, 0).
test(er_min):-
    er_min(neg_infinity, 0, neg_infinity).
test(er_min):-
    er_min(neg_infinity, neg_infinity, neg_infinity).
test(er_min):- 
    er_min(pos_infinity, pos_infinity, pos_infinity).
test(er_min):- 
    er_min(-100, 0, -100).
% er_max
test(er_max_fail, fail) :-
    er_max(a, _, _).
test(er_max_fail, fail) :-
    er_max(neg_infinity, pos_infinity, neg_infinity).
test(er_max) :-
    er_max(neg_infinity, pos_infinity, pos_infinity).
test(er_max) :-
    er_max(0, 0, 0).
test(er_max) :-
    er_max(0, pos_infinity, pos_infinity).
test(er_max) :-
    er_max(neg_infinity, 0, 0).
test(er_max) :-
    er_max(neg_infinity, neg_infinity, neg_infinity).
test(er_max) :-
    er_max(pos_infinity, pos_infinity, pos_infinity).
test(er_max) :-
    er_max(-100, 0, 0).

% extended_real_min_list
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list(_, _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([_], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([1,0,2, _], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([_, 0, 1], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([a, 0, 1], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([0, 1, a], _).
test(extended_real_min_list_fail, fail) :-
    extended_real_min_list([0, 1, neg_infinity], 0).

test(extended_real_min_list) :-
    extended_real_min_list([0, 1, 2], 0).
test(extended_real_min_list) :-
    extended_real_min_list([0, 1, 0], 0).
test(extended_real_min_list) :-
    extended_real_min_list([0, 0, 0], 0).
test(extended_real_min_list) :-
    extended_real_min_list([-100000, 0, neg_infinity], neg_infinity).
test(extended_real_min_list) :-
    extended_real_min_list([1000000000,10000000, pos_infinity], 10000000).
test(extended_real_min_list) :-
    extended_real_min_list([neg_infinity, 0, pos_infinity], neg_infinity).
test(extended_real_min_list) :-
    extended_real_min_list([neg_infinity, 0, -10000000], neg_infinity).
test(extended_real_min_list) :-
    extended_real_min_list([pos_infinity, 0, 10000000], 0).

% extended_real_max_list
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list(_, _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([_], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([1,0,2, _], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([_, 0, 1], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([a, 0, 1], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([0, 1, a], _).
test(extended_real_max_list_fail, fail) :-
    extended_real_max_list([0, 1, pos_infinity], 1).

test(extended_real_max_list) :-
    extended_real_max_list([0, 1, 2], 2).
test(extended_real_max_list) :-
    extended_real_max_list([2, 1, 2], 2).
test(extended_real_max_list) :-
    extended_real_max_list([0, 0, 0], 0).
test(extended_real_max_list) :-
    extended_real_max_list([-100000, 0, neg_infinity], 0).
test(extended_real_max_list) :-
    extended_real_max_list([1000000000,10000000, pos_infinity], pos_infinity).
test(extended_real_max_list) :-
    extended_real_max_list([neg_infinity, 0, pos_infinity], pos_infinity).
test(extended_real_max_list) :-
    extended_real_max_list([neg_infinity, 0, -10000000], 0).
test(extended_real_max_list) :-
    extended_real_max_list([pos_infinity, 0, 10000000], pos_infinity).

%%% gli intervalli disgiunti li gestiamo a parte

%%% Interval Tests

% empty_interval/1
test(empty_interval) :- 
    empty_interval([]).

% interval/2
% Test con numero reale positivo
test('interval/2_singleton') :- 
    interval(5, [5, 5]),
    interval(-3, [-3, -3]),
    interval(0, [0, 0]),
    interval(neg_infinity, [neg_infinity, neg_infinity]),
    interval(pos_infinity, [pos_infinity, pos_infinity]).

% Test con fallimento su variabile libera (deve fallire)
test('interval/2_fail_variable', fail) :- 
    interval(_, _).
% Test con fallimento su match sbagliato
test('interval/2_fail_wrong_match', fail) :- 
    interval(5, [6, 6]).


% interval/3
% Test con intervallo valido (inferiore e superiore uguali)
test('interval/3_equal_bounds') :- 
    interval(5, 5, [5, 5]),
    interval(-3, 2, [-3, 2]),
    interval(neg_infinity, neg_infinity, [neg_infinity, neg_infinity]),
    interval(pos_infinity, pos_infinity, [pos_infinity, pos_infinity]),
    interval(neg_infinity, pos_infinity, [neg_infinity, pos_infinity]).

% Test con intervallo vuoto (inferiore maggiore del superiore)
test('interval/3_empty_interval') :- 
    interval(5, 3, []),
    interval(pos_infinity, 2, []),
    interval(-2, neg_infinity, []),
    interval(pos_infinity, neg_infinity, []).
% Test con fallimento su variabile libera per L H
test('interval/3_fail_var_L', fail) :- 
    interval(a, _, _).
test('interval/3_fail_var_H', fail) :- 
    interval(_, a, _).
% Test con fallimento su variabili libere per L e H
test('interval/3_fail_vars', fail) :- 
    interval(_, _, _).
% Test con fallimento su L non esteso reale
test('interval/3_fail_non_extended_real1', fail) :- 
    interval(a, 3, _).
test('interval/3_fail_non_extended_real2', fail) :- 
    interval(3, a, _).
test('interval/3_fail_non_extended_real3', fail) :- 
    interval(a, b, _).

% Test per interval/1
test('is_interval/1_valid') :- 
    is_interval([5, 10]),
    is_interval([neg_infinity, pos_infinity]),
    is_interval([pos_infinity, pos_infinity]),
    is_interval([neg_infinity, neg_infinity]),
    is_interval([3, 3]). % singleton
test('is_interval/1_valid_disgiunti') :- 
    is_interval([[neg_infinity, -1], [1, pos_infinity]]),
    is_interval([[-5, -1], [-3, -1]]),
    is_interval([[-5, -1], [1, pos_infinity]]),
    is_interval([[4, pos_infinity], [1, 5]]).

test('is_interval/1_invalid', fail) :- 
    is_interval([10, 5]).
test('is_interval/1_invalid', fail) :- 
    is_interval([pos_infinity, 5]).
test('is_interval/1_invalid', fail) :- 
    is_interval([5, neg_infinity]).
test('is_interval/1_invalid', fail) :- 
    is_interval([5, pos_infinity, 10]).

% Test per whole_interval/1
test('whole_interval/1_valid') :- 
    whole_interval([neg_infinity, pos_infinity]).
test('whole_interval/1_invalid1', fail) :- 
    whole_interval([5, 10]).
test('whole_interval/1_invalid2', fail) :- 
    whole_interval([neg_infinity, 5]).
test('whole_interval/1_invalid3', fail) :- 
    whole_interval([5, pos_infinity]).
test('whole_interval/1_invalid4', fail) :- 
    whole_interval([pos_infinity, pos_infinity]).
test('whole_interval/1_invalid5', fail) :- 
    whole_interval([neg_infinity, neg_infinity]).

% Test per singleton/1
test('is_singleton/1_valid') :- 
    is_singleton([5, 5]),
    is_singleton([pos_infinity, pos_infinity]),
    is_singleton([neg_infinity, neg_infinity]).

%fail
test('is_singleton/1_invalid1', fail) :- 
    is_singleton([5, 10]).
test('is_singleton/1_invalid2', fail) :- 
    is_singleton([10, 5]).
test('is_singleton/1_invalid3', fail) :- 
    is_singleton([neg_infinity, pos_infinity]).

% Test per iinf/2
test('iinf/2_valid') :- 
    iinf([5, 10], 5),
    iinf([neg_infinity, 10], neg_infinity),
    iinf([5, pos_infinity], 5).

test('iinf/2_invalid', fail) :- 
    iinf([5, 10], 10).
test('iinf/2_invalid', fail) :- 
    iinf([neg_infinity, pos_infinity], 5).
test('iinf/2_invalid', fail) :- 
    iinf([10, 5], 10).
%test per iinf/2 disgiunti
test('iinf/2_valid_dis') :- 
    iinf([[neg_infinity, -1], [1, pos_infinity]], neg_infinity).
test('iinf/2_valid_dis') :- 
    iinf([[neg_infinity, -4], [-3, -1]], neg_infinity).
test('iinf/2_valid_dis') :- 
    iinf([[-5, -1], [1, pos_infinity]], -5).
test('iinf/2_valid_dis') :-
    iinf([[4, pos_infinity], [1, 5]], 1).

% Test per isup/2
test('isup/2_valid') :- 
    isup([5, 10], 10),
    isup([neg_infinity, 10], 10),
    isup([5, pos_infinity], pos_infinity).

%test per isup/2 disgiunti
test('isup/2_valid_dis') :- 
    isup([[neg_infinity, -1], [1, pos_infinity]], pos_infinity).
test('isup/2_valid_dis') :- 
    isup([[neg_infinity, -4], [-3, -1]], -1).
test('isup/2_valid_dis') :- 
    isup([[-5, -1], [1, pos_infinity]], pos_infinity).
test('isup/2_valid_dis') :- 
    isup([[4, pos_infinity], [1, 5]], pos_infinity).

test('isup/2_invalid', fail) :- 
    isup([5, 10], 5).
test('iinf/2_invalid', fail) :- 
    isup([neg_infinity, pos_infinity], 10).
test('iinf/2_invalid', fail) :- 
    isup([10, 5], 10).

% Test per icontains/2
test('icontains/2_valid') :- 
    icontains([5, 10], 7),
    icontains([2, 3], [2,3]),
    icontains([2, pos_infinity], pos_infinity),
    icontains([neg_infinity, pos_infinity], [5, 10]),
    icontains([neg_infinity, pos_infinity], 0),
    icontains([5, 10], [6, 8]),
    icontains([5, 10], [5, 7]).

test('icontains/2_invalid', fail) :- 
    icontains([5, 10], 11).
test('icontains/2_invalid', fail) :- 
    icontains([5, 10], [4, 6]).
test('icontains/2_invalid', fail) :- 
    icontains([5, 10], [11, 12]).
test('icontains/2_invalid', fail) :- 
    icontains([pos_infinity, pos_infinity], [neg_infinity, 12]).
test('icontains/2_invalid', fail) :- 
    icontains([pos_infinity, pos_infinity], [neg_infinity, neg_infinity]).
% ioverlap/2
test(ioverlap) :- 
    ioverlap([1, 5], [3, 7]),
    ioverlap([1, 5], [5, 10]).
test(ioverlap_fail, fail) :-
    ioverlap([pos_infinity, pos_infinity], [neg_infinity, neg_infinity]).
test(ioverlap_fail, fail) :-
    ioverlap([neg_infinity, neg_infinity], [pos_infinity, pos_infinity]).
test(ioverlap_fail, fail) :- 
    ioverlap([1, 5], [6, 10]).
test(ioverlap_fail, fail) :- 
    ioverlap([], [3, 7]).

%%% Interval Arithmetic Tests

% iplus/2
test('iplus/2_unifies') :- 
    iplus([1, 2], [1, 2]),
    iplus([pos_infinity, pos_infinity], [pos_infinity, pos_infinity]),
    iplus([neg_infinity, neg_infinity], [neg_infinity, neg_infinity]).
test('iplus/2_singleton') :- 
    iplus(2, [2, 2]),
    iplus(pos_infinity, [pos_infinity, pos_infinity]),
    iplus(neg_infinity, [neg_infinity, neg_infinity]).
test('iplus/2_fail', fail) :- 
    iplus([1, 2],  [-2, -1]).
test('iplus/2_fail', fail) :- 
    iplus([], _).

% iplus/3
% Test base per somma di intervalli finiti
test('iplus/3') :- 
    iplus([1, 2], [2, 3], [3, 5]).
% Test con intervallo infinito positivo
test('iplus/3_pos_infinity') :- 
    iplus([1, pos_infinity], [2, 3], [3, pos_infinity]).
% Test con intervallo infinito negativo
test('iplus/3_neg_infinity') :- 
    iplus([neg_infinity, 1], [2, 3], [neg_infinity, 4]).
% Test con intervallo positivo e negativo
test('iplus/3_pos_neg_infinity') :- 
    iplus([neg_infinity, 1], [2, pos_infinity], [neg_infinity, pos_infinity]).
% Test con intervallo infinito positivo e infinito negativo
test('iplus/3_inf_inf') :- 
    iplus([neg_infinity, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).
% Test con intervallo finito e infinito negativo
test('iplus/3_finite_neg_infinity') :- 
    iplus([1, 2], [neg_infinity, 3], [neg_infinity, 5]).

% Test che fallisce quando l'intervallo risultante è errato
test('iplus/3_fail', fail) :- 
    iplus([1, 2], [2, 3], [4, 6]).
test('iplus/3_fail2', fail) :- 
    iplus([neg_infinity, pos_infinity], [pos_infinity, pos_infinity], _).
test('iplus/3_fail3', fail) :- 
    iplus([neg_infinity, pos_infinity], [neg_infinity, neg_infinity], _).
test('iplus/3_fail3', fail) :- 
    iplus([neg_infinity, neg_infinity], [pos_infinity, pos_infinity], _).


% iminus/2
test('iminus/2') :- 
    iminus([1, 2], [-2, -1]).

test('iminus/2_fail', fail) :- 
    iminus([], _).

% iminus/3
% Test base per sottrazione di intervalli finiti
test('iminus/3') :- 
    iminus([3, 5], [1, 2], [1, 4]).

% Test con intervallo infinito positivo
test('iminus/3_pos_infinity') :- 
    iminus([3, pos_infinity], [1, 2], [1, pos_infinity]).

% Test con intervallo infinito negativo
test('iminus/3_neg_infinity') :- 
    iminus([neg_infinity, 5], [1, 2], [neg_infinity, 4]).

% Test con intervallo positivo e negativo
test('iminus/3_pos_neg_infinity') :- 
    iminus([neg_infinity, 5], [1, pos_infinity], [neg_infinity, 4]).

% Test con intervallo infinito positivo e infinito negativo
test('iminus/3_inf_inf') :- 
    iminus([neg_infinity, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).

% Test con intervallo finito e infinito negativo
test('iminus/3_finite_neg_infinity') :- 
    iminus([1, 5], [neg_infinity, 2], [-1, pos_infinity]).

% Test che fallisce quando l'intervallo risultante è errato
test('iminus/3_fail', fail) :- 
    iminus([3, 5], [1, 2], [2, 4]).

% itimes/2  
test('itimes/2') :- 
    itimes([2, 3], [2, 3]).
test('itimes/2_fail', fail) :- 
    itimes(a, _).
test('itimes/2_fail', fail) :- 
    itimes([], _).

% itimes/3
% Test base per moltiplicazione di intervalli finiti
test('itimes/3') :- 
    itimes([2, 3], [4, 5], [8, 15]).

% Test con intervallo infinito positivo
test('itimes/3_pos_infinity') :- 
    itimes([2, pos_infinity], [3, 5], [6, pos_infinity]).

% Test con intervallo infinito negativo
test('itimes/3_neg_infinity') :- 
    itimes([neg_infinity, -2], [3, 5], [neg_infinity, -6]).

% Test con intervallo positivo e negativo
test('itimes/3_pos_neg_infinity') :- 
    itimes([neg_infinity, 2], [3, 5], [neg_infinity, 10]).

% Test con intervallo infinito positivo e infinito negativo
test('itimes/3_inf_inf') :- 
    itimes([neg_infinity, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).

% Test con intervallo finito e infinito negativo
test('itimes/3_finite_neg_infinity') :- 
    itimes([1, 3], [neg_infinity, 4], [neg_infinity, 12]).

% Test che fallisce quando l'intervallo risultante è errato
test('itimes/3_fail', fail) :- 
    itimes([2, 3], [4, 5], [9, 15]).



% idiv/2
test('idiv/2') :- 
    idiv([4, 8], [0.125, 0.25]).

test('idiv/2_fail', fail) :- 
    idiv([], _).

% idiv/3

% [0,0] / I
test('idiv/3_case_0_i') :- 
    idiv([0, 0], [1, 2], [0, 0]),
    idiv([0, 0], [1, pos_infinity], [0, 0]),
    idiv([0, 0], [neg_infinity, 2], [0, 0]),
    idiv([0, 0], [neg_infinity, neg_infinity], [0, 0]),
    idiv([0, 0], [pos_infinity, pos_infinity], [0, 0]),
    idiv([0, 0], [neg_infinity, pos_infinity], [0, 0]).
% I / [0,0]
test('idiv/3_case_i_0', fail) :- 
    idiv([1, 2], [0, 0], _),
    idiv([1, pos_infinity], [0, 0], _),
    idiv([neg_infinity, 2], [0, 0], _),
    idiv([neg_infinity, neg_infinity], [0, 0], _),
    idiv([pos_infinity, pos_infinity], [0, 0], _),
    idiv([neg_infinity, pos_infinity], [0, 0], _).
% 1 CASO P1 P \{0} a/d b/c
test('idiv/3_case_p1_p') :- 
    idiv([1, 4], [1, 2], [0.5, 4]),
    idiv([1, 4], [1, pos_infinity], [0, 4]),
    idiv([1, 4], [pos_infinity, pos_infinity], [0, 0]).
test('idiv/3_case_p1_p') :-
    idiv([1, pos_infinity], [1, 2], [0.5, pos_infinity]),
    idiv([1, pos_infinity], [1, pos_infinity], [0, pos_infinity]).
test('idiv/3_case_p1_p', fail) :-
    idiv([1, pos_infinity], [pos_infinity, pos_infinity], _).
test('idiv/3_case_p1_p') :-
    idiv([pos_infinity, pos_infinity], [1, 4], [pos_infinity, pos_infinity]).
test('idiv/3_case_p1_p', fail) :-
    idiv([pos_infinity, pos_infinity], [1, pos_infinity], _).
test('idiv/3_case_p1_p', fail) :-
    idiv([pos_infinity, pos_infinity], [pos_infinity, pos_infinity], _).
% Eccezione: C=0 \{0}
test('idiv/3_case_p1_p_exception') :- 
    idiv([1, 4], [0, 2], [0.5, pos_infinity]),
    idiv([1, 4], [0, pos_infinity], [0, pos_infinity]).%
test('idiv/3_case_p1_p') :- 
    idiv([1, pos_infinity], [0, 2], [0.5, pos_infinity]),
    idiv([1, pos_infinity], [0, pos_infinity], [0, pos_infinity]).
test('idiv/3_case_p1_p') :- 
    idiv([pos_infinity, pos_infinity], [0, 2], [pos_infinity, pos_infinity]).
test('idiv/3_case_p1_p', fail) :-
    idiv([pos_infinity, pos_infinity], [0, pos_infinity], _).

% 2 CASO P0 P \{0} 0 b/c
test('idiv/3_case_p0_p') :- 
    idiv([0, 4], [1, 2], [0, 4]),
    idiv([0, 4], [1, pos_infinity], [0, 4]),
    idiv([0, 4], [pos_infinity, pos_infinity], [0, 0]).
test('idiv/3_case_p0_p') :-
    idiv([0, pos_infinity], [1, 2], [0, pos_infinity]),
    idiv([0, pos_infinity], [1, pos_infinity], [0, pos_infinity]).
test('idiv/3_case_p0_p', fail) :-
    idiv([0, pos_infinity], [pos_infinity, pos_infinity], _).
% Eccezione: C=0 
test('idiv/3_case_p0_p_exception') :- 
    idiv([0, 4], [0, 2], [0, pos_infinity]),
    idiv([0, 4], [0, pos_infinity], [0, pos_infinity]).

% 3 CASO M P a/c b/c
test('idiv/3_case_m_p') :- 
    idiv([-4, 4], [1, 2], [-4, 4]),
    idiv([-4, 4], [1, pos_infinity], [-4, 4]),
    idiv([-4, 4], [pos_infinity, pos_infinity], [0, 0]).

test('idiv/3_case_m_p') :-
    idiv([-4, pos_infinity], [1, 2], [-4, pos_infinity]),
    idiv([-4, pos_infinity], [1, pos_infinity], [-4, pos_infinity]).
test('idiv/3_case_m_p', fail) :-
    idiv([-4, pos_infinity], [pos_infinity, pos_infinity], _).

test('idiv/3_case_m_p') :-
    idiv([neg_infinity, 4], [1, 2], [neg_infinity, 4]),
    idiv([neg_infinity, 4], [1, pos_infinity], [neg_infinity, 4]).
test('idiv/3_case_m_p', fail) :-
    idiv([neg_infinity, 4], [pos_infinity, pos_infinity], _).

test('idiv/3_case_m_p') :-
    idiv([neg_infinity, pos_infinity], [1, 2], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [1, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_p', fail) :-
    idiv([neg_infinity, pos_infinity], [pos_infinity, pos_infinity], _).

%eccezione C=0
test('idiv/3_case_m_p') :- 
    idiv([-4, 4], [0, 2], [neg_infinity, pos_infinity]),
    idiv([-4, 4], [0, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_p') :-
    idiv([-4, pos_infinity], [0, 2], [neg_infinity, pos_infinity]),
    idiv([-4, pos_infinity], [0, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_p') :-
    idiv([neg_infinity, 4], [0, 2], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 4], [0, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_p') :-
    idiv([neg_infinity, pos_infinity], [0, 2], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [0, pos_infinity], [neg_infinity, pos_infinity]).

% 4 CASO N0 P a/c 0
test('idiv/3_case_n0_p') :- 
    idiv([-4, 0], [1, 2], [-4, 0]),
    idiv([-4, 0], [1, pos_infinity], [-4, 0]),
    idiv([-4, 0], [pos_infinity, pos_infinity], [0, 0]).
test('idiv/3_case_n0_p') :-
    idiv([neg_infinity, 0], [1, 3], [neg_infinity, 0]),
    idiv([neg_infinity, 0], [1, pos_infinity], [neg_infinity, 0]).
test('idiv/3_case_n0_p', fail) :-
    idiv([neg_infinity, 0], [pos_infinity, pos_infinity], _).
% Eccezione: c=0
test('idiv/3_case_n0_p_exception') :- 
    idiv([-4, 0], [0, 2], [neg_infinity, 0]),
    idiv([-4, 0], [0, pos_infinity], [neg_infinity, 0]).
test('idiv/3_case_n0_p_exception') :-
    idiv([neg_infinity, 0], [0, 3], [neg_infinity, 0]),
    idiv([neg_infinity, 0], [0, pos_infinity], [neg_infinity, 0]).
test('idiv/3_case_n0_p_exception', fail) :-
    idiv([neg_infinity, 0], [pos_infinity, pos_infinity], _).

% 5 CASO N1 P \{0} a/c b/d
test('idiv/3_case_n1_p') :- 
    idiv([-4, -1], [1, 2], [-4, -0.5]),
    idiv([-4, -1], [1, pos_infinity], [-4, 0]),%
    idiv([-4, -1], [pos_infinity, pos_infinity], [0, 0]).%
test('idiv/3_case_n1_p') :-
    idiv([neg_infinity, -1], [1, 2], [neg_infinity, -0.5]),
    idiv([neg_infinity, -1], [1, pos_infinity], [neg_infinity, 0]).%
test('idiv/3_case_n1_p', fail) :-
    idiv([neg_infinity, -1], [pos_infinity, pos_infinity], _).%
% Eccezione: Divisione per zero \{0}
test('idiv/3_case_n1_p_exception') :- 
    idiv([-4, -1], [0, 2], [neg_infinity, -0.5]),
    idiv([-4, -1], [0, pos_infinity], [neg_infinity, 0]).
test('idiv/3_case_n1_p_exception') :-
    idiv([neg_infinity, -4], [0, 2], [neg_infinity, -2]),
    idiv([neg_infinity, -10], [0, pos_infinity], [neg_infinity, 0]).
test('idiv/3_case_n1_p_exception') :-
   idiv([neg_infinity, neg_infinity], [0, 2], [neg_infinity, neg_infinity]).
test('idiv/3_case_n1_p_fail', fail) :-
   idiv([neg_infinity, neg_infinity], [0, pos_infinity], _).

% INTERVALLI DISGIUNTI
% 6 CASO P1 M
test('idiv/3_case_p1_m') :- 
    idiv([1, 4], [-2, 2], [[neg_infinity, -0.5], [0.5, pos_infinity]]),
    idiv([2, 6], [neg_infinity, 1], [[neg_infinity, 0], [2, pos_infinity]]),
    idiv([1, 5], [-2, pos_infinity], [[neg_infinity, -0.5], [0, pos_infinity]]),
    idiv([1, pos_infinity], [-5, 2], [[neg_infinity, -0.2], [0.5, pos_infinity]]),
    idiv([pos_infinity, pos_infinity], [-2, 4], [[neg_infinity, neg_infinity], [pos_infinity, pos_infinity]]),
    idiv([2, pos_infinity], [-2, 4], [[neg_infinity, -1], [0.5, pos_infinity]]).

test('idiv/3_case_p1_m', fail) :- 
    idiv([pos_infinity, pos_infinity], [neg_infinity, pos_infinity], _).
% 7 CASO P0 M
test('idiv/3_case_p0_m') :- 
    idiv([0, 2], [-2, 2], [neg_infinity, pos_infinity]),
    idiv([0, 4], [-2, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([0, pos_infinity], [-5, 3], [neg_infinity, pos_infinity]).
test('idiv/3_case_p0_m') :- 
    idiv([0, 3], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([0, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).

% 8 CASO M M
test('idiv/3_case_m_m') :- 
    idiv([-4, 1], [-2, 2], [neg_infinity, pos_infinity]),
    idiv([-5, 2], [-2, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 2], [-2, 4], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 2], [neg_infinity, 4], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 2], [-2, pos_infinity], [neg_infinity, pos_infinity]). 
test('idiv/3_case_m_m') :-
    idiv([neg_infinity, pos_infinity], [-2, 4], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [-2, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [neg_infinity, 4], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_m') :-
    idiv([-2, 4], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([-2, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 4], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_m') :- 
    idiv([neg_infinity, pos_infinity], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).
% 9 CASO N0 M
test('idiv/3_case_n0_m') :- 
    idiv([-2, 0], [-2, 2], [neg_infinity, pos_infinity]),
    idiv([-2, 0], [-2, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([-2, 0], [neg_infinity, 2], [neg_infinity, pos_infinity]),
    idiv([-2, 0], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).
test('idiv/3_case_n0_m') :-
    idiv([-4, 0], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 0], [-5, 3], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 0], [-2, pos_infinity], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 0], [neg_infinity, 2], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 0], [neg_infinity, pos_infinity], [neg_infinity, pos_infinity]).

% 10 CASO N1 M
test('idiv/3_case_n1_m') :- 
    idiv([-4, -1], [-2, 2], [[neg_infinity, -0.5], [0.5, pos_infinity]]),
    idiv([-5, -1], [-2, pos_infinity], [[neg_infinity, 0], [0.5, pos_infinity]]),
    idiv([-5, -1], [neg_infinity, 2], [[neg_infinity, -0.5], [0, pos_infinity]]),%
    idiv([neg_infinity, -2], [-2, 4], [[neg_infinity, -0.5], [1, pos_infinity]]),
    idiv([neg_infinity, -2], [-2, pos_infinity], [[neg_infinity, 0], [1, pos_infinity]]),
    idiv([neg_infinity, -2], [neg_infinity, 4], [[neg_infinity, -0.5], [0, pos_infinity]]),
    idiv([neg_infinity, -2], [neg_infinity, pos_infinity], [[neg_infinity, 0], [0, pos_infinity]]).%
test('idiv/3_case_n1_m') :-
    idiv([neg_infinity, neg_infinity], [-2, 4], [[neg_infinity, neg_infinity], [pos_infinity, pos_infinity]]).
test('idiv/3_case_n1_m_fail', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, 4], fail).
test('idiv/3_case_n1_m_fail', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, pos_infinity], fail).
test('idiv/3_case_n1_m_fail', fail) :-
    idiv([neg_infinity, neg_infinity], [-2, pos_infinity], [[neg_infinity, neg_infinity], [pos_infinity, pos_infinity]]).

% 11 CASO P1 N \{0}
test('idiv/3_case_p1_n') :- 
    idiv([1, 4], [-2, -1], [-4, -0.5]),
    idiv([1, 4], [neg_infinity, -1], [-4, 0]),
    idiv([1, 4], [neg_infinity, neg_infinity], [0, 0]).

test('idiv/3_case_p1_n') :-
    idiv([1, pos_infinity], [-2, -1], [neg_infinity, -0.5]),
    idiv([1, pos_infinity], [neg_infinity, -1], [neg_infinity, 0]).%
test('idiv/3_case_p1_n', fail) :-   
    idiv([1, pos_infinity], [neg_infinity, neg_infinity], _).

test('idiv/3_case_p1_n') :-
    idiv([pos_infinity, pos_infinity], [-2, -1], [neg_infinity, neg_infinity]).
test('idiv/3_case_p1_n', fail) :- 
    idiv([pos_infinity, pos_infinity], [neg_infinity, -1], _).
test('idiv/3_case_p1_n', fail) :- 
    idiv([pos_infinity, pos_infinity], [neg_infinity, neg_infinity], fail).

% Eccezione: Divisione per zero
test('idiv/3_case_p1_n_exception') :- 
    idiv([1, 4], [-2, 0], [neg_infinity, -0.5]),
    idiv([1, 4], [neg_infinity, 0], [neg_infinity, 0]).
test('idiv/3_case_p1_n_exception') :-
    idiv([1, pos_infinity], [-2, 0], [neg_infinity, -0.5]),
    idiv([1, pos_infinity], [neg_infinity, 0], [neg_infinity, 0]).%
test('idiv/3_case_p1_n_exception') :-
    idiv([pos_infinity, pos_infinity], [-2, 0], [neg_infinity, neg_infinity]).
test('idiv/3_case_p1_n', fail) :- 
    idiv([pos_infinity, pos_infinity], [neg_infinity, 0], _).

% 12 CASO P0 N
test('idiv/3_case_p0_n') :- 
    idiv([0, 4], [-2, -1], [-4, 0]),
    idiv([0, 4], [neg_infinity, -1], [-4, 0]),
    idiv([0, 4], [neg_infinity, neg_infinity], [0, 0]).

test('idiv/3_case_p0_n') :-
    idiv([0, pos_infinity], [-2, -1], [neg_infinity, 0]),
    idiv([0, pos_infinity], [neg_infinity, -1], [neg_infinity, 0]).
test('idiv/3_case_p0_n', fail) :-
    idiv([0, pos_infinity], [neg_infinity, neg_infinity], fail).

% Eccezione: Divisione per zero
test('idiv/3_case_p0_n_exception') :- 
    idiv([0, 4], [-2, 0], [neg_infinity, 0]),
    idiv([0, 4], [neg_infinity, 0], [neg_infinity, 0]),
    idiv([0, 4], [neg_infinity, neg_infinity], [0, 0]).
test('idiv/3_case_p0_n_exception') :-
    idiv([0, pos_infinity], [-2, 0], [neg_infinity, 0]).
test('idiv/3_case_p0_n', fail) :- 
    idiv([0, pos_infinity], [neg_infinity, 0], fail).
test('idiv/3_case_p0_n', fail) :- 
    idiv([0, pos_infinity], [neg_infinity, neg_infinity], fail).

% 13 CASO M N
test('idiv/3_case_m_n') :- 
    idiv([-1, 4], [-2, -1], [-4, 1]),
    idiv([-1, 4], [neg_infinity, -1], [-4, 1]),
    idiv([-1, 4], [neg_infinity, neg_infinity], [0, 0]).
test('idiv/3_case_m_n') :-
   idiv([-1, pos_infinity], [-2, -1], [neg_infinity, 1]),
    idiv([-1, pos_infinity], [neg_infinity, -1], [neg_infinity, 1]).
    %idiv([-1, pos_infinity], [neg_infinity, neg_infinity], fail).
test('idiv/3_case_m_n') :-
    idiv([neg_infinity, 4], [-2, -1], [-4, pos_infinity]),
    idiv([neg_infinity, 4], [neg_infinity, -1], [-4, pos_infinity]).
test('idiv/3_case_m_n', fail) :-
    idiv([neg_infinity, 4], [neg_infinity, neg_infinity], fail).
test('idiv/3_case_m_n') :-
    idiv([neg_infinity, pos_infinity], [-2, -1], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [neg_infinity, -1], [neg_infinity, pos_infinity]).
test('idiv/3_case_m_n', fail) :-
    idiv([neg_infinity, pos_infinity], [neg_infinity, neg_infinity], fail).
test('idiv/3_case_m_n', fail) :-
    idiv([neg_infinity, pos_infinity], [neg_infinity, neg_infinity], fail).
% Eccezione: Divisione per zero
test('idiv/3_case_m_n_exception') :- 
    idiv([-1, 4], [-2, 0], [neg_infinity, pos_infinity]),
    idiv([-1, pos_infinity], [-2, 0], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, 4], [-2, 0], [neg_infinity, pos_infinity]),
    idiv([neg_infinity, pos_infinity], [-2, 0], [neg_infinity, pos_infinity]).

% 14 CASO N0 N
test('idiv/3_case_n0_n') :- %0 a/d
    idiv([-1, 0], [-2, -1], [0, 1]),
    idiv([-1, 0], [neg_infinity, -1], [0, 1]),
    idiv([-1, 0], [neg_infinity, neg_infinity], [0, 0]).
test('idiv/3_case_n0_n') :- 
    idiv([neg_infinity, 0], [-2, -1], [0, pos_infinity]),
    idiv([neg_infinity, 0], [neg_infinity, -1], [0, pos_infinity]).
test('idiv/3_case_n0_n', fail) :-
    idiv([neg_infinity, 0], [neg_infinity, neg_infinity], fail).

% Eccezione: Divisione per zero
test('idiv/3_case_n0_n_exception') :- 
    idiv([-1, 0], [-2, 0], [0, pos_infinity]),
    idiv([-1, 0], [neg_infinity, 0], [0, pos_infinity]).
test('idiv/3_case_n0_n_exception') :- 
    idiv([neg_infinity, 0], [-2, 0], [0, pos_infinity]),
    idiv([neg_infinity, 0], [neg_infinity, 0], [0, pos_infinity]).

% 15 CASO N1 N
test('idiv/3_case_n1_n') :- % b/c a/d   
    idiv([-2, -1], [-2, -1], [0.5, 2]),
    idiv([-2, -1], [neg_infinity, -1], [0, 2]),
    idiv([-2, -1], [neg_infinity, neg_infinity], [0, 0]).
test('idiv/3_case_n1_n') :-
    idiv([neg_infinity, -1], [-2, -1], [0.5, pos_infinity]),
    idiv([neg_infinity, -1], [neg_infinity, -1], [0, pos_infinity]).
test('idiv/3_case_n1_n', fail) :-
    idiv([neg_infinity, -1], [neg_infinity, neg_infinity], fail).
test('idiv/3_case_n1_n') :-
    idiv([neg_infinity, neg_infinity], [-2, -1], [pos_infinity, pos_infinity]).
test('idiv/3_case_n1_n', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, -1], fail).
test('idiv/3_case_n1_n', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, neg_infinity], fail).
    

% Eccezione: Divisione per zero
test('idiv/3_case_n1_n') :- % b/c a/d   
    idiv([-2, -1], [-2, 0], [0.5, pos_infinity]),
    idiv([-2, -1], [neg_infinity, 0], [0, pos_infinity]).
test('idiv/3_case_n1_n') :-
    idiv([neg_infinity, -1], [-2, 0], [0.5, pos_infinity]),
    idiv([neg_infinity, -1], [neg_infinity, 0], [0, pos_infinity]).
test('idiv/3_case_n1_n') :-
    idiv([neg_infinity, neg_infinity], [-2, 0], [pos_infinity, pos_infinity]).
test('idiv/3_case_n1_n', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, 0], fail).
test('idiv/3_case_n1_n', fail) :-
    idiv([neg_infinity, neg_infinity], [neg_infinity, neg_infinity], fail).

:- end_tests(intar).