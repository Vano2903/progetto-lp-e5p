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
    minus_e(pos_infinity, pos_infinity, _).
test('minus_e_fail/3', fail) :-
    minus_e(neg_infinity, neg_infinity, _).
test('minus_e_fail/3', fail) :-
    minus_e(a, _, _).


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
    times_e(0, neg_infinity, _).

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


% interval/2

% interval/3

% is_interval

% is_singleton

% iinf

% isup

% icontains

% ioverlap

% iplus/2

% iplus/3

% iminus/2

% iminus/3

% itimes/2

% itimes/3

% idiv/2

% idiv/3

:- end_tests(intar).