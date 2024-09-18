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

% er_min

% er_max

% extended_real_min_list

% extended_real_max_list

% plus_e/2

% plus_e/3 == extended_real_sum
test('plus_e/3') :-
    random(A),
    random(B),
    C is (A + B),
    plus_e(A, B, C).
test('plus_e_inf/3') :-
    random(A),
    plus_e(neg_infinity, A, neg_infinity).
test('plus_e_fail/3', fail) :-
    plus_e(pos_infinity, neg_infinity, _).

% minus_e/2 = minus_reciprocal

% minus_e/3 == extended_real_subtraction
test('minus_e_fail/3', fail) :-
    minus_e(pos_infinity, pos_infinity, _).

% times_e/3 == extended_real_multiplication
test('times_e_fail/3', fail) :-
    times_e(0, neg_infinity, _).

% extended_real_division

% times_e/2

% div_e/2 = div_reciprocal


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