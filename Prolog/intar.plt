:- begin_tests(intar).
:- include(intar).
:- use_module(library(random)).



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
    interval(neg_infinity, pos_infinity, [neg_infinity, pos_infinity]),
    interval(0, 0, [0, 0]),
    interval(neg_infinity, -2, [neg_infinity, -2]),
    interval(2, pos_infinity, [2, pos_infinity]).

% Test con intervallo vuoto (inferiore maggiore del superiore)
test('interval/3_empty_interval') :- 
    interval(5, 3, []),
    interval(-3, -5, []),
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
    icontains([neg_infinity, 2], neg_infinity),
    icontains([neg_infinity, neg_infinity], neg_infinity),
    icontains([pos_infinity, pos_infinity], pos_infinity),
    icontains([neg_infinity, pos_infinity], [5, 10]),
    icontains([neg_infinity, pos_infinity], 0),
    icontains([5, 10], [5, 7]).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity, -1], [1, pos_infinity]], 2).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity, -1], [1, pos_infinity]], -2).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity, -1], [1, pos_infinity]], pos_infinity).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity, -1], [1, pos_infinity]], neg_infinity).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity, -4], [1, pos_infinity]], [[neg_infinity, -5], [4, pos_infinity]]).
test('icontains/2_valid_disjoint', fail) :-
    icontains([[neg_infinity, -4], [1, pos_infinity]], [[neg_infinity, -2], [1, pos_infinity]]).
test('icontains/2_valid_disjoint', fail) :-
    icontains([[neg_infinity, -4], [4, pos_infinity]], [[neg_infinity, -5], [3, pos_infinity]]).
test('icontains/2_valid_disjoint') :-
    icontains([[-1, -1], [0,1], [2,3]], [[2, 3], [0, 1]]).
test('icontains/2_valid_disjoint') :-
    icontains([[-1, -1], [0,1], [2,3]], [[0, 1], [2, 3]]).
test('icontains/2_valid_disjoint', fail) :-
    icontains([[-1, -1], [0,1], [2,3]], [[2, 3], [0, 2]]).
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity,-2], [4,10], [11, pos_infinity]], [[-10,-2], [5,10], [12, pos_infinity]]).     
test('icontains/2_valid_disjoint') :-
    icontains([[neg_infinity,-2], [4,10], [11, pos_infinity]], [[5,10], [12, pos_infinity], [-10,-2]]).       
test('icontains/2_valid_disjoint') :-
    icontains([[-10,-2], [4,10], [11, 30]], [[-10,-2], [5,10]]).    
test('icontains/2_valid_disjoint') :-
    icontains([[1,4], [5, 8]], [[2, 3], [6, 7]]).                  
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
test('ioverlap_valid_disjoint') :-
    ioverlap([[neg_infinity,-2], [4,10], [20, pos_infinity]], [[-10,-1], [2,10], [15, pos_infinity]]). 
test('ioverlap_valid_disjoint', fail) :-
    ioverlap([[neg_infinity,-2], [4,10], [20, pos_infinity]], [[-10,-1], [1,3], [2,10], [15, pos_infinity]]).     
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
test('iplus/2_disjoint') :-
    iplus([[neg_infinity, -2], [-1,1], [3, pos_infinity]], [[neg_infinity, -2], [-1,1], [3, pos_infinity]]).
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

% Test somma intervalli disgiunti stessa lunghezza
test('iplus/3_disjoint') :- 
    iplus([[1, 3], [-4, 5]], [[neg_infinity, 6], [-34, 5], [21, pos_infinity]], X),
    X = [[neg_infinity, 9], [-33, 8], [22, pos_infinity], [neg_infinity, 11], [-38, 10], [17, pos_infinity]].       
test('iplus/3_disjoint') :-
    iplus([[0,10], [20, 30]], [[3, 5]], [[3, 15], [23, 35]]).

% iminus/2
test('iminus/2') :- 
    iminus([1, 2], [-2, -1]).
test('iminus/2_fail', fail) :- % reciproco alla somma applicabile ai reali
    iminus([pos_infinity, pos_infinity], [neg_infinity, neg_infinity]),
    iminus([neg_infinity, neg_infinity], [pos_infinity, pos_infinity]).
test('iminus/2_singleton') :-
    iminus(2, [-2, -2]),
    iminus(-2, [2, 2]).

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

%test('iminus/3_disjoint') :- 


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

test('itimes/3_disjoint') :- % <a,b> * <c,d> = [min(ac,ad,bc,bd), max(ac,ad,bc,bd)]
    itimes([[neg_infinity, -1], [1, pos_infinity]], [[neg_infinity, -1], [1, pos_infinity]], X),
    X = [[1, pos_infinity], [neg_infinity, -1], [neg_infinity, -1], [1, pos_infinity]].
% idiv/2
test('idiv/2') :- 
    idiv([4, 8], [0.125, 0.25]).
test('idiv/2') :-
    idiv([-5, 5], [[neg_infinity, -0.2], [0.2, pos_infinity]]).
test('idiv/2') :-
    idiv([-8, -4], [-0.25, -0.125]).
test('idiv/2_fail', fail) :-
    idiv([0, 5], fail).
test('idiv/2_fail', fail) :-
    idiv([-2, 0], fail).
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

test('idiv/3_disjoint') :-
    idiv([[neg_infinity, -1], [1, pos_infinity]], [[neg_infinity, -1], [1, pos_infinity]], X),
    X = [[0, pos_infinity], [neg_infinity, 0], [neg_infinity, 0], [0, pos_infinity]].
test('idiv/3_disjoint') :-
    idiv([[neg_infinity, -1], [1, pos_infinity]], [[neg_infinity, -6], [2, 4],[7, pos_infinity]], X),
    X = [[0, pos_infinity], [neg_infinity, -0.25], [neg_infinity, 0], [neg_infinity, 0], [0.25, pos_infinity], [0, pos_infinity]].
:- end_tests(intar).