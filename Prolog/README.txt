empty interval è da considera un intervallo normale

inverso di un intervallo: https://elearning.unimib.it/mod/forum/discuss.php?d=270840


Come trattare Infinito:
    l'intervallo [inf, X] è valido se X è inf  (inf senza segno da considerare come positivo)
    però non ha senso come dice il ragazzo nel post perche (+inf, +inf) non è valido

intervalli disgiunti:
    bisogna ancora capire ne stanno parlando sul forum
    l'unica risposta per ora sul argomento che si possono ottenere intervalli disgiunti solo mediante divisione
    teoricamente un intervallo disgiunto non è un intervallo ma unione di piu intervalli
    iiinf e isup credono vada considerato il caso di intervalli disgiunti
    
is_singleton: 
    no se si tratta di []

is_interval:
    se X ha L>H is_interval generare un errore e il predicato prolog deve fallire

is_singleton unifica se gli passi UNA variabile (es. ?- is_singleton([X, 10]). X = 10.)
se si vuole correggere bisogna mettere controlli su var. libere (già messi commentati su is_singleton)

QUERY INTERVALLI disgiunti

%1 CASO P1 P0
    idiv([1, 4], [1, 2], Result).
        Result = [0.5, 4].
        exception case:
            idiv([1, 4], [0, 2], Result). 
                Result = [0.5, pos_infinity].

%2 CASO P0 P
    idiv([0, 4], [1, 2], Result).
        Result = [0, 4].
        exception case:
            idiv([0, 4], [0, 2], Result).  
                Result = [0, pos_infinity].

%3 CASO M P
    idiv([-2, 4], [1, 2], Result). 
        Result = [-2, 4].
        exception case:
            idiv([-2, 4], [0, 2], Result). 
                Result = [neg_infinity, pos_infinity].

%4 CASO N0 P
    idiv([-2, 0], [1, 2], Result). 
        Result = [-2, 0].
        exception case:
            idiv([-2, 0], [0, 2], Result). 
                Result = [neg_infinity, 0].

%5 CASO N1 P
    idiv([-2, -1], [1, 2], Result). 
        Result = [-2, -0.5].
        exception case:
        idiv([-2, -1], [0, 2], Result). 
            Result = [neg_infinity, -0.5].


%INTERVALLI DISGIUNTI
%6 CASO P1 M
    idiv([1, 2], [-2, 4], Result).
        Result = [[neg_infinity, -0.5], [0.25, pos_infinity]].

%7 CASO P0 M
    idiv([0, 2], [-2, 4], Result). 
        Result = [neg_infinity, pos_infinity].

%8 CASO M M
    idiv([-2, 2], [-2, 4], Result). 
        Result = [neg_infinity, pos_infinity].

%9 CASO N0 M
    idiv([-2, 0], [-2, 4], Result).   
        Result = [neg_infinity, pos_infinity].
%10 CASO N1 M
    idiv([-2, -1], [-2, 4], Result).  
         Result = [[neg_infinity, -0.25], [0.5, pos_infinity]].

%11 CASO P1 N
    idiv([1, 4], [-2, -1], Result). 
        Result = [-4, -0.5].
        exception case:
        idiv([1, 4], [-2, 0], Result).   
            Result = [neg_infinity, -0.5].

%12 CASO P0 N
    idiv([0, 4], [-2, 1], Result).  
        Result = [neg_infinity, pos_infinity].
        exception case:
            idiv([0, 4], [-2, 0], Result).  
                Result = [neg_infinity, 0].
%13 CASO M N
    idiv([-1, 4], [-2, -1], Result).  
        Result = [-4, 1].
        exception case:
            idiv([-1, 4], [-2, 0], Result).   
                Result = [neg_infinity, pos_infinity].
%14 CASO N0 N
    idiv([-1, 0], [-2, -1], Result).  
        Result = [0, 1].
        exception case:
        idiv([-1, 0], [-2, 0], Result).  
            Result = [0, pos_infinity].

%15 CASO N1 N
    idiv([-2, -1], [-2, -1], Result).  
        Result = [0.5, 2].
        exception case:
            idiv([-2, -1], [-2, 0], Result).   
                Result = [0.5, pos_infinity].