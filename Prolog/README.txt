###Rappresentazione intervallo
Gli intervalli sono rappresentati in forma di liste con due elementi [L, H], 
dove: L è il limite inferiore dell'intervallo e H il limite superiore.

###Intervalli disgiunti
Per rappresentare intervalli disgiunti (più intervalli non contigui), 
viene utilizzata una lista di intervalli. 
Ogni elemento della lista è un intervallo valido. 
Ad esempio, la lista [[1, 4], [6, 9]] rappresenta intervalli disgiunti.
  
###Operazioni aritmetiche
Le operazioni gestiscono
- intervalli finiti
- intervalli infiniti con pos_infinity e neg_infininty
- combinazioni di intervalli disgiunti
- divisione intervalli che contengono lo zero
- gestione variabili non instanziate durante l'esecuzione delle operazioni
- gestione intervallo vuoto

###Accortezze 
Se le operazioni contengono i disgiunti bisogna rappresentare anche 
l'intervallo singolo tra doppio [], altrimenti lo riconosce come reale.
esempio:
    iplus([[2, 2]], [[1, 4], [6, 7]], R).
    R = [[3, 6], [8, 9]]

    iplus([2], [[1, 4], [6, 7]], R).
    R = [[3, 6], [8, 9]]

    iplus([-1, 1], [[1, 4], [6, 7]], R). 
    R = [[0, 3], [5, 6], [2, 5], [7, 8]].

    iplus([[-1, -1], [1,  1]], [[1, 4], [6, 7]], R). 
    R = [[0, 3], [5, 6], [2, 5], [7, 8]].
