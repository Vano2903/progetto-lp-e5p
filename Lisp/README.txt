## Specifiche

### Cosa è un "cons-interval"

Un "cons-interval è una particella cons che rappresenta un intervallo
formato da due estremi.
Un cons-interval è una cons cell che ha come valore:

- car: il limite inferiore dell'intervallo
- cdr: il limite superiore dell'intervallo


L'intervallo viene chiamato "intervallo singolo" se la lista di
cons-intervals è formata da un solo cons-interval

###Operazioni aritmetiche
Le operazioni gestiscono

- intervalli finiti
- intervalli infiniti con pos_infinity e neg_infininty
- combinazioni di intervalli disgiunti
- divisione intervalli che contengono lo zero
- gestione variabili non instanziate durante l'esecuzione delle operazioni
- gestione intervallo vuoto