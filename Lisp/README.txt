## Specifiche

### Cosa è un "cons-interval"

Un "cons-interval è una particella cons che rappresenta un intervallo
formato da due estremi.
Un cons-interval è una cons cell che ha come valore:

- car: il limite inferiore dell'intervallo
- cdr: il limite superiore dell'intervallo


L'intervallo viene chiamato "intervallo singolo" se la lista di
cons-intervals è formata da un solo cons-interval

### Operazioni aritmetiche

Le operazioni gestiscono

- intervalli finiti
- intervalli infiniti con pos_infinity e neg_infininty
- combinazioni di intervalli disgiunti
- divisione intervalli che contengono lo zero
- gestione variabili non instanziate durante l'esecuzione delle operazioni
- gestione intervallo vuoto


### Accortezze

##### Controllo contains con valore reale

Usando la funzione `contains` nel caso in cui si voglia controllare
che un valore x sia presente all'interno di un intervallo, tale funzione
controlla eventuali punti di discontinuità dell'intervallo quindi, 
ad esempio:
(print-interval (extended-interval '(0) (cons-interval -1 1)))
 -> [-1, 1] ∖ {0}

(contains (extended-interval '(0) (cons-interval -1 1)) 0)
 -> NIL

siccome 0 non fa parte dell'intervallo

### Funzioni ausiliarie

Non richieste ma utili abbiamo creato:

##### interval-to-string

`interval-to-string` che trasforma un intervallo in una stringa
prende come parametri un intervallo e un booleano che dice se è possibile
usare carattere unicode o solo ascii 
(in caso ci siano problemi con la stampa di tali caratteri)

##### print-interval

`print-interval` che stampa l'intervallo in modo più leggibile 
in una stream di output

la funzione prende come parametri un intervallo, 
una stream (default *standard-output*) e un booleano che se è NIL
stampa l'intervallo usando solo caratteri ascii, altrimenti usa
i simboli unicode per rappresentare l'intervallo


##### sort-and-merge-interval

`sort-and-merge-interval` riordina gli intervalli 
(utile se usata su un intervallo disgiunto) e unisce i sottointervalli
che si sovrappongono così da avere un intervallo più compatto

le funzioni non usano questo metodo di default per non alterare
l'intervallo originale ma viene usato da funzioni come `sup` e `inf`
