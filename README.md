# Polinomi Multivariati


## 1 - Introduzione
Una delle prime e più importanti applicazioni dei calcolatori fu la manipolazione simbolica di operazioni
matematiche. In particolare, i sistemi noti come Computer Algebra Systems forniscono funzionalità per la manipolazione di polinomi multivariati.
Lo scopo di questo progetto è la costruzione di due librerie (in Prolog ed in Common Lisp) per la
manipolazione, per l’appunto – di polinomi multivariati.
<br><br>

### 1.1 - Polinomi Multivariati
Un polinomio multivariato è un’espressione matematica che contiene diverse variabili a cui possono essere
associati dei valori in un certo dominio. 
Due esempi sono (con le solite regole di associatività):

* _x_<sup>2</sup> + _y_<sup>2</sup>

* 42 × _x_ × _y_<sup>3</sup> - _z_<sup>2</sup> x _y_ × _w_ ×  _x_ - _x_<sup>3</sup> × _w_<sup>3</sup>

Il secondo polinomio viene normalmente riscritto in modo compatto come:<br><br>
42 _xy_<sup>3</sup> - _z_<sup>2</sup>_ywx_ - _x_<sup>3</sup>_w_<sup>3</sup> <br><br>
omettendo il simbolo di moltiplicazione ×.
<br>
Un polinomio è composto da monomi: i termini corrispondenti alle moltiplicazioni di coefficienti (elementi del dominio) e delle variabili (elevate a potenze). Nel secondo esempio qui sopra i monomi sono:

* 42 _xy_<sup>3</sup>

* -_z_<sup>2</sup> x _y_ × _w_ ×  _x_

* -_x_<sup>3</sup>_w_<sup>3</sup>

Notate anche che il polinomio qui sopra può anche essere scritto come <br><br>
-_w_<sup>3</sup>_x_<sup>3</sup> + 42 _xy_<sup>3</sup> - _wxyz_<sup>2</sup>
<br><br>
ovvero riordinando i monomi.
<br><br>

## 2 - Ordinamento di monomi e polinomi multivariati
Un polinomio univariato è normalmente scritto in ordine decrescente (o crescente) delle potenze della variabile. Ad esempio:<br><br>
_y_<sup>4</sup> - 3 _y_<sup>3</sup> - 42 _y_ + 123
<br><br>
I monomi ed i polinomi multivariati possono essere invece scritti e “ordinati” in molti modi diversi; ognuno di questi ordinamenti ha una sua funzione in Computer Algebra.
<br><br>

### 2.1 - Ordinamento di un monomio

Un monomio deve essere ordinato in ordine lessicografico crescente
delle variabili. Ovvero:
<br><br>
_y_<sup>42</sup>_x_<sup>4</sup>_sz_<sup>2</sup>_t_<sup>2</sup> ⇒ _st_<sup>2</sup>_x_<sup>4</sup>_y_<sup>42</sup>_z_<sup>2</sup>
<br><br>
Si noti come l’esponente non modifichi l’ordinamento.
<br><br>

### 2.2 - Ordinamento di un polinomio

Dato un insieme di monomi (ordinati), il polinomio risultante sarà ordinato prima in _ordine crescente del grado dei monomi_ con spareggi determinati dalle variabili (questa è la ragione per tener traccia del grado complessivo di un monomio). Ad esempio:
<br><br>
_y_<sup>4</sup>_zx_<sup>5</sup> - _yzr_ + _y_<sup>4</sup>_rz_<sup>5</sup> ⇒ - _ryz_ + _ry_<sup>4</sup>_z_<sup>5</sup> + _x_<sup>5</sup>_y_<sup>4</sup>_z_
<br><br>
L’ordinamento di due monomi con le stesse variabili va fatto in modo crescente rispetto alle combinazioni
variabile/esponente.

<br><br>

## 4 - "Parsing" di Polinomio
I predicati **as_monomial**, **as_polynomial** e le funzioni **as-monomial** e **as-polynomial** si preoccupano di
trasformare un monomio e un polinomio nella rappresentazione canonica interna. Il loro ruolo è quello di fare il _parsing_ di una rappresentazione superficiale di monomi e polinomi. Queste rappresentazioni sono diverse per **Prolog** e **Common Lisp**.