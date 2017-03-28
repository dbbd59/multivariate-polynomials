
LISP


La libreria implementata permette di effettuare alcune operazioni standard per la manipolazione di polinomi e monomi.

I monomi vengono rappresentati nel seguente modo:

	(m coefficient total-degree vars-n-powers)

dove
	- coefficient indica il coefficiente del monomio
	- total-degree indica il grado totale del monomio
	- vars-n-powers è la lista che contiente i termini del polinomio rappesentati come (v power var-symbol)


I polinomi sono invece rappresentati in questo modo:

	(poly monomials)

dove monomials è una lista di monomi



::::::::::::::::::::::::: Lista delle funzioni implementate :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

- varpowers Monomial -> VP-list
	Data una struttura Monomial, ritorna la lista di varpowers VP-list

- var-of Monomial -> Variable
	Data una struttura Monomial, ritorna la variabile Variable

- monomial-degree Monomial -> TotalDegree
	Data una struttura Monomial, ritorna il suo grado totale TotalDegree

- monomial-coefficient Monomial -> Coefficient
	Data una struttura Monomial, ritorna il suo coeciente Coefficient

- coefficients Poly -> Coefficients
	Ritorna una lista Coefficcients dei coefficienti di Poly

- variables Poly -> Variables
	Ritorna una lista Variables dei simboli di variabile che appaiono in Poly

- monomials Poly -> Monomials
	Ritorna la lista dei monomi che appaiono in Poly

- maxdegree Poly -> Degree
	Ritorna il massimo grado dei monomi che appaiono in Poly

- mindegree Poly -> Degree
	Ritorna il minimo grado dei monomi che appaiono in Poly

- polyplus Poly1 Poly2 -> Result
	Ritorna il polinomio somma di Poly1 e Poly2

- polyminus Poly1 Poly2 -> Result
	Ritorna il polinomio dierenza di Poly1 e Poly2

- polytimes Poly1 Poly2 -> Result
	Ritorna il polinomio risultante dalla moltiplicazione di Poly1 e Poly2

- as-monomial Expression -> Monomial
	Ritorna la struttura dati che rappresenta il monomio risultante dal "parsing" dell'espressione Expression

- as-polynomial Expression -> Polynomial
	Ritorna la struttura dati che rappresenta il monomio risultante dal "parsing" dell'espressione Expression

- polyval Polynomial VariableValues -> Value
	Restituisce il valore Value del polinomio Polynomial nel punto n-dimensionale rappresentato dalla lista VariableValues

- pprint-polynomial Polynomial -> NIL
	Ritorna NIL dopo aver stampato sullo standard output una rappresentazione tradizionale del termine polinomio associato a Polynomial