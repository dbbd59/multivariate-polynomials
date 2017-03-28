
PROLOG


La libreria implementata permette di effettuare alcune operazioni standard per la manipolazione di polinomi e monomi.

I monomi vengono rappresentati nel seguente modo:

	m(Coefficient, TotalDegree, VarsPowers)

dove
	- Coefficient indica il coefficiente del monomio
	- TotalDegree indica il grado totale del monomio
	- VarsPowers è la lista che contiente i termini del polinomio rappesentati come v(Power, Symbol)


I polinomi sono invece rappresentati in questo modo:

	poly(Monomials)

dove Monomials è una lista di monomi





::::::::::::::::::::::::: Lista dei predicati implementati :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

- coefficients(Poly, Coefficients)
	vero quando Coefficients è una lista dei coefficienti di Poly	

- variables(Poly, Variables)
	vero quando Variables è una lista dei simboli di variabile che appaioni in Poly

- monomials(Poly, Monomials)
	vero quando Monomials è una lista ordinata dei monomi che appaioni in Poly

- maxdegree(Poly, Degree)
	vero quando Degree è il massimo grado dei monomi che appaiono in Poly

- mindegree(Poly, Degree)
	vero quando Degree è il minimo grado dei monomi che appaiono in Poly

- polyplus(Poly1, Poly2, Result)
	vero quando Result è il polinomio somma di Poly1 e Poly2

- polyminus(Poly1, Poly2, Result)
	vero quando Result è il polinomio differenza di Poly1 e Poly2

- polytimes(Poly1, Poly2, Result)
	vero quando Result è il polinomio risultante dalla moltiplicazione di Poly1 e Poly2

- as_monomial(Expression, Monomial)
	vero quando Monomial è il termine che rappresenta il monomio risultante dal "parsing" dell'espressione Expression

- as_polynomial(Expression, Polynomial)
	 vero quando Polynomial è il termine che rappresenta il polinomio risultante dal "parsing" dell'espressione Expression

- polyval(Polynomial, VariableValues, Value)
	vero quando Value contiene il valore del polinomio Polynomial nel punto n-dimensionale rappesentato dalla lista VariableValues

- pprint_polynomial(Polynomial)
	risulta vero dopo aver stampato sullo standard output una rappresentazione tradizionale del termine polinomio associato a Polynomial
