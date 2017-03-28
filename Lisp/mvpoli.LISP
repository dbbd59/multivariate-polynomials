

(defun is-monomial (m) 
  (and (listp m) 
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m)) 
             (vps (monomial-vars-and-powers m)) 
             ) 
         (and (integerp mtd) 
              (>= mtd 0) 
              (listp vps) 
              (every #'is-varpower vps)))))

(defun is-polynomial (p) 
  (and (listp p) 
       (eq 'poly (first p)) 
       (let ((ms (poly-monomials p))) 
         (and (listp ms) 
              (every #'is-monomial ms)))))
				
(defun is-varpower(vp) 
  (and (listp vp) 
       (eq 'v (first vp)) 
       (let ((p (varpower-power vp)) 
             (v (varpower-symbol vp))
             ) 
         (and (integerp p) 
              (>= p 0) 
              (symbolp v)))))

(defun varpower-power (exp)
  (cond
   ((null exp) nil)
   (t (second exp))))

(defun varpower-symbol (exp)
  (cond
   ((null exp) nil)
   (t (third exp))))

(defun monomial-total-degree (exp)
  (cond
   ((null exp) nil)
   (t (third exp))))

(defun monomial-vars-and-powers (exp)
  (cond
   ((null exp) nil)
   (t (fourth exp))))

(defun poly-monomials (exp)
  (cond
   ((null exp) nil)
   (t (second exp))))
				
(defun get-coeff (exp)
  (cond
   ((null exp) nil)
   ((numberp (car(cdr exp))) 
    (cond
     ((equal 0 (car(cdr exp))) nil)
     (t (car(cdr exp))))) 
   ((not (numberp (car (cdr exp)))) 1)))

(defun get-vars-pow (exp) 
  (cond
   ((null (car exp)) nil)
   ((and (listp (car exp)) (equal (third (car exp)) 0)) 
    (get-vars-pow (cdr exp)))
   ((numberp (car exp)) (get-vars-pow (cdr exp)))
   ((not (listp (car exp))) (cons (cons 'v (cons 1 (cons (car exp) nil))) 
                                  (get-vars-pow (cdr exp))))
   ((listp (car exp)) (cons (cons 'v (cons (third (car exp))
                                           (cons (second (car exp)) nil))) 
                            (get-vars-pow (cdr exp))))))

(defun get-tot-degree (var) 
  (cond 
   ((null var) 0)
   ((null (cdr var)) (second (car var)))
   (t (+ (second (car var)) (get-tot-degree (cdr var))))))

(defun sort-monomial (exp) 
  (cond
   ((null exp) nil)
   (t (sort (varpowers exp) #'string-lessp :key #'third) exp)))

(defun as-monomial (exp)
  (cond
   ((null exp) nil)
   ((numberp exp) (cons 'm (cons exp (cons 0 (cons nil nil)))))
   (t(simplify-monomial(sort-monomial (cons 'm (cons (get-coeff exp ) 
                                   (cons (get-tot-degree
                                          (get-vars-pow (cdr exp)))
                                         (cons (get-vars-pow (cdr exp))
                                               nil)))))))))

(defun sort-polynomial (exp) 
  (cond
   ((null exp) nil)
   (t (sort (poly-monomials exp) #'sort-mega) (sort (poly-monomials exp)
                                                    #'< :key #'third) exp)))

(defun sort-mega (a b)
  (cond
   (t (sort-mega-helper (varpowers a) (varpowers b)))))

(defun sort-mega-helper (a b)
  (cond 
   ((null a) (not (null b)))
   ((null b) nil)
   ((and (equal (third(first a)) (third(first b))) (equal (second(first a))
                                                          (second(first b)))) 
    (sort-mega-helper (rest a) (rest b)))
   (t (string< (third(first a)) (third(first b))))))

(defun as-polynomial (exp)
  (cond
   ((null exp) nil)
   ((numberp exp) (list 'poly (list nil)))
   (t (simplify-polynomial(sort-polynomial (cons 'poly (cons 
                                                        (as-polynomial-helper 
                                                         (cdr exp)) nil)))))))

(defun as-polynomial-helper (exp)
  (cond
   ((null exp) nil)
   (t (cons (as-monomial (car exp))(as-polynomial-helper (cdr exp))))))

(defun pprint-polynomial (exp)
  (cond
   ((null exp) nil)
   ((not (is-polynomial exp)) (pprint-polynomial (as-polynomial exp)))
   (t (simplify-polynomial(sort-polynomial exp))
      (pprint-polynomial-helper-first (car (cdr exp))))))

(defun pprint-polynomial-helper-first (exp)
  (cond 
   ((null exp) nil)
   (t (pprint-monomial-first (car exp)) (pprint-polynomial-helper (cdr exp)))))

(defun pprint-polynomial-helper (exp)
  (cond 
   ((null exp) nil)
   (t (pprint-monomial (car exp)) (pprint-polynomial-helper (cdr exp)))))

(defun pprint-monomial-first (exp)
  (cond
   ((null exp) nil)
   ((null (fourth exp)) (write (second exp))(princ " "))
   ((equal (second exp) 1) (pprint-monomial-variables (fourth exp)))
   (t (write (second exp)) (princ " ") 
      (pprint-monomial-variables (fourth exp)))))

(defun pprint-monomial (exp)
  (cond
   ((null exp) nil)
   ((null (fourth exp)) (princ "+ ")(write (second exp))(princ " "))
   ((equal (second exp) 1) (princ "+ ")
    (pprint-monomial-variables (fourth exp)))
   ((< (second exp) 0) (write (second exp))(princ " ")
    (pprint-monomial-variables (fourth exp)))
   (t (princ "+ ")(write (second exp)) (princ " ") 
      (pprint-monomial-variables (fourth exp)))))

(defun pprint-monomial-variables (exp)
  (cond 
   ((null exp) nil)
   ((null (cdr exp)) 
    (cond
     ((equal (second (first exp)) 1) (write (third (first exp))) (princ " "))
     (t (write (third (first exp))) (princ "^") (write (second (first exp)))
        (princ " "))))
   (t (cond
       ((equal (second (first exp)) 1) (write (third (first exp))) (princ " ") 
        (pprint-monomial-variables (cdr exp)))
       (t (write (third (first exp))) (princ "^") (write (second (first exp)))
          (princ " ") (pprint-monomial-variables (cdr exp)))))))

(defun varpowers (exp)
  (cond
   ((null exp) nil)
   ((not(is-monomial exp)) (varpowers (as-monomial exp)))
   (t (fourth exp))))

(defun vars-of (exp)
  (cond
   ((null exp) nil)
   ((not(is-monomial exp)) (vars-of (as-monomial exp)))
   (t (vars-of-helper (varpowers exp)))))

(defun vars-of-helper (exp)
  (cond
   ((null exp) nil)
   (t (cons (third (car exp)) (vars-of-helper (cdr exp))))))

(defun monomial-degree (exp)
  (cond
   ((null exp) nil)
   ((not(is-monomial exp)) (monomial-degree (as-monomial exp)))
   (t (third exp))))

(defun monomial-coefficient (exp)
  (cond
   ((null exp) nil)
   ((not(is-monomial exp)) (monomial-coefficient (as-monomial exp)))
   (t (second exp))))
   
(defun monomials (exp)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (monomials (as-polynomial exp)))
   (t (sort-polynomial exp)(second exp))))

(defun coefficients (exp)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (coefficients (as-polynomial exp)))
   (t (coefficients-helper (monomials exp)))))

(defun coefficients-helper (exp)
  (cond
   ((null exp) nil)
	(t (cons (second (car exp)) (coefficients-helper (cdr exp))))
	))

(defun variables (exp)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (variables (as-polynomial exp)))
   (t (sort (remove-duplicates (flatten (variables-helper (monomials exp))))
            #'string-lessp))))

(defun variables-helper (exp)
  (cond
   ((null exp) nil)
   (t (cons (vars-of (car exp)) (variables-helper (cdr exp))))))

(defun flatten (l)
  (cond 
   ((null l) nil)
   ((atom l) (list l))
   (t (append (flatten (first l)) 
              (flatten (rest l))))))

(defun maxdegree (exp)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (maxdegree (as-polynomial exp)))
   (t (maximum (maxdegree-helper (monomials exp))))))

(defun maxdegree-helper (exp)
  (cond
   ((null exp) nil)
   (t (cons (monomial-degree (car exp)) (maxdegree-helper (cdr exp))))))

(defun mindegree (exp)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (mindegree (as-polynomial exp)))
   (t (minimum (mindegree-helper (monomials exp))))))

(defun mindegree-helper (exp)
  (cond
   ((null exp) nil)
   (t (cons (monomial-degree (car exp)) (mindegree-helper (cdr exp))))))

(defun maximum (l)
  (if (= (length l) 1)
      (car l)
      (if (> (car l) (maximum (cdr l)))
          (car l)
          (maximum (cdr l)))))
  
(defun minimum (l)
  (if (= (length l) 1)
      (car l)
      (if (< (car l) (minimum (cdr l)))
          (car l)
          (minimum (cdr l)))))
   
(defun polyval (exp l)
  (cond
   ((null exp) nil)
   ((not(is-polynomial exp)) (polyval (as-polynomial exp) l))
   (t (polyval-helper (monomials exp) l (variables exp)))))
  
(defun polyval-helper (exp l v)
  (cond
   ((null exp) nil)
   ((null (cdr exp))  (calcola (car exp) l v))
   (t (+ (calcola (car exp) l v) (polyval-helper (cdr exp) l v))))) 

(defun calcola (exp l v)
  (cond
   ((null exp) nil)
   (t (* (calcola-expt (varpowers exp) l v) (second exp)))))

(defun calcola-expt (exp l v)
  (cond
   ((null exp) 1)
   (t (* (expt (get-val-var (third (car exp)) l v) (second (car exp)))
         (calcola-expt (cdr exp) l v)))))

(defun get-val-var (n l v)
  (cond
   ((null n) nil)
   (t (nth (position n v) l))))

(defun simplify-monomial (exp) 
  (cond
   ((null exp) nil)
   (t (list 'm (second exp) (third exp) 
            (simplify-monomial-helper(varpowers exp))))))

(defun simplify-monomial-helper (exp) 
  (cond
   ((null (cdr exp)) exp)
   ((not (equal (third (first exp)) (third (second exp)))) 
    (cons (first exp)(simplify-monomial-helper (cdr exp))))
   (t (simplify-monomial-helper (cons (cons 'v 
                                            (cons (+ (second (first exp))
                                                     (second (second exp))) 
                                                  (cons (third (first exp))
                                                        nil))) 
                                      (cdr (cdr exp)))))))

(defun simplify-polynomial (exp)
  (cond
   ((null exp) nil)
   (t (list 'poly (simplify-polynomial-verify
                   (simplify-polynomial-helper(monomials exp)))))))

(defun simplify-polynomial-helper (exp) 
  (cond
   ((null (cdr exp)) exp)
   ((not (equal (varpowers (first exp)) (varpowers (second exp))))
    (cons (first exp) (simplify-polynomial-helper (cdr exp))))
   (t (simplify-polynomial-helper 
       (cons (cons 'm 
                   (cons (+ (second (first exp))(second (second exp)))
                         (cons (third (first exp))
                               (cons (varpowers (second exp))
                                     nil))))
             (cdr (cdr exp)))))))

(defun simplify-polynomial-verify (exp)
  (cond
   ((null exp) nil)
   ((not (equal (second (first exp)) 0)) (append (list (first exp))
                                                 (simplify-polynomial-verify 
                                                  (cdr exp))))
   (t (simplify-polynomial-verify (cdr exp)))))
 
(defun polyplus (poly1 poly2)
  (cond
   ((null poly1) poly2)
   ((null poly2) poly1)
   ((and (not(is-polynomial poly1))(not(is-polynomial poly2)))
    (polyplus (as-polynomial poly1)(as-polynomial poly2)))
   ((not(is-polynomial poly1)) (polyplus (as-polynomial poly1) poly2))
   ((not(is-polynomial poly2)) (polyplus poly1 (as-polynomial poly2)))
   (t (simplify-polynomial(sort-polynomial (list 'poly (append 
                                                        (monomials poly1)
                                                        (monomials poly2))))))))

(defun polyminus (poly1 poly2)
  (cond
   ((null poly1) poly2)
   ((null poly2) poly1)
   ((and (not(is-polynomial poly1))(not(is-polynomial poly2))) 
    (polyminus (as-polynomial poly1)(as-polynomial poly2)))
   ((not(is-polynomial poly1)) (polyminus (as-polynomial poly1) poly2))
   ((not(is-polynomial poly2)) (polyminus poly1 (as-polynomial poly2)))
   (t (simplify-polynomial(sort-polynomial (list 'poly (append
                                                        (monomials poly1) 
                                                        (polytimes-helper 
                                                         (monomials poly2)
                                                         '(M -1 0 NIL)))))))))

(defun polytimes (poly1 poly2)
  (cond 
   ((or (null poly1) (null poly2)(and (listp poly1)(eq 'nil (second poly1)))
        (and (listp poly2)(eq 'nil (second poly2)))(and (numberp poly1)
                                                        (= 0 poly1))
        (and (numberp poly2)(= 0 poly2)))(cons 'poly (cons nil nil)))
   ((and (not (is-polynomial poly1))(not (is-polynomial poly2)))
    (let ((p1 (as-polynomial poly1))(p2 (as-polynomial poly2)))
      (let ((ris (times p1 p2)))
        (cond 
         ((null ris) nil)
         ((listp ris) (simplify-polynomial(sort-polynomial 
                                           (list 'poly 
                                                 (monomials 
                                                  (list 'poly ris))))))))))
   ((and (is-polynomial poly1)(not (is-polynomial poly2)))
    (let ((p2 (as-polynomial poly2)))
      (let ((ris (times poly1 p2)))
        (cond 
         ((null ris) nil)((listp ris)
                          (simplify-polynomial(sort-polynomial
                                               (list 'poly 
                                                     (monomials 
                                                      (list 'poly ris))))))))))
   ((and (not (is-polynomial poly1))(is-polynomial poly2))
    (let ((p1 (as-polynomial poly1)))
      (let ((ris (times p1 poly2)))
        (cond 
         ((null ris) nil)((listp ris)
                          (simplify-polynomial(sort-polynomial
                                               (list 'poly 
                                                     (monomials 
                                                      (list 'poly ris))))))))))
   ((and (is-polynomial poly1)(is-polynomial poly2))
    (let ((ris (times poly1 poly2)))
      (cond
       ((null ris)nil)((listp ris)
                       (simplify-polynomial(sort-polynomial
                                            (list 'poly 
                                                  (monomials 
                                                   (list 'poly ris)))))))))
   (T nil)))

(defun times (poly1 poly2)
  (cond 
   ((or (and (listp poly1)(null (car poly1)))(and (listp poly2)
                                                  (null (car poly2)))) nil)
   ((eq 'poly (car poly1))(times (car (cdr poly1)) poly2))
   ((eq 'poly (car poly2))(times poly1 (car (cdr poly2))))
   ((is-monomial (car poly1))(append (pol-mon-times poly2 (car poly1))
                                     (times (cdr poly1) poly2)))
   (T nil)))
 
(defun pol-mon-times (poly monomial)
  (cond 
   ((null (car poly)) nil)
   ((eq 'poly (car poly))(pol-mon-times (car (cdr poly)) monomial))
   ((is-monomial (car poly))(cons (monotimes (car poly) monomial)
                                  (pol-mon-times (cdr poly) monomial)))
   ((is-monomial poly)(cons (monotimes poly monomial) nil))))

(defun monotimes (monomial1 monomial2)
  (cond
   ((or (null monomial1) (null monomial2))nil)
   ((and (is-monomial monomial1)(is-monomial monomial2))
    (simplify-monomial(cons 'm (cons (* (second monomial1)(second monomial2))
                   (cons (+ (third monomial1)(third monomial2))
                         (list (union (fourth monomial1)
                                      (fourth monomial2))))))))))

   