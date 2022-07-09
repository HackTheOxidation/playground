;; Lisp Syntax example
(defun square (n)
  (* n n))

;; Symbols (and the eq function)
(eq 'fooo 'FoOo) ;; Return "T" because Lisp is not case-sensitive

;; Type "poisoning"
(+ 1 1.0) ;; Returns "2.0"

;; Lisp is good with numbers
(expt 53 53) ;; Doesn't choke like some other languages O.O

;; Lisp handles fractions, i.e. Lisp is very Rational
(/ 4 6) ;; Returns "2/3"

;; Strings are no match either!!
(princ "Tutti Frutti")

;; Avoid being tagged, escape the quotes!
(princ "He yelled \"Stop that thief!\" from the busy street.")

;; Cons cells
(cons 'chicken 'cat) ;; Returns "(CHICKEN . CAT)"

(cons 'chicken 'nil) ;; Returns "(CHICKEN)", because "nil" is the empty list

(cons 'chicken ()) ;; Identical to the above

;; Consing cons cells
(cons 'pork '(beef chicken))

(cons 'beef (cons 'chicken ()))

(cons 'pork (cons 'beef (cons 'chicken ())))

;; car and cdr
(car '(pork beef chicken)) ;; Select the first element and discard the rest

(cdr '(pork beef chicken)) ;; Discard the first element and return the rest

(car (cdr '(pork beef chicken))) ;; Combine to select different elements of a list

(cadr '(pork beef chicken)) ;; Same as the above, performs cdr then car (right to left)

;; The convenient list function
(list 'pork 'beef 'chicken)

;; Nested lists
(car '((peas carrots tomatoes) (pork beef chicken)))

(cdar '((peas carrots tomatoes) (pork beef chicken)))
