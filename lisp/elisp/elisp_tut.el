;;; package --- Summary
;;; Commentary:
;; This is a short introduction to Emacs Lisp.

;;; Code:
;; Print a message to the echo area
(message "This is printed to the echo area.")

;; Do some arithmetic with reverse polish notation
(+ 42 (/ 42 2))

;; Nil is an empty list
(equal nil '())

;; Use setq to assign a value to a symbol
(setq a-variable 42)
(setq a-string "my string")
(message "a-variable=%d,\na-string=%s" a-variable a-string) ; Print a formatted string to the echo area

;; Bind values to symbols for function scope with let
(let ((my-header "This is a header\n")
      (my-delimiter "----------------\n")
      (my-footer "This is a footer"))
  (message (concat my-header my-delimiter my-footer)))

;; Use defun to define a function
(defun my-function (first-argument second-argument)
  "This is the docstring for the function.
FIRST-ARGUMENT is a string.
SECOND-ARGUMENT is a string."
  (message (concat first-argument second-argument)))
(my-function "baby's first " "lisp tutorial")

;; Use interactive in a function definition to make the function callable with M-x
(defun my-interactive-function ()
  "This is the docstring for my interactive function.
It prints 'Hello Emacs' to the echo area."
  (interactive)
  (message "Hello Emacs!"))

;; Use the special if form as a control structure.
(if (= 4 5)
    (progn ; use progn to excuse multiple expressions
      (message "They are equal.")
      (message "I mean it!"))
  (message "They are not equal."))

;; Use the special when form
(when (<= 4 5)
  (message "They are equal.")) ; Evaluates if t

;; Use the special unless form
(unless (<= 4 5)
  (message "They are not equal.")) ; Evaluates if nil

;; Conditional functions
(null nil) ; -> t
(equal 5 5) ; -> t
(or (null nil) (null t)) ; -> t
(and (null nil) (null t)) ; -> nil
(stringp "A string") ; -> t
(numberp 32) ; -> t
(functionp 'my-function) ; -> t
(symbolp 'a-variable) ; -> t
(listp '(one two three)) ; -> t


;; Use the special cond form
(defun typeof (object)
  "Check the type of OBJECT and return its type as a string."
  (cond
   ((null object) "nil")
   ((numberp object) "number")
   ((stringp object) "string")
   ((functionp object) "function")
   ((symbolp object) "symbol")
   ((listp object) "listp")))

(typeof '(one two three))

;;; Loops in Lisp
;; The while loop
(defun print-0-to-9 ()
  "Print numbers 0 through 9."
  (let ((number 0))
    (while (< number 10)
      (message "%d" number)
      (setq number (+ number 1)))))

(print-0-to-9)

;; dotimes loop
(dotimes (iterator 10)
  (message "%d" iterator))

;;; Lists
(let ((my-list '(one two three)))
  (car my-list) ; head
  (cdr my-list) ; tail
  (setq my-list (cons 'a '(b c d))) ; head-insert
  )

(defun reverse-list (list-arg)
  "Reverses LIST-ARG."
  (unless (null list-arg)
    (cons (reverse-list (cdr list-arg)) (car list-arg))))

(reverse-list '(a b c d))

(dolist (head '(a b c))
  (message "%s" head))

(provide 'elisp_tut)
;;; elisp_tut.el ends here
