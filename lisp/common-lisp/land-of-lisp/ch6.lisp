;; Command-line interface

;; Standard output
(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test")) ;; Print every string with a line-break

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test")) ;; Print every string

;; Saying hello to the user
(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

;; Staring with print and read
(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))

(print 'foo) ;; Print a symbol, must be explicitly quoted
(print '#\a) ;; Print the character 'a'
(print '|This is a legal, case-sensitive Lisp symbol!|)

;; Reading and Printing Stuff the Way Humans like It

(princ '3) ;; Print it like a human ==> 3
(princ '#\a) ;; ==> Just 'a'

(progn (princ "This sentence will be interrupted")
       (princ #\newline) ;; Special character
       (princ "by an annoying newline character."))

(defun say-hello-like-a-human ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

;; Symmetry Between Code and Data in Lisp
(defparameter *foo* '(+ 1 2)) ;; Define a variable with data value

(eval *foo*) ;; Evaluate *foo* data as code

;; Game-REPL: see code for chapter 5

;; The Dangers of read and eval

