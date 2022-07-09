;; Empty equals false
(if '()
    'i-am-true
    'i-am-false) ;; '() evaluates to false


(if '(1)
    'i-am-true
    'i-am-false) ;; A non-empty list evaluates to true

;; Determining the length of a list with recursion
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

;; Conditionals
(if (= (+ 1 2) 3) ;; "if" is a special form with special privileges
    'yup
    'nope)

(if (oddp 5)
    'odd-number
    'even-number)

(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t) ;; progn returns only the last evaluation of a full expression
           'odd-number)
    'even-number)

;; when and unless (with implicit "progn")
(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number) ;; when T => only ODD-NUMBER is returned

(unless (oddp 4)
  (setf *number-was-odd* nil)
  'even-number) ;; unless T => only EVEN-NUMBER is return

;; The "cond" command
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnny))
        (t '(why you eat my pudding stranger ?)))) ;; Default case for any non-nil argument

;; Branching with case (only for symbol variables)
(defun pudding-eater (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger ?)))) ;; Default case

;; Comparing Stuff: eq equal and More
(defparameter *fruit* 'apple)

(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))

(equal 'apple 'apple) ;; ==> T, equal can handle isomoprhic data

(eql 3.4 3.4) ;; Like eq, but handles numbers and characters

(equalp "Bob Smith" "bob smith") ;; equalp, the sophisticated cousin of equal
