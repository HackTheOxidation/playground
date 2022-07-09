;; Lambda: a Function so important it deserves its own chapter

;; What lambda does

(defun half (n) ;; Named function without lambda
  (/ n 2))

(lambda (n) (/ n 2)) ;; Anonymous function with lambda

(mapcar (lambda (n) (/ n 2)) '(2 4 6)) ;; Parsing a lambda function as an argument

;; Lambda in Lisp is derived from Lambda Calculus and is the only fundamental command

