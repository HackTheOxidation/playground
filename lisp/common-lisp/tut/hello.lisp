(defun hello ()
  (write-line "What is your name?")
  (let ((name (read-line)))
    (format t "Hello, ~A.~%" name)))

(defun my-length (list)
  (if list (1+ (my-length (cdr list)))
      0))

(defun square (n)
  (* n n))

(defparameter *fruits* '(apples bananas oranges))

(defun select-last (my-list)
  (if (eq (my-length (my-list)) 1)
      (my-list)
      (select-last (cdr my-list))))


