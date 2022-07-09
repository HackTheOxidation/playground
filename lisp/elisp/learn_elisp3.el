;;; package --- Summary
;;; Commentary:
;; This file contains code examples from System Crafters episode 3 on Emacs Lisp.

;;; Code:

;; Y and Z are optional arguments
(defun multiply-maybe (x &optional y z)
  "Multiply X with maybe Y and/or Z."
  (* x
     (or y 1)
     (or z 1)))

;; OPERANDS is a list of rest arguments
(defun multiply-many (x &rest operands)
  "Multiply X with OPERANDS."
  (dolist (operand operands)
    (when operand
      (setq x (* x operand)))))

(provide 'learn_elisp3)
;;; learn_elisp3 ends here
