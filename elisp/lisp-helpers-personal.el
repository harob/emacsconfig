;;; lisp-helpers-personal.el --- Utility functions for writing lisp in Emacs -*- lexical-binding: t; -*-
;;
;; Utility functions for writing lisp in Emacs.
;;

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/_p2-GXAANgw. Requires CL.
(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (cl-assert (zerop (mod (length l) n)))
  (cl-loop for l on l by #'(lambda (l) (nthcdr n l)) collect (cl-subseq l 0 n)))

(provide 'lisp-helpers-personal)
