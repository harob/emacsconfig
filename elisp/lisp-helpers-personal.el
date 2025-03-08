;;
;; Utility functions for writing lisp in Emacs.
;;
(provide 'lisp-helpers-personal)

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/_p2-GXAANgw. Requires CL.
(defun partition (l n)
  "Return a list of L's consecutive sublists of length N."
  (cl-assert (zerop (mod (length l) n)))
  (cl-loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))
