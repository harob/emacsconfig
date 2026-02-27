;;; emacs-utils.el --- Lisp utility functions for working with text in Emacs -*- lexical-binding: t; -*-
;;
;; Lisp utility functions to serve as building blocks for working with text in Emacs.
;;

(require 'lisp-helpers-personal)

(defmacro defcmd (name &rest body)
  "Define an interactive command NAME with BODY. Useful for one-line definitions."
  (declare (indent defun))
  `(defun ,name () (interactive) ,@body))

(defun util/define-keys (keymap &rest key-and-fn-pairs)
  "Like define-key, but takes a variable number of arguments -- two per key binding pair."
  (dolist (pair (partition key-and-fn-pairs 2))
    (define-key keymap (first pair) (second pair))))

(defun util/save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun util/without-confirmation (fn)
  "Applies the given fn but skips any confirmation prompts invoked via yes-or-no-p."
  ;; Taken from https://www.emacswiki.org/emacs/YesOrNoP.
  (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
            ((symbol-function 'yes-or-no-p) #'always-yes))
    (apply fn args)))

(defun util/preserve-selected-window (f)
  "Runs the given function and then restores focus to the original window. Useful when you want to invoke
   a function (like showing documentation) but don't want to keep editing your current buffer."
  (lexical-let ((f f))
    (let ((original-window (selected-window)))
      (funcall f)
      (select-window original-window))))

(provide 'emacs-utils)
