;;
;; Clojure
;;
;; Docs:
;; https://github.com/clojure-emacs/cider
;; http://clojure-doc.org/articles/tutorials/emacs.html

(provide 'clojure-mode-personal)

(defun setup-clojure-buffer ()
  ;; Count hyphens, etc. as word characters in lisps
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)

  ;; Comment lines using only one semi-colon instead of two.
  (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
  (setq comment-add 0))

(add-hook 'clojure-mode-hook 'setup-clojure-buffer)

(evil-define-key 'normal clojure-mode-map "K"
  (lambda () (interactive) (util/preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(dolist (state '(normal insert))
  (evil-define-key state clojure-mode-map
    (kbd "M-h") 'shift-sexp-backward
    (kbd "M-l") 'shift-sexp-forward))

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; From http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
(defun my-cider-eval-current-sexp-in-repl ()
  (interactive)
  ;; Update the REPL namespace first if necessary:
  (unless (equal (cider-current-ns) (with-current-buffer (cider-current-repl-buffer) cider-buffer-ns))
    (cider-repl-set-ns (cider-current-ns)))
  (let ((form (current-sexp)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
           (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(require 'cider-mode)

(evil-leader/set-key-for-mode 'clojure-mode
  "nj" 'cider-jack-in
  "nn" 'cider-repl-set-ns
  "nb" 'cider-switch-to-repl-buffer
  "nc" 'cider-find-and-clear-repl-output
  "eb" 'cider-load-buffer
  ;; This function actually CPs the current s-expr into the REPL, rather the just printing the result:
  ;; TODO(harry) Port the other similar functions to have the same behavior
  "es" 'my-cider-eval-current-sexp-in-repl
  )

(which-key-add-major-mode-key-based-replacements 'clojure-mode
  "SPC n" "Cider")

;; Clojure indentation rules
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (assoc 1) (-> 0) (->> 0) (cond-> 0) (cond->> 0)                   ; Override defaults
     (send-off 1) (cli 1) (go-loop 1)                                  ; Core
     (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (OPTIONS 2)
     (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
     (values 1) (delete 1) (upsert 1) (subselect 1)
     (clone-for 1)                                                     ; Enlive
     (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
     (checker 1)                                                       ; Midje
     (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
     (url-of-form 1) (when-let* 1)                                     ; Personal
     ))

(defun lisp-indent-line-single-semicolon-fix (&optional whole-exp)
  "Identical to the built-in function lisp-indent-line,
but doesn't treat single semicolons as right-hand-side comments."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-result-prefix ";; => ")
(setq cider-prompt-for-symbol nil)
(setq cider-save-file-on-load t)

;; Autocompletion in nrepl

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(setq company-idle-delay nil) ; never start completions automatically
(eval-after-load 'company
  '(progn
     ; FIXME: Autocomplete is broken currently:
     ; completion--some: Invalid function: (dict (thread-first (\` ("op" "complete" "ns" (\, (cider-current-ns)) "symbol" (\, str) "context" (\, context))) (cider-nrepl-send-sync-request nil (quote abort-on-input)))) [2 times]
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))


;;
;; cljfmt -- automatic formatting of Clojure code. This configuration is Liftoff-specific.
;;

(defconst cljfmt-accessible (getenv "REPOS"))
(if cljfmt-accessible
  (load "$REPOS/liftoff/exp/emacs/cljfmt.el")
  (message "REPOS environment variable is not defined. Not loading cljfmt."))

;; `cljfmt-before-save` triggers this save-hook for some reason, so we lock on clj-in-progress to to protect
;; from infinite recurision:
(setq cljfmt-in-progress nil)
(defun cljfmt-before-save-mutually-exclusive ()
 (interactive)
 (when (and cljfmt-accessible
            (eq major-mode 'clojure-mode)
            (not cljfmt-in-progress))
   (setq cljfmt-in-progress 't)
   (cljfmt)
   (setq cljfmt-in-progress nil)))

(setq cljfmt-show-errors nil)

;; TODO(harry) The before-save-hook fires every time cider looks up the docstring for a variable, which is
;; all the time in normal mode. As as short term fix I'm only running cljfmt when I explicitly save with M-s:
(add-hook 'before-explicit-save-hook 'cljfmt-before-save-mutually-exclusive nil)
;; Run this again after save so we see any formatting error messages in the Emacs echo area,
;; because they get cloberred by Emacs's "Wrote [file]" message.
(add-hook 'after-explicit-save-hook 'cljfmt-before-save-mutually-exclusive nil)
