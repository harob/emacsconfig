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

(evil-define-key 'normal clojure-mode-map "gf" 'cider-jump)
(evil-define-key 'normal clojure-mode-map "gb" 'cider-jump-back)

(dolist (state '(normal insert))
  (evil-define-key state clojure-mode-map
    (kbd "M-h") 'shift-sexp-backward
    (kbd "M-l") 'shift-sexp-forward
    (kbd "M-H") 'sp-forward-slurp-sexp
    (kbd "M-L") 'sp-forward-barf-sexp))

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
;; (setq cider-repl-pop-to-buffer-on-connect nil)

;; Don't ask confirmation for closing any open nrepl connections when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(evil-define-operator evil-cider-eval (beg end)
  "Evaluate the text region moved over by an evil motion."
  (cider-eval-region beg end))

;; Eval a paragraph. This is different from eval-surrounding-sexp in that it will eval multiple adjacent
;; s-expressions which are not separated by a new line. It's equivalent to wrapping the expressions in a do.
(defun cider-eval-paragraph (beg end)
  (interactive "r")
  (let ((region (evil-a-paragraph)))
    (evil-cider-eval (first region) (second region))))

(defun cider-show-cider-buffer ()
  "Shows the nrepl buffer, but does not focus it."
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(defun cider-clear-buffer-inside-cider-buffer ()
  "Switches to the cider buffer, clears it, and refocused to the original buffer."
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (cider-clear-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(defun my-cider-restart-nrepl ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (let ((repl-buffer (nrepl-connection-for-buffer (current-buffer))))
    (util/without-confirmation (lambda ()
                            (when (not (stringp repl-buffer))
                              (nrepl-close repl-buffer))
                            (cider-jack-in nil)))))

(defun my-cider-make-connection-buffer-the-current-connection (connection-buffer)
  (cons connection-buffer (delq connection-buffer nrepl-connection-list)))

(defun with-nrepl-connection-of-current-buffer (f)
  (let ((result (nrepl-connection-for-buffer (current-buffer))))
    (if (stringp result)
        (message result)
      (progn
        (my-cider-make-connection-buffer-the-current-connection result)
        (funcall f)))))

;; Based on `cider-switch-to-relevant-repl-buffer` in cider.el.
;; NOTE(philc): I think this needs further tuning. It doesn't work in all circumstances.
(defun nrepl-connection-for-buffer (buffer)
  "Returns either the corresponding nrepl buffer for the given buffer, or a string error message."
  (if (not (cider-connected-p))
      "No active nREPL connection."
    (let ((project-directory (nrepl-project-directory-for (nrepl-current-dir))))
      (if (not project-directory)
          "No project directory found."
        (let ((buf (cond
                    ;; I'm special casing shared_lib et al so that I can eval files from that project against
                    ;; the most recent repl.
                    ((or (search "shared_lib" project-directory)
                         (search "ml_lib" project-directory))
                     (car nrepl-connection-list))
                    (project-directory
                     (or (car (-filter
                               (lambda (conn)
                                 (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                                        nrepl-project-dir)))
                                   (when conn-proj-dir
                                     (equal (file-truename project-directory)
                                            (file-truename conn-proj-dir)))))
                               nrepl-connection-list))
                         (car nrepl-connection-list))))))
          (if buf
              (get-buffer buf)
            "No relevant nREPL connection found."))))))

(defun my-cider-eval-and-print-to-repl (form)
  "Wraps the form in 'print' and evaluates the expression."
  ;; Cider has cider-interactive-eval-to-repl, but it prints the results of expressions to random places in
  ;; the repl buffer.
  (let* (;; NOTE(philc): I have pprint aliased into clojure.core as >pprint for all of my lein projects. I've
         ;; done this through ~/.lein/profiles.clj. Assuming you don't, you can just use println as the
         ;; print-fn.
         ;; (print-fn ">pprint")
         (print-fn "println")
         (form (concat "(" print-fn form ")")))
    (cider-interactive-eval form)))

;; From http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
(defun my-cider-eval-current-sexp-in-repl ()
  (interactive)
  ;; Update the REPL namespace first if necessary:
  (unless (equal (cider-current-ns) (with-current-buffer (cider-current-repl-buffer) nrepl-buffer-ns))
    (cider-repl-set-ns (cider-current-ns)))
  (let ((form (current-sexp)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
           (setq form (replace-match "" t t form)))
    (set-buffer (cider-get-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(require 'cider-mode)
(define-key cider-mode-map
            (kbd "C-;") 'cider-eval-expression-at-point-in-repl)

(evil-leader/set-key-for-mode 'clojure-mode
  "eap" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-paragraph))
  "ek" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-find-and-clear-repl-buffer))
  "eb" (lambda ()
         (interactive)
         (save-buffer)
         ;; Note that I actually use cider-load-file here, not cider-eval-buffer, because it gives useful line
         ;; numbers on exceptions.
         (with-nrepl-connection-of-current-buffer 'cider-load-buffer))
  ;; cider-restart-nrepl is more handy than cider-jack-in, because it doesn't leave existing repls running.
  "en" 'my-cider-restart-nrepl
  ;; This function actually CPs the current s-expr into the REPL, rather the just printing the result:
  ;; TODO(harry) Port the other similar functions to have the same behavior
  "es" 'my-cider-eval-current-sexp-in-repl
  "ex" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-defun-at-point))
  "er" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-region))
  "nj" 'cider-jack-in
  "nn" 'cider-repl-set-ns
  ;; This command sets and pulls up the appropriate nREPL for the current buffer. Useful when you have
  ;; multiple REPLs going.
  "nb" 'cider-switch-to-repl-buffer
  "nt" 'cider-toggle-trace
  "nc" 'cider-find-and-clear-repl-buffer
  )

;; NOTE(harry) These are the simpler commands I had before:
;; (evil-leader/set-key-for-mode 'clojure-mode
;;   "eb" 'cider-load-current-buffer
;;   "es" 'cider-eval-expression-at-point
;;   "er" 'cider-eval-region
;;   "nj" 'cider-jack-in
;;   "nn" 'cider-repl-set-ns
;;   ;; This command sets and pulls up the appropriate nREPL for the current buffer. Useful when you have
;;   ;; multiple REPLs going.
;;   "nb" 'cider-switch-to-repl-buffer
;;   "nt" 'cider-toggle-trace)

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


(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (setq cider-repl-popup-stacktraces t)
(setq cider-repl-print-length 100)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-result-prefix ";; => ")

;; ;; Autocompletion in nrepl
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'auto-complete-mode)
(eval-after-load 'auto-complete '(add-to-list 'ac-modes 'cider-mode))
;; TODO: Replace with:
;; (require 'ac-cider-compliment)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-compliment-repl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes cider-mode))


;;
;; cljfmt -- automatic formatting of Clojure code. This configuration is Liftoff-specific.
;;

(load "$REPOS/liftoff/exp/emacs/cljfmt.el")

;; Note that `cljfmt-before-save` triggers this save-hook for some reason, so we lock on clj-in-progress to
;; to protect from infinite recurision.
(setq cljfmt-in-progress nil)
(defun cljfmt-before-save-mutually-exclusive ()
 (interactive)
 (when (and (eq major-mode 'clojure-mode)
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
