;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ac-nrepl
                      ace-jump-mode
                      ack-and-a-half
                      auto-complete
                      clojure-mode
                      clojure-test-mode
                      column-marker
                      color-theme-sanityinc-tomorrow
                      diminish
                      elisp-slime-nav
                      elscreen
                      evil
                      evil-leader
                      evil-nerd-commenter
                      fill-column-indicator
                      fiplr
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      framemove
                      goto-last-change
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      markdown-mode
                      midje-mode
                      nrepl
                      org
                      powerline
                      projectile ; Find file in project (ala CTRL-P).
                      rainbow-delimiters
                      smartparens
                      smex
                      undo-tree
                      yasnippet
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Packages not on melpa:
(add-to-list 'load-path "~/.emacs.d/plugins/evil-surround")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")

;;
;; General
;;

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(global-auto-revert-mode t) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(setq make-backup-files nil)
(setq auto-save-default nil)

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
  scroll-margin 5
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; The preference file for Emac's "Customize" system. `M-x customize` to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Colorscheme
(load-theme 'sanityinc-tomorrow-bright t)
(set-face-attribute 'default nil :family "Consolas" :height 150)

;; Whitespace
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     ;; (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace mode by default marks all whitespace. Show only tabs, trailing space, and trailing lines.
     (setq whitespace-style '(face empty trailing tabs tab-mark))))
;; NOTE(harry) Flip the following two settings for editing snippets
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (setq-default mode-require-final-newline nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)

(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!
; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
(global-visual-line-mode t)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example.If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

;;
;; Evil mode -- Vim keybindings for Emacs.
;;

(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader) ; Provide configuration functions for assigning actions to a Vim leader key.
(require 'evil-nerd-commenter)
(require 'goto-last-change)
(require 'surround)
(evil-mode t)
(global-evil-leader-mode)
(global-surround-mode 1)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'fiplr-find-file ;; 'projectile-find-file
  "eb" 'eval-buffer
  "es" 'eval-surrounding-sexp
  ; "v" is a mnemonic prefix for "view X".
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/emacs"))
  "vh" (lambda () (interactive) (find-file "~/workspace/hmp_repos/liftoff/haggler/src/haggler/handler.clj"))
  "vt" (lambda () (interactive) (find-file "~/workspace/hmp_repos/liftoff/zdocs/text_scratchpad.txt"))
  "vl" (lambda () (interactive) (find-file "~/.lein/profiles.clj")))

(eval-after-load 'evil
  '(progn
     (setq evil-leader/leader ",")
     (setq evil-default-cursor t)
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     (define-key evil-normal-state-map (kbd "M-,") nil)
     (define-key evil-normal-state-map (kbd "M-.") nil)))

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(defun backward-kill-line (arg)
  "Delete backward (Ctrl-u) as in Bash."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Enable Emacs/Bash insert-mode keybindings
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-h") 'backward-delete-char) ; C-h in Emacs is the prefix for help functions.

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;; Evil's window map is the set of keys which control window functions. All of its keys are prefixed with
;; <C-w>.
;; Undo the last change you made to your window configuration. Very handy as a method for temporarily
;; maximizing a window: first invoke delete-other-windows, and then invoke winner-undo..
(define-key evil-window-map (kbd "b") 'winner-undo)





;;
;; OS X keybindings minor mode. Make it so the OSX keybindings you're used to always work.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;

(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key osx-keys-minor-mode-map (kbd "M-w") 'vimlike-quit)
(define-key osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key osx-keys-minor-mode-map (kbd "M-n") 'new-frame)
(define-key osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)

(define-minor-mode osx-keys-minor-mode
  "A minor-mode for emulating osx keyboard shortcuts."
  t " osx" osx-keys-minor-mode-map)

(osx-keys-minor-mode t)

(defadvice load (after give-osx-keybindings-priority)
  "Try to ensure that osx keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'osx-keys-minor-mode))
      (let ((osx-keys (assq 'osx-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'osx-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist osx-keys))))
(ad-activate 'load)

; Closes the current elscreen, or if there's only one screen, use the ":q" Evil
; command. This simulates the ":q" behavior of Vim when used with tabs.
; http://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
   otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
        (one-window (one-window-p)))
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      (elscreen-kill)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-elscreen
      (evil-quit)
      nil))))

;;
;; Filename completions (CTRL-P / CMD+T)
;;
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(eval-after-load 'ido
  '(progn
     (setq ido-enable-flex-matching t)
     (setq ido-use-virtual-buffers t)
     (setq ido-everywhere t)))

;;
;; Org mode, for TODOs and note taking.
;;

(require 'org)
(require 'evil-org)
(eval-after-load 'org
  '(progn
     ; This enables "clean mode", such that sublists use whitespace for indentation ala markdown.
     (setq org-startup-indented t)))

;; Moves the current heading (and all of its children) into the matching parent note in the archive file.
;; I think this is the most sensible way to archive TODOs in org mode files.
;; http://orgmode.org/worg/org-hacks.html
(defadvice org-archive-subtree (around my-org-archive-subtree activate)
  (let ((org-archive-location
         (if (save-excursion (org-back-to-heading)
                             (> (org-outline-level) 1))
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    ad-do-it))

(setq org-default-notes-file "~/Dropbox/tasks.org")
(define-key org-mode-map "\C-cc" 'org-capture)

;;
;; Projectile (find file from the root of the current project).
;;

(projectile-global-mode)

;;
;; elscreen (tabs on the window).
;;

(elscreen-start)
(define-key evil-normal-state-map (kbd "M-}") 'elscreen-next)
(define-key evil-normal-state-map (kbd "M-{") 'elscreen-previous)
(define-key evil-normal-state-map (kbd "M-t") 'open-new-tab-with-current-buffer)

(defun open-new-tab-with-current-buffer ()
  (interactive)
  (elscreen-clone)
  (delete-other-windows))


;;
;; Snippets
;;

;; Ignore the default snippets that come with yasnippet. My own are all I need, and I don't want any
;; conflicts.
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)


;;
;; From Dmac - https://github.com/dmacdougall/dotfiles/blob/master/.emacs
;;

(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-cleanup nil)
(setq lazy-highlight-max-at-a-time nil)
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
(setq vc-follow-symlinks t)

(global-undo-tree-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
;; (line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "RET") 'comment-indent-new-line)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(eval-after-load 'paren
  '(setq show-paren-delay 0))
(show-paren-mode t)

;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))

(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(ac-linum-workaround)

(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function split-window-sensibly, but prefers horizontal splits over vertical splits."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
       ;; Split window horizontally.
       (with-selected-window window
         (split-window-right)))
  (and (window-splittable-p window)
       ;; Split window vertically.(column-marker-1 80)
       (with-selected-window window
         (split-window-below)))
  (and (eq window (frame-root-window (window-frame window)))
       (not (window-minibuffer-p window))
       ;; If WINDOW is the only window on its frame and is not the
       ;; minibuffer window, try to split it vertically disregarding
       ;; the value of `split-height-threshold'.
       (let ((split-height-threshold 0))
         (when (window-splittable-p window)
     (with-selected-window window
       (split-window-below))))))))

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

(add-hook 'clojure-mode-hook '(lambda () (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)))


;;
;; All new!
;;

;; Switch across both windows (i.e. panes/splits) and frames (i.e. OS windows)!
(require 'framemove)
(setq framemove-hook-into-windmove t)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)

(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map ",c " 'evilnc-comment-or-uncomment-lines)
     (define-key evil-visual-state-map ",c " 'evilnc-comment-operator)))

(evil-leader/set-key
  "|" (lambda () (interactive)(split-window-horizontally) (other-window 1))
  "\\" (lambda () (interactive)(split-window-horizontally) (other-window 1))
  "-" (lambda () (interactive)(split-window-vertically) (other-window 1))
  "a" 'projectile-ack
  "d" 'projectile-dired)

(require 'cl)
(dolist (i (number-sequence 1 9))
  (lexical-let ((tab-index (- i 1)))
    (global-set-key (kbd (concat "M-" (number-to-string i)))
                    (lambda () (interactive) (elscreen-goto tab-index)))))

(require 'smartparens)
(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem)
(setq sp-autoescape-string-quote nil)

;; fix the PATH variable - from http://clojure-doc.org/articles/tutorials/emacs.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(require 'powerline)
(powerline-default-theme)

(define-key osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-increase 0)))

(define-key evil-normal-state-map (kbd "s") 'newline-and-indent)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

(require 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; TODO(harry) I couldn't get this working
;; (require 'git-gutter-fringe+)
;; (setq git-gutter+-hide-gutter t)


;;
;; Dired related
;;

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; "go to dired, then call split-window-vertically, then go to another dired dir. Now, when you press C to
;; copy, the other dir in the split pane will be default destination. Same for R (rename; move)."
(setq dired-dwim-target t)

; Use same buffer for going into and up directories
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
 ))

(evil-define-key 'normal dired-mode-map "H" (lambda () (interactive) (find-alternate-file "..")))

;; tweak projectile to not us git ls-files
(require 'projectile)
(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  'none)

(eval-after-load 'fiplr
  '(setq fiplr-ignored-globs '((directories (".git" ".svn" "target" "log" ".sass-cache" "Build"))
                               (files (".#*" "*.so" ".DS_Store" ".class")))))

; Diminish modeline clutter - http://whattheemacsd.com/init.el-04.html
(require 'diminish)
;; TODO(harry) These calls should only be made after that mode is loaded.
;; (diminish 'clojure-test-mode)
;; (diminish 'nrepl-mode)
;; (diminish 'projectile-mode)
;; (diminish 'osx-keys-minor-mode)
;; (diminish 'undo-tree-mode)
;; (diminish 'eldoc-mode)
;; (diminish 'smartparens-mode)
;; (diminish 'auto-complete-mode)
;; (diminish 'global-whitespace-mode)

; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))


;;
;; Ruby related
;;

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))


;;
;; Clojure related
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(evil-leader/set-key-for-mode 'clojure-mode
  "eb" 'nrepl-load-current-buffer
  "es" 'nrepl-eval-expression-at-point
  "er" 'nrepl-eval-region
  "nj" 'nrepl-jack-in
  "nn" 'nrepl-set-ns
  ;; This command sets and pulls up the appropriate nREPL for the current buffer. Useful when you have
  ;; multiple REPLs going.
  "nb" 'nrepl-switch-to-repl-buffer)

(add-hook 'clojure-mode-hook 'clojure-test-mode)
(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map "\C-c\M-r" 'nrepl-switch-to-repl-buffer))

;; Clojure indentation rules
;; (add-hook 'clojure-mode-hook (lambda () (setq lisp-indent-offset 2)))
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (send-off 1)                                              ; Core
     (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (select 1) (insert 1) (update 1) (delete 1) (upsert 1)    ; Korma
     (clone-for 1)                                             ; Enlive
     (up 1) (down 1) (alter 1) (table 1)                       ; Lobos
     ))

;; Autocompletion in nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(eval-after-load 'auto-complete '(add-to-list 'ac-modes 'nrepl-mode))

(evil-define-key 'normal clojure-mode-map "K" 'nrepl-doc)
(evil-define-key 'normal clojure-mode-map "gf" 'nrepl-jump)
(put 'dired-find-alternate-file 'disabled nil)
