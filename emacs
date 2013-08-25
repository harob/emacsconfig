;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      elscreen
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx ; Fuzzy matching for ido, which improves the UX of Projectile.
                      goto-last-change
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      markdown-mode
                      midje-mode
                      projectile ; Find file in project (ala CTRL-P).
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(load-theme 'tangotango t) ; A reasonable color scheme which lives in my .emacs.d.
(set-face-attribute 'default nil :family "Consolas" :height 150)

;; Whitespace
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace mode by default marks all whitespace. Show only tabs, trailing space, and trailing lines.
     (setq whitespace-style '(face empty trailing tabs tab-mark lines-tail))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
(evil-mode t)
(global-evil-leader-mode)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'projectile-find-file
  "eb" 'eval-buffer
  "es" 'eval-last-sexp
  "ex" 'eval-surrounding-sexp
  ; "v" is a mnemonic prefix for "view X".
  "vo" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs")))

(eval-after-load 'evil
  '(progn (setq evil-leader/leader ",")))
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     ;; TODO(philc): Will I need these?
     ; (define-key evil-normal-state-map (kbd "M-,") nil)
     ; (define-key evil-normal-state-map (kbd "M-.") nil)))

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

;; Moving between Emacs windows (splits).
(global-set-key (kbd "M-J") 'windmove-down)
(global-set-key (kbd "M-K") 'windmove-up)

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

;;
;; OS X keybindings minor mode. Make it so the OSX keybindings you're used to always work.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;

(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key osx-keys-minor-mode-map (kbd "M-w") 'delete-frame)
(define-key osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key osx-keys-minor-mode-map (kbd "M-N") 'new-frame)
(define-key osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)

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
;; (defun vimlike-quit ()
;;   "Vimlike ':q' behavior: close current window if there are split windows;
;;    otherwise, close current tab (elscreen)."
;;   (interactive)
;;   (let ((one-elscreen (elscreen-one-screen-p))
;;         (one-window (one-window-p)))
;;     (cond
;;      if current tab has split windows in it, close the current live window
;;      ((not one-window)
;;       (delete-window) ; delete the current window
;;       (balance-windows) ; balance remaining windows
;;       nil)
;;      if there are multiple elscreens (tabs), close the current elscreen
;;      ((not one-elscreen)
;;       (elscreen-kill)
;;       nil)
;;      if there is only one elscreen, just try to quit (calling elscreen-kill
;;      will not work, because elscreen-kill fails if there is only one
;;      elscreen)
;;      (one-elscreen
;;       (evil-quit)
;;       nil))))

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
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
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

;;
;; Projectile (find file from the root of the current project).
;;
(projectile-global-mode)

;;
;; elscreen (tabs on the window).
;;
;; (elscreen-start)
;; (define-key evil-normal-state-map (kbd "M-S-]") 'elscreen-next)
;; (define-key evil-normal-state-map (kbd "M-S-[") 'elscreen-previous)
;; (define-key evil-normal-state-map (kbd "M-t") 'elscreen-create)



;; From Dmac - https://github.com/dmacdougall/dotfiles/blob/master/.emacs

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

(setq split-window-preferred-function 'split-window-sensibly-reverse)


;; All new!

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map ",c " 'evilnc-comment-or-uncomment-lines)
     (define-key evil-visual-state-map ",c " 'evilnc-comment-operator)))

(evil-leader/set-key
  "|" 'split-window-horizontally
  "-" 'split-window-vertically)
