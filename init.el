;; Largely "inspired" by philc's .emacs: https://github.com/philc/emacs-config/blob/master/.emacs

;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ac-nrepl
                      ace-jump-mode
                      ack-and-a-half
                      ag
                      auto-complete
                      buffer-move
                      cider
                      cider-tracing
                      clojure-mode
                      clojure-test-mode
                      coffee-mode
                      column-marker
                      color-theme-sanityinc-tomorrow
                      diminish
                      dired-details+ ; Hides all of the unnecessary file details in dired mode.
                      elisp-slime-nav
                      elscreen
                      exec-path-from-shell
                      evil
                      evil-leader
                      evil-nerd-commenter
                      fill-column-indicator
                      fiplr
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      framemove
                      go-mode
                      goto-last-change
                      ido-ubiquitous ; Make ido completions work everywhere.
                      ido-vertical-mode ; Show ido results vertically.
                      magit
                      markdown-mode
                      midje-mode
                      org
                      powerline
                      projectile ; Find file in project (ala CTRL-P).
                      rainbow-delimiters
                      ruby-electric ; Insert matching delimiters; unindent end blocks after you type them.
                      scss-mode
                      smartparens
                      smex
                      surround
                      undo-tree
                      yaml-mode
                      yasnippet
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Packages not on melpa:
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")

;;
;; General
;;

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/")
(require 'lisp-helpers-personal)

;; Anecdotally, this reduces the amount of display flicker on some Emacs startup.
(setq redisplay-dont-pause t)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Use the same PATH variable as your shell does. From http://clojure-doc.org/articles/tutorials/emacs.html
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(global-auto-revert-mode t) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(0.01))

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
     (setq whitespace-style '(face empty trailing))))
;; NOTE(harry) Flip the following two settings for editing snippets
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (setq-default mode-require-final-newline nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
; Some modes have their own tab-width variables.
(setq-default css-indent-offset 2)

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

;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-kill-word)
(define-key recentf-mode-map (kbd "C-h") 'backward-delete-char)

;; Settings for window splits.
(setq split-height-threshold 40)
(setq split-width-threshold 200)
(setq split-window-preferred-function 'split-window-sensibly-reverse)
;; Ensure these open in the selected window, not a new popup.
(setq same-window-buffer-names '("*magit-log*"))

;; "I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference."
(setq special-display-buffer-names '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
                                     "*magit-process*" "*magit-commit*" "*Compile-Log*" "*Gofmt Errors*"))
;; (setq special-display-regexps '())
(setq special-display-regexps '("*cider.*" "*ag.*"))
(setq special-display-function 'show-ephemeral-buffer-in-a-sensible-window)

;; A list of "special" (ephemeral) buffer names which should be focused after they are shown. Used by
;; show-ephemeral-buffer-in-a-sensible-window
(setq special-display-auto-focused-buffers '())

(defun switch-to-upper-left () (interactive) (select-window (frame-first-window)))
(defun switch-to-lower-left () (interactive) (switch-to-upper-left) (ignore-errors (windmove-down)))
(defun switch-to-upper-right () (interactive) (switch-to-upper-left) (ignore-errors (windmove-right 1)))
(defun switch-to-lower-right () (interactive) (switch-to-upper-right) (ignore-errors (windmove-down)))

;; References, for context:
;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
;; http://stackoverflow.com/questions/1002091/how-to-force-emacs-not-to-display-buffer-in-a-specific-window
;; The implementation of this function is based on `special-display-popup-frame` in window.el.
(defun show-ephemeral-buffer-in-a-sensible-window (buffer &optional buffer-data)
  "Given a buffer, shows the window in a right-side split."
  (let* ((original-window (selected-window))
         (create-new-window (one-window-p))
         (window (if create-new-window
                     (split-window-sensibly-reverse)
                   (save-excursion (switch-to-lower-right) (selected-window)))))
    (display-buffer-record-window (if create-new-window 'window 'reuse) window buffer)
    (set-window-buffer window buffer)
    (when create-new-window (set-window-prev-buffers window nil))
    (select-window original-window)
    (when (member (buffer-name buffer) special-display-auto-focused-buffers)
      (select-window window))
    window))

(defun dismiss-ephemeral-windows ()
  "Dismisses any visible windows in the current frame identifiedy by `special-display-buffer-names` and
   `special-display-regexps`. I use this to quickly dismiss help windows, compile output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (special-display-p (buffer-name buffer))
            (quit-window nil window))))
      (select-window original-window))))


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
;; Note that there is a bug where Evil-leader isn't properly bound to the initial buffers Emacs opens
;; with. We work around this by killing them. See https://github.com/cofi/evil-leader/issues/10.
(kill-buffer "*Messages*")

;; When opening new lines, indent according to the previous line.
(setq evil-auto-indent t)

(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)

; Some help keybindings which conflict with nothing else, so you can pull up help in any context.
(global-set-key (kbd "C-A-M-h") 'help) ; Here we clobber C-h, which accesses Emacs's help.
(global-set-key (kbd "C-A-M-b") 'describe-bindings)

(evil-leader/set-key
  "h" 'help
  "b" 'ido-switch-buffer
  "t" 'fiplr-find-file ;; 'projectile-find-file
  "a" 'ag-project
  "d" 'projectile-dired
  "|" (lambda () (interactive)(split-window-horizontally) (other-window 1))
  "\\" (lambda () (interactive)(split-window-horizontally) (other-window 1))
  "-" (lambda () (interactive)(split-window-vertically) (other-window 1))
  "eb" 'eval-buffer
  "es" 'eval-surrounding-sexp
  "gs" 'magit-status-and-focus-unstaged
  "gl" 'magit-log
  ; "v" is a mnemonic prefix for "view X".
  "vt" (lambda () (interactive) (find-file "~/Dropbox/tasks.org"))
  "vn" (lambda () (interactive) (find-file "~/Dropbox/notes.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "vl" (lambda () (interactive) (find-file "~/.lein/profiles.clj"))
  "vp" 'open-root-of-project-in-dired
  "vh" (lambda () (interactive) (find-file "~/workspace/liftoff_repos/liftoff/haggler/src/haggler/handler.clj"))
  "vg" (lambda () (interactive) (find-file "~/workspace/liftoff_repos/gumshoedb/src/gumshoe/core.go")))

(eval-after-load 'evil
  '(progn
     (setq evil-default-cursor t)
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     (define-key evil-normal-state-map (kbd "M-,") nil)
     (define-key evil-normal-state-map (kbd "M-.") nil)))

(setq evil-leader/leader ",")

;; Ensure we evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

(defun evil-shift-paragraph-left (beg end)
  "Shifts a paragraph left."
  (interactive "r")
  (let ((region (evil-inner-paragraph)))
    (save-excursion
      (evil-shift-left (first region) (second region)))))

(defun evil-shift-paragraph-right (beg end)
  "Shifts a paragraph right."
  (interactive "r")
  (let ((region (evil-inner-paragraph)))
    (save-excursion
      (evil-shift-right (first region) (second region)))))

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

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O.
(defun save-buffer-if-dirty ()
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate) (save-buffer-if-dirty))
(defadvice other-window (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-up (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-down (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-left (before other-window-now activate) (save-buffer-if-dirty))
(defadvice windmove-right (before other-window-now activate) (save-buffer-if-dirty))
;; This hasn't been a problem yet, but advising "select-window" may cause problems. For instance, it's called
;; every time a character is typed in isearch mode.
;; (defadvice select-window (before select-window activate) (save-buffer-if-dirty))

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
(define-key evil-window-map (kbd "m") 'delete-other-windows)
(define-key evil-window-map (kbd "b") 'winner-undo)
(define-key evil-window-map (kbd "q") 'dismiss-ephemeral-windows)


;;
;; Incremental search (isearch)
;;

;; Make highlighting during incremental search feel snappier.
(setq case-fold-search t) ; Make searches case insensitive.
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-max-at-a-time nil)
;; Hitting escape aborts the search, restoring your cursor to the original position, as it does in Vim.
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;; Make C-h act the same as backspace.
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; Make M-v paste the clipboard's text into the search ring.
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-w") 'isearch-del-word)

(defun trim-last-word-of-string (string)
  "Removes the last word from the given string. Word separators are -, _ and spaces. This is designed to
  perform the same function as kill-word, but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on isearch-del-char, from isearch.el."
  (interactive "p")
  (if (= 0 (length isearch-string))
    (ding)
    (setq isearch-string (trim-last-word-of-string isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; When pressing enter to confirm a search, or jumping to the next result, scroll the result to the center of
;; the window. This solves the UX problem of the result appearing at the bottom of the screen, with little
;; context.
;; (defadvice evil-search-next (after isearch-recenter activate) (recenter-no-redraw))
;; (defadvice evil-search-previous (after isearch-recenter activate) (recenter-no-redraw))
;; (defadvice isearch-exit (before isearch-recenter activate) (recenter-no-redraw))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))


;;
;; OS X keybindings minor mode. Make it so the OSX keybindings you're used to always work.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;

(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key osx-keys-minor-mode-map (kbd "M-~") '(lambda () (interactive) (other-frame -1)))
(define-key osx-keys-minor-mode-map (kbd "M-w") 'vimlike-quit)
(define-key osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key osx-keys-minor-mode-map (kbd "M-n") 'new-frame)
(define-key osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M-W") 'evil-quit) ; Close all tabs in the current frame..

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
;; Dired related
;;

(require 'dired-details+)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(put 'dired-find-alternate-file 'disabled nil)

;; "go to dired, then call split-window-vertically, then go to another dired dir. Now, when you press C to
;; copy, the other dir in the split pane will be default destination. Same for R (rename; move)."
(setq dired-dwim-target t)

;; Use the same buffer for going into and up directories.
(evil-define-key 'normal dired-mode-map (kbd "gu") (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map "H" (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map (kbd "<return>")
  'dired-find-alternate-file) ; was dired-advertised-find-file

(evil-define-key 'normal dired-mode-map "," nil) ; Ensure my evil-leader key works unhindered.
(evil-define-key 'normal dired-mode-map "cd" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "cf" 'dired-create-file)
;; (evil-define-key 'normal dired-mode-map "x" 'dired-mark)
(evil-define-key 'normal dired-mode-map "v" 'dired-details-toggle)
;; The "e" prefix is for execute.
(evil-define-key 'normal dired-mode-map "ed" 'dired-do-flagged-delete)
(evil-define-key 'normal dired-mode-map "em" 'dired-do-rename)

;; Taken from http://stackoverflow.com/a/18885461/46237.
(defun dired-create-file (file)
  "Create a file called FILE, and recursively create any parent directories.
  If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))


;;
;; Org mode, for TODOs and note taking.
;;

(require 'org)
(eval-after-load 'org
  '(progn
     ; This enables "clean mode", such that sublists use whitespace for indentation ala markdown.
     (setq org-startup-indented t)))

(defun always-insert-item ()
  "Force insertion of org item"
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

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

;; normal state shortcuts
(evil-define-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
         'org-forward-same-level
         'org-forward-heading-same-level)
  "gk" (if (fboundp 'org-backward-same-level)
         'org-backward-same-level
         'org-backward-heading-same-level)
  "gl" 'outline-next-visible-heading
  "t" 'org-todo
  "T" 'org-set-tags-command
  "O" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
  "o" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "^" 'org-beginning-of-line
  "$" 'org-end-of-line
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  "<" 'org-metaleft
  ">" 'org-metaright
  ",a" 'org-archive-subtree
  ",vt" 'org-show-todo-tree
  ",va" 'org-agenda
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown))
      '(normal insert))

(define-key org-mode-map "\M-t" nil)

(setq org-default-notes-file "~/Dropbox/tasks.org")
(define-key org-mode-map "\C-cc" 'org-capture)


;;
;; Projectile (find file from the root of the current project).
;;

(projectile-global-mode)

(setq project-folders '("~/workspace/liftoff_repos" "~/workspace/liftoff_repos/liftoff"))

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 1200)

(defun filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (->> (directory-files directory t)
       (remove-if (lambda (path)
                    (or (string/ends-with path ".")
                        (string/ends-with path "..")
                        (string/ends-with path ".git"))))
       (mapcar (lambda (file)
                 (if (and include-subdirectories (file-directory-p file))
                     (filter-files-in-directory file filter-fn include-subdirectories)
                   file)))
       flatten
       (remove-if-not filter-fn)))

(defun open-root-of-project-in-dired ()
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-file-file.
   Once a project is chosen, the current elscreen-tab is set to be the name of that project."
  (interactive)
  (let ((all-project-folders (->> project-folders
                                  (mapcar (lambda (file)
                                            (filter-files-in-directory file 'file-directory-p nil)))
                                  flatten)))
    (let ((project-to-open (ido-completing-read "Project folder: "
                                                (mapcar 'file-name-nondirectory all-project-folders)
                                                nil t)))
      (->> all-project-folders
           (remove-if-not (lambda (project) (string/ends-with project (concat "/" project-to-open))))
           first
           ((lambda (project)
              (dired project)
              ;; If we invoke this inside of a split, don't set the tab's title.
              (when (= 1 (length (window-list)))
                (elscreen-screen-nickname (file-name-nondirectory project)))))))))


;;
;; elscreen (tabs on the window).
;;

(elscreen-start)
(define-key evil-normal-state-map (kbd "M-}") 'elscreen-next)
(define-key evil-normal-state-map (kbd "M-{") 'elscreen-previous)
(define-key evil-normal-state-map (kbd "M-t") 'open-new-tab-with-current-buffer)

(defun open-new-tab-with-current-buffer ()
  (interactive)
  (evil-change-to-initial-state)
  (elscreen-clone)
  (delete-other-windows))


;;
;; Markdown
;;

(defun markdown-insert-list-item-below ()
  "Inserts a new list item under the current one. markdown-insert-list-item inserts above, by default."
  (interactive)
  (end-of-line)
  (call-interactively 'markdown-insert-list-item)
  (evil-append nil))

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))


(eval-after-load 'markdown-mode
  '(progn
     (evil-define-key 'normal markdown-mode-map
       (kbd "M-h") 'evil-shift-paragraph-left
       (kbd "M-l") 'evil-shift-paragraph-right)
     ;; Note that while in insert mode, using "evil-shift-paragraph-right" while the cursor is at the end of a
     ;; line will shift the paragraph incorrectly. That's why we jump to normal mode first, as a workaround.
     (evil-define-key 'insert markdown-mode-map
       (kbd "M-h") '(lambda ()
                        (interactive)
                        (evil-change-to-initial-state)
                        (call-interactively 'evil-shift-paragraph-left)
                        (evil-append nil))
       (kbd "M-l") '(lambda ()
                        (interactive)
                        (evil-change-to-initial-state)
                        (call-interactively 'evil-shift-paragraph-right)
                        (evil-append nil)))
     (mapc (lambda (state)
             (evil-define-key state markdown-mode-map
               (kbd "M-k") 'markdown-move-up
               (kbd "M-j") 'markdown-move-down
               ;; M-return creates a new todo item and enters insert mode.
               (kbd "<M-return>") 'markdown-insert-list-item-below))
           '(normal insert))))
;;
;; Snippets
;;

;; "Ignore the default snippets that come with yasnippet. My own are all I need, and I don't want any
;; conflicts."
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "ESC") 'yas-abort-snippet)


;;
;; Diminish - hide or shorten the names of minor modes in your modeline.
;; To see which minor modes you have loaded and what their modeline strings are: (message minor-mode-alist)
;;

(require 'diminish)
(diminish 'visual-line-mode "")
(diminish 'global-whitespace-mode "")
(diminish 'global-visual-line-mode "")
(diminish 'auto-fill-function "")
(diminish 'projectile-mode " p")
(diminish 'yas-minor-mode "yas")
(diminish 'osx-keys-minor-mode "")
(diminish 'undo-tree-mode "")


;;
;; Powerline: improve the appearance & density of the Emacs status bar (mode line).
;;

(require 'powerline)

(defface powerline-white-face
  '((t (:background "#e0e0e0" :foreground "black" :inherit mode-line)))
    "Face for powerline")
(defface powerline-black-face
  '((t (:background "#191919" :inherit mode-line)))
  "Face for powerline")

(defun powerline-projectile-project-name (&optional face padding)
  "Returns a string describing the projectile project for the current buffer. Takes the same arguments as
   powerline-raw."
  (powerline-raw (concat "(" (projectile-project-name) ")") face padding))

(defun powerline-personal-theme ()
  "My customized powerline, copied and slightly modified from the default theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" 'powerline-black-face 'l)
                                     (powerline-buffer-id 'powerline-black-face 'l)
                                     (powerline-raw " " 'powerline-black-face)
                                     (powerline-projectile-project-name 'powerline-black-face 'l)
                                     (powerline-raw " " 'powerline-black-face)
                                     (funcall separator-left mode-line face1)
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     ;; "Version control" - show the modeline of any active VC mode.
                                     (powerline-vc face1 'r)
                                     (powerline-raw "%4l" face1 'l) ; Current line number
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r) ; Current column number
                                     (powerline-raw " " face1)
                                     ;; A visual scrollbar shown inside 1x1 char
                                     (powerline-hud 'powerline-white-face face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

;; (powerline-default-theme)
(powerline-personal-theme)


;;
;; Misc languages
;;

;; CSS
(add-hook 'css-mode-hook (lambda ()
                           (autopair-mode 1) ; Auto-insert matching delimiters.
                           ;; Properly unindent a closing brace after you type it and hit enter.
                           (eletric-indent-mode)))

;; HTML
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

;; SCSS
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;
;; Coffeescript
;;

(setq coffee-tab-width 2)
(evil-leader/set-key-for-mode 'coffee-mode
  "c" nil ; Establishes "c" as a "prefix key". I found this trick here: http://www.emacswiki.org/emacs/Evil
  "cf" (lambda ()
         (interactive)
         (save-buffer)
         (coffee-compile-file))
  ;; The mnemonic for this is "compile & preview".
  "cp" 'coffee-compile-buffer)

;; Make return and open-line indent the cursor properly.
(evil-define-key 'insert coffee-mode-map (kbd "RET") 'coffee-newline-and-indent)
(evil-define-key 'normal coffee-mode-map "o" '(lambda ()
                                                (interactive)
                                                (end-of-line)
                                                (evil-append nil)
                                                (coffee-newline-and-indent)))


;;
;; Ruby
;;

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; Insert matching delimiters; unindent end blocks after you type them.
(add-hook 'ruby-mode-hook (lambda () (ruby-electric)))


;;
;; Emacs Lisp (elisp)
;;

(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(evil-define-key 'normal emacs-lisp-mode-map
  "K"'(lambda ()
        (interactive)
        ;; Run `describe-function` and show its output in a help
        ;; window. Inspired from help-fns.el.
        (with-help-window "*Help*"
          (describe-function (intern (current-word))))))

(defun current-sexp ()
  "Returns the text content of the sexp list around the cursor."
  (let ((position (bounds-of-thing-at-point 'list)))
    (buffer-substring-no-properties (car position) (cdr position))))

(defun elisp-eval-current-sexp ()
  (interactive)
  (print (eval (read (current-sexp)))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "eb" (lambda() (interactive) (save-buffer) (eval-buffer))
  "es" 'elisp-eval-current-sexp
  "ex" 'eval-defun)


;;
;; Clojure
;;

;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w" clojure-mode-syntax-table)))
(add-hook 'clojure-mode-hook (lambda ()
                               (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
                               ;; Comment lines using only one semi-colon instead of two.
                               (setq comment-add 0)))

(evil-define-key 'normal clojure-mode-map "K"
  (lambda () (interactive) (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(evil-define-key 'normal clojure-mode-map "gf" 'cider-jump)

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; Don't ask confirmation for closing any open nrepl connections when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(evil-leader/set-key-for-mode 'clojure-mode
  "eb" 'cider-load-current-buffer
  "es" 'cider-eval-expression-at-point
  "er" 'cider-eval-region
  "nj" 'cider-jack-in
  "nn" 'cider-repl-set-ns
  ;; This command sets and pulls up the appropriate nREPL for the current buffer. Useful when you have
  ;; multiple REPLs going.
  "nb" 'cider-switch-to-repl-buffer
  "nt" 'cider-toggle-trace)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-print-length 100)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-result-prefix ";; => ")

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Clojure indentation rules
;; (add-hook 'clojure-mode-hook (lambda () (setq lisp-indent-offset 2)))
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (send-off 1) (cli 1) (go-loop 1)                                  ; Core
     (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
     (values 1) (delete 1) (upsert 1) (subselect 1)
     (clone-for 1)                                                     ; Enlive
     (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
     (checker 1)                                                       ; Midje
     (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
     (url-of-form 1)                                                   ; Personal
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

(add-hook 'clojure-mode-hook 'clojure-test-mode)

;; Autocompletion in nrepl
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'auto-complete-mode)
(eval-after-load 'auto-complete '(add-to-list 'ac-modes 'cider-mode))


;;
;; From Dmac - https://github.com/dmacdougall/dotfiles/blob/master/.emacs
;;

(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-cleanup nil)
(setq lazy-highlight-max-at-a-time nil)
(global-undo-tree-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
;; (line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "RET") 'comment-indent-new-line)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(eval-after-load 'paren
  '(setq show-paren-delay 0))
(show-paren-mode t)

(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(ac-linum-workaround)


;;
;; Go mode, for writing Go code
;;

(defun go-save-and-compile-fn (command-name)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command-name command-name))
    #'(lambda ()
        (interactive)
        (save-buffer)
        (message command-name)
        (compile (concat "cd " (projectile-project-root) " && " command-name)))))

(evil-leader/set-key-for-mode 'go-mode
  ;; "r" is a namespace for run-related commands.
  "rr" (go-save-and-compile-fn "make run")
  ;; "rb" (go-save-and-compile-fn "make synthetic-benchmark")
  "rt" (go-save-and-compile-fn "make test")
  ;; "rw" (go-save-and-compile-fn "make run-web")
  ;; "c" is a namespace for compile-related commands.
  "cn" 'next-error
  "cp" 'previous-error
  ;; "cw" (go-save-and-compile-fn "make web")
  ;; "cb" (go-save-and-compile-fn "make benchmark")
  "cc" (go-save-and-compile-fn "make build"))

(defun gofmt-before-save-ignoring-errors ()
  "Don't pop up syntax errors in a new window when running gofmt-before-save."
  (interactive)
  (flet ((gofmt--process-errors (&rest args) t)) ; Don't show any syntax error output
    (gofmt-before-save)))

(defun init-go-buffer-settings ()
  ;; I have Emacs configured to save when switching buffers, so popping up errors when I switch buffers is
  ;; really jarring.
  (add-hook 'before-save-hook 'gofmt-before-save-ignoring-errors)
  ;; Make it so comments are line-wrapped properly when filling. It's an oversight that this is missing from
  ;; go-mode.
  (setq-local fill-prefix "// "))

(add-hook 'go-mode-hook 'init-go-buffer-settings)


;;
;; Magit - for staging hunks and making commits to git
;;

(require 'magit-mode-personal)


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

(require 'buffer-move)
(global-set-key (kbd "C-S-k") 'buf-move-up)
(global-set-key (kbd "C-S-j") 'buf-move-down)
(global-set-key (kbd "C-S-h") 'buf-move-left)
(global-set-key (kbd "C-S-l") 'buf-move-right)

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

(dolist (i (number-sequence 1 9))
  (lexical-let ((tab-index (- i 1)))
    (global-set-key (kbd (concat "M-" (number-to-string i)))
                    (lambda () (interactive) (elscreen-goto tab-index)))))

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem)
(setq sp-autoescape-string-quote nil)

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


;; tweak projectile to not us git ls-files
(require 'projectile)
(defun projectile-project-vcs ()
  "Determine the VCS used by the project if any."
  'none)

(eval-after-load 'fiplr
  '(setq fiplr-ignored-globs '((directories (".git" ".svn" "target" "log" ".sass-cache" "Build"))
                               (files (".#*" "*.so" ".DS_Store" ".class")))))

; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))
