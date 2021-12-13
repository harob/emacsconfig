;; Largely "inspired" by philc's .emacs: https://github.com/philc/emacs-config/blob/master/.emacs

;;
;; Package management
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ace-link
                      ag
                      amx ; A fork of smex, which upgrades M-x
                      auto-complete
                      avy
                      browse-at-remote
                      buffer-move
                      cider
                      clojure-mode
                      coffee-mode
                      color-theme-sanityinc-tomorrow
                      company
                      counsel
                      dash
                      dash-functional
                      deft ; Notational Velocity-style note taking
                      diminish
                      dumb-jump ; Go-to-definition for all languages, using ag
                      elisp-slime-nav
                      elscreen
                      ess
                      evil
                      evil-anzu
                      evil-args
                      evil-exchange
                      evil-leader
                      evil-matchit
                      evil-nerd-commenter
                      evil-numbers
                      evil-surround
                      evil-visualstar
                      fill-column-indicator
                      ;framemove ;; TODO(harry) No longer available on melpa?
                      go-mode
                      goto-last-change
                      haskell-mode
                      htmlize
                      ivy
                      less-css-mode
                      lua-mode
                      magit
                      markdown-mode
                      mustache-mode
                      neotree
                      noflet ; Replacement for the deprecated flet macro - see
                             ; http://emacsredux.com/blog/2013/09/05/a-proper-replacement-for-flet/
                      org
                      org-download
                      paradox ; Better package menu
                      projectile
                      protobuf-mode
                      rainbow-delimiters
                      ruby-electric ; Insert matching delimiters; unindent end blocks after you type them.
                      scss-mode
                      smartparens
                      swiper
                      undo-tree
                      wcheck-mode
                      web-mode
                      which-key
                      yaml-mode
                      yasnippet
                      ))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;
;; General settings
;;

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'lisp-helpers-personal)
(require 'emacs-utils)

;; Anecdotally, this reduces the amount of display flicker on some Emacs startup.
(setq redisplay-dont-pause t)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Make it so that the scratch buffer uses markdown. By default it uses Emacs Lisp mode.
(setq initial-major-mode 'markdown-mode)

;; Sync environment variables.
;; NOTE(harry) On OSX, run the commands from https://gist.github.com/mcandre/7235205 to properly set the PATH
;; when launching from Spotlight, LaunchBar, etc.
(defun sync-env ()
  (when (memq window-system '(mac ns x))
    (let ((env-pair-re "^\\([^=[:space:]]+\\)=\\(.*\\)$"))
      (with-temp-buffer
        (shell-command (concat shell-file-name " -i -c env") t)
        (goto-char (point-min))
        (while (re-search-forward env-pair-re nil t)
          (let ((name (match-string 1))
                (val (match-string 2)))
            (setenv name val)
            (when (string-equal "PATH" name)
              (setq eshell-path-env val
                    exec-path (append (parse-colon-path val) (list exec-directory))))))))))
(sync-env)

(global-auto-revert-mode t) ; Reload an open file from disk if it is changed outside of Emacs.

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(setq mac-pass-command-to-system nil) ; Avoid e.g. M-h performing OSX's "Hide window" command
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      ;; Make touchpad scrolling on OSX less jerky
      mouse-wheel-scroll-amount '(0.01))

;; The preference file for Emac's "Customize" system. `M-x customize` to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Colorscheme
(load-theme 'sanityinc-tomorrow-bright t)
(set-face-attribute 'default nil :family "Consolas" :height 150)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     ;; (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace mode by default marks all whitespace. Show only tabs, trailing space, and trailing lines.
     (setq whitespace-style '(face empty trailing))))
;; NOTE(harry) Flip the following two settings for editing snippets
(add-hook 'before-explicit-save-hook 'delete-trailing-whitespace)
;; (setq-default mode-require-final-newline nil)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
; Some modes have their own tab-width variables.
(setq-default css-indent-offset 2)

(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
;; (auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
;; (add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!

;; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
;; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
(global-visual-line-mode t)

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
(global-hl-line-mode t)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example.If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

(defun backward-delete-word ()
  "Deletes the word behind the cursor, and does not yank the contents to the clipboard."
  ; This implementation is based on backward-kill-word.
  (interactive)
  (delete-region (point) (progn (forward-word -1) (point))))

;; Enable the common Bash text-editing shortcuts in the minibuffer.
;; Enable the common Bash text-editing shortcuts in the minibuffer.
(util/define-keys minibuffer-local-map
                  (kbd "C-k") 'kill-line
                  (kbd "C-e") 'end-of-line
                  ;; NOTE(harry) I don't use this, and it conflicts with Emac's universal argument key binding:
                  ;; (kbd "C-u") 'backward-kill-line
                  (kbd "C-d") 'delete-char
                  (kbd "C-w") 'backward-kill-word
                  (kbd "C-h") 'backward-delete-char)

;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; ;; Use amx to show the M-x command prompt. It has better completion support than the default M-x.
(require 'amx)
(amx-mode 1)
;; (require 'counsel)
;; (global-set-key (kbd "M-x") 'counsel-M-x)

;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-kill-word)
(define-key recentf-mode-map (kbd "C-h") 'backward-delete-char)

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O.
(add-hook 'focus-out-hook 'util/save-buffer-if-dirty) ; This hook is only available in Emacs 24.4+.
(defadvice windmove-up (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-down (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-left (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-right (before other-window-now activate) (util/save-buffer-if-dirty))

; This is fired whenever the buffer list is updated, which is a reasonably robust way to detect that the
; window config has changed and the current buffer should be saved.
(add-hook 'buffer-list-update-hook 'util/save-buffer-if-dirty)

(setq create-lockfiles nil)
(setq eldoc-echo-area-use-multiline-p nil)


;;
;; Evil mode -- Vim keybindings for Emacs.
;;

(setq evil-want-C-u-scroll t)
(setq evil-undo-system 'undo-tree)
(require 'evil)
(require 'evil-nerd-commenter)
(require 'goto-last-change)

(require 'evil-leader) ; Provide configuration functions for assigning actions to a Vim leader key.
(setq evil-leader/leader "SPC")
;; Access leader with C-SPC in insert mode:
(setq evil-leader/in-all-states t)
;; Ensure evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))
(global-evil-leader-mode)

(evil-mode t)

(require 'which-key)
(which-key-mode)
(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-state-maps t)

;; When opening new lines, indent according to the previous line.
(setq evil-auto-indent t)

;; Move up and down through long, wrapped lines one visual line at a time.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)
;; I use this shortcut for manually splitting lines. Note that it does not put you in insert mode.
(define-key evil-normal-state-map (kbd "s") 'newline-and-indent)

;; By default, Emacs will not indent when you hit enter/return within a comment.
;; (define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

;; When jumping back and forth between marks, recenter the screen on the cursor.
(define-key evil-normal-state-map (kbd "C-o")
  (lambda () (interactive) (evil-jump-backward) (recenter-no-redraw)))
(define-key evil-normal-state-map (kbd "C-i")
  (lambda () (interactive) (evil-jump-forward) (recenter-no-redraw)))

;; Evil uses the current file's mode's definition of a paragraph, which is often surprising. For instance, in
;; Markdown mode, a single item in a bullet list consistutes a paragraph. Instead, I've defined a paragraph to
;; be hunks of text separated by newlines. That's typically what I would expect of a paragraph. You can still
;; use Evil's paragraph definition using the text object "P" instead of "p".
(evil-define-text-object evil-paragraph-from-newlines (count &optional beg end type)
  "Select a paragraph separated by newlines."
  :type line
  ;; These two vars are set by the current programming mode. Set them to their default text mode values
  ;; temporarily while we select the paragraph. The implementation of evil-move-paragraph invokes
  ;; `forward-paragraph`, which uses these variables.
  (let ((paragraph-start "\f\\|[     ]*$")
        (paragraph-separate "[  ]*$"))
    ;; TODO(harry) Change to the following after upgrading evil:
    ;; (evil-select-an-object 'evil-paragraph beg end type count)))
    (evil-an-object-range count beg end type #'evil-move-paragraph nil nil t)))

(define-key evil-outer-text-objects-map "p" 'evil-paragraph-from-newlines)
(define-key evil-outer-text-objects-map "P" 'evil-a-paragraph)

(evil-leader/set-key
  "h" 'help
  "SPC" 'amx
  ":" 'eval-expression
  ";" 'eval-expression
  "b" 'ivy-switch-buffer
  "t" 'counsel-fzf
  "a" 'counsel-rg
  "d" 'deft
  "/" 'swiper
  "u" 'universal-argument
  "\\" (lambda () (interactive)
         (split-window-horizontally)
         (other-window 1)
         (balance-windows))
  "-" (lambda () (interactive)
        (split-window-vertically)
        (other-window 1)
        (balance-windows))
  "gs" (lambda() (interactive)
          (util/save-buffer-if-dirty)
          (magit-status-and-focus-unstaged))
  "gl" 'magit-log-current
  ;; "v" is a mnemonic prefix for "view X".
  "vd" 'projectile-dired
  "vp" 'open-root-of-project-in-dired
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "vh" (lambda () (interactive) (find-file "~/workspace/src/liftoff/haggler/src/haggler/handler.clj"))
  "vk" (lambda () (interactive) (find-file "~/workspace/side_projects/qmk_firmware/keyboards/ergodox/keymaps/dvorak_harob/keymap.c"))
  "vi" (lambda () (interactive) (find-file "~/Dropbox/notes/inbox.org") (org-mode))
  "vs" (lambda () (interactive) (switch-to-buffer "*scratch*"))
  "vt" (lambda () (interactive) (find-file "~/Dropbox/notes/tasks.org") (org-mode))
  "vz" (lambda () (interactive) (find-file "~/dotfiles/.zshrc")))

;; TODO(harry) Write a macro to prepend the evil-leader key instead of manually specifying SPC.
(which-key-add-key-based-replacements
  "SPC c" "Comment"
  "SPC e" "Evaluate"
  "SPC g" "Git"
  "SPC r" "Render"
  "SPC v" "View"
  "SPC w" "Window")

(eval-after-load 'evil
  '(progn
     (setq evil-default-cursor t)
     ;; Unbind these keys in evil so they can instead be used for code navigation.
     (define-key evil-normal-state-map (kbd "M-,") nil)
     (define-key evil-normal-state-map (kbd "M-.") nil)))

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

;; Enable the typical Bash/readline keybindings when in insert mode.
(util/define-keys evil-insert-state-map
                  (kbd "C-h") 'backward-delete-char
                  (kbd "C-k") 'kill-line
                  (kbd "C-a") 'beginning-of-line
                  (kbd "C-e") 'end-of-line
                  ;; (kbd "C-u") 'backward-kill-line
                  (kbd "C-d") 'delete-char
                  (kbd "C-w") 'backward-delete-word
                  (kbd "C-p") 'previous-line
                  (kbd "C-n") 'next-line)
;; (global-set-key (kbd "C-h") 'backward-delete-char) ; Here we clobber C-h, which accesses Emacs's help.

(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map " cc" 'evilnc-comment-or-uncomment-lines)
     (define-key evil-visual-state-map " cc" 'evilnc-comment-operator)))

(require 'evil-surround)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "gs" 'evil-Surround-region)
(define-global-minor-mode global-surround-mode-with-exclusions global-evil-surround-mode
  (lambda ()
    (when (not (memq major-mode (list 'magit-status-mode)))
      (evil-surround-mode 1))))
(global-surround-mode-with-exclusions 1)

(require 'evil-visualstar)
(global-evil-visualstar-mode)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-anzu)
(set-face-attribute 'anzu-mode-line nil :foreground "black" :weight 'bold)

(require 'evil-args)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; gx to swap two text objects:
(require 'evil-exchange)
(evil-exchange-install)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)


;;
;; Window manipulation, switching, & management.
;;

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
(setq special-display-regexps '("*cider.*" "*ag.*" "magit: .**"))
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

;; Evil's window map is the set of keys which control window functions. All of its keys are prefixed with
;; <C-w>.
;; Undo the last change you made to your window configuration. Very handy as a method for temporarily
;; maximizing a window: first invoke delete-other-windows, and then invoke winner-undo..
(define-key evil-window-map (kbd "m") 'delete-other-windows)
(define-key evil-window-map (kbd "b") 'winner-undo)
(define-key evil-window-map (kbd "q") 'dismiss-ephemeral-windows)

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
;; Note that when Emacs becomes unresponsive (e.g. because I accidentally grepped my home directory), I might
;; still need to hold C-g (the Emacs esc/cancel key) to bring it back.
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
;; Incremental search (isearch)
;;

;; Make highlighting during incremental search feel snappier.
(setq case-fold-search t) ; Make searches case insensitive.
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-cleanup nil)
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
;; Mac OS X keybindings minor mode.
;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;

(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(util/define-keys osx-keys-minor-mode-map
                  (kbd "M-`") 'other-frame
                  (kbd "M-~") '(lambda () (interactive) (other-frame -1))
                  (kbd "M-w") 'vimlike-quit
                  (kbd "M-q") 'save-buffers-kill-terminal
                  (kbd "M-n") 'new-frame
                  (kbd "M-a") 'mark-whole-buffer
                  (kbd "M-s") 'explicitly-save-buffer
                  (kbd "M-v") 'clipboard-yank
                  (kbd "M-c") 'clipboard-kill-ring-save
                  (kbd "M-W") 'evil-quit ; Close all tabs in the current frame..
                  (kbd "M-A-h") 'mac-hide-others)

(define-minor-mode osx-keys-minor-mode
  "A minor-mode for emulating osx keyboard shortcuts."
  t " osx" osx-keys-minor-mode-map)

(osx-keys-minor-mode t)

(defadvice load (after give-osx-keybindings-priority activate)
  "Try to ensure that osx keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'osx-keys-minor-mode))
      (let ((osx-keys (assq 'osx-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'osx-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist osx-keys))))
(ad-activate 'load)

(defun open-folder-in-finder ()
  "Opens the folder of the current file in OSX's Finder."
  (interactive)
  (call-process-region nil nil "/usr/bin/open" nil nil nil "."))

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

(defvar before-explicit-save-hook nil)
(defvar after-explicit-save-hook nil)

(defun explicitly-save-buffer ()
  (interactive)
  (run-hooks 'before-explicit-save-hook)
  (save-buffer)
  (run-hooks 'after-explicit-save-hook))

;; From http://emacs.stackexchange.com/a/18981
(defun mac-hide-others ()
  "On a Mac, hide all applications other than Emacs."
  (interactive)
  (do-applescript (concat "tell application \"System Events\" to "
                          "set visible of every process whose visible is true "
                          "and name is not \"Emacs\" to "
                          "false")))


;;
;; Filename completions (i.e. CTRL-P or CMD-T in other editors)
;;

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 20)
(setq ivy-wrap t)
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-use-selectable-prompt t)
;; TODO(harry) Remove some of the default "^"'s in this var:
;; (setq ivy-initial-inputs-alist )
(define-key ivy-mode-map (kbd "C-h") 'backward-delete-char)
(define-key ivy-mode-map (kbd "C-w") 'backward-delete-word)

(require 'swiper)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "C-h") 'backward-delete-char)
(define-key swiper-map (kbd "C-w") 'backward-delete-word)

(setq counsel-fzf-cmd "fzf --exact --filter=\"%s\"")

;;
;; Dired mode - using the Emacs file browser.
;;

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(put 'dired-find-alternate-file 'disabled nil) ; By default, the dired-find-alternative-file fn is disabled.

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
(evil-define-key 'normal dired-mode-map "x" 'dired-mark)
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
;; Emacs Lisp (elisp)
;;

(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "M-h") 'shift-sexp-backward
  (kbd "M-l") 'shift-sexp-forward
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
  (message "%s" (eval (read (current-sexp)))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  ; Note that I'm saving the buffer before each eval because otherwise, the buffer gets saved after the eval,
  ; (due to save-when-switching-windows setup) and the output from the buffer save overwrites the eval results
  ; in the minibuffer.
  "eb" (lambda() (interactive) (util/save-buffer-if-dirty) (eval-buffer))
  "es" (lambda () (interactive) (util/save-buffer-if-dirty) (elisp-eval-current-sexp))
  "ex" (lambda () (interactive) (util/save-buffer-if-dirty) (call-interactively 'eval-defun))
  "ee" 'view-echo-area-messages)

;; Indentation rules.
(put '-> 'lisp-indent-function nil)
(put '->> 'lisp-indent-function nil)


;;
;; Org mode, for TODOs and note taking.
;;

(require 'org-mode-personal)


;;
;; Projectile (find file from the root of the current project).
;;

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

(setq project-folders '("~/workspace/src" "~/workspace/src/liftoff"))

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
    (let ((project-to-open (ivy-completing-read "Project folder: "
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
;; Snippets
;;

;; "Ignore the default snippets that come with yasnippet. My own are all I need, and I don't want any
;; conflicts."
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "ESC") 'yas-abort-snippet)


;;
;; Spell checking
;;

;; TODO(harry) This is breaking counsel for some reason:
;; (require 'wcheck-mode)
;; (setq-default wcheck-language "English")
;; (setq-default wcheck-language-data
;;               '(("English"
;;                  (program . "/usr/local/bin/aspell")
;;                  (args "list") ; -l: list only the mispellings.
;;                  (face . hi-yellow)
;;                  (connection . pty)
;;                  ;; Note that I don't use this functionality of providing suggested spelling corrects, and
;;                  ;; this config is untested. I just like to highlight mispelled words.
;;                  (action-program . "/usr/local/bin/aspell")
;;                  (action-args "-a") ; -a: lists alternatives.
;;                  (action-parser . wcheck-parser-ispell-suggestions))))

;; (add-hook 'text-mode-hook 'wcheck-mode)

;; (define-key evil-normal-state-map (kbd "zg") 'wcheck-add-to-dictionary)

;; (defvar custom-dictionary-file "~/.aspell.en.pws")

;; (defun wcheck-add-to-dictionary ()
;;   "Adds the word under the cursor to your personal dictionary. Also re-spellchecks the buffer to clear any
;;    stale highlights."
;;   (interactive)
;;   (let ((word (thing-at-point 'word)))
;;     (if (not (and custom-dictionary-file (file-writable-p custom-dictionary-file)))
;;         (message "Couldn't locate your custom dictionary file '%s'" custom-dictionary-file)
;;       (progn
;;         (with-temp-buffer
;;           (insert word) (newline)
;;           (append-to-file (point-min) (point-max) custom-dictionary-file))
;;         (message "Added word \"%s\" to %s" word custom-dictionary-file)
;;         ; This is a hack to toggle the mode on and then off, to rescane the buffer and remove the mispelt
;;         ; marker for the word that was just added to the dict.
;;         (wcheck-mode)
;;         (wcheck-mode)))))


;;
;; Diminish - hide or shorten the names of minor modes in your modeline.
;; To see which minor modes you have loaded and what their modeline strings are: (message minor-mode-alist)
;;

(require 'diminish)
(diminish 'visual-line-mode "")
(diminish 'global-whitespace-mode "")
;(diminish 'global-visual-line-mode "")
(diminish 'auto-fill-function "")
(diminish 'projectile-mode "")
(diminish 'yas-minor-mode "")
(diminish 'osx-keys-minor-mode "")
(diminish 'undo-tree-mode "")
(diminish 'ivy-mode "")
(diminish 'company-mode "")


;;
;; Powerline: improve the appearance & density of the Emacs status bar (mode line).
;;

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))


;;
;; Markdown
;;

(defun markdown-insert-list-item-below ()
  "Inserts a new list item under the current one. markdown-insert-list-item inserts above, by default."
  (interactive)
  (end-of-line)
  (call-interactively 'markdown-insert-list-item)
  (evil-append nil))

(defun insert-markdown-header (header-line-text)
  "With the cursor focused on the header's text, insert a setext header line below that text.
   header-line-text: either '===' or '---'"
  (end-of-line)
  (insert (concat "\n" header-line-text))
  (markdown-complete)
  ;; markdown-complete inserts a newline after the header. Remove it and move the cursor to a logical place.
  (next-line)
  (next-line)
  (delete-backward-char 1)
  (next-line)
  (beginning-of-line))

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(eval-after-load 'markdown-mode
  '(progn
     (evil-define-key 'normal markdown-mode-map
       ;; Autocomplete setext headers by typing "==" or "--" on the header's line in normal mode.
       (kbd "==") '(lambda () (interactive) (insert-markdown-header "=="))
       (kbd "--") '(lambda () (interactive) (insert-markdown-header "--"))
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

(mapc (lambda (mode)
        (evil-leader/set-key-for-mode mode
          "rr" 'markdown-preview))
      '(gfm-mode markdown-mode))

(setq markdown-command
      (concat
       "/usr/local/bin/pandoc"
       " --from markdown --to html"
       " --metadata title='-'"
       ;; Use Gmail's default styling, so I can copy exported HTML into the Compose window with no reformatting:
       " --include-in-header $HOME/.emacs.d/resources/gmail.css"))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 80)))


;;
;; CSS
;;

(add-hook 'css-mode-hook (lambda ()
                           ;; (autopair-mode 1) ; Auto-insert matching delimiters.
                           ;; Properly unindent a closing brace after you type it and hit enter.
                           (electric-indent-mode)))


;;
;; Coffeescript
;;

(setq coffee-tab-width 2)
(evil-leader/set-key-for-mode 'coffee-mode
  "c" nil ; Establishes "c" as a "prefix key". I found this trick here: http://www.emacswiki.org/emacs/Evil
  ;; This compiles the file and jumps to the first error, if there is one.
  "cc" (lambda ()
         (interactive)
         (save-buffer)
         (coffee-compile-without-side-effect))
  ;; The mnemonic for this is "compile & preview". It shows the javascript output in a new buffer.
  "cp" 'coffee-compile-buffer)

(defun coffee-compile-without-side-effect ()
  ;; coffee-compile-file annoyingly creates a file on disk.
  (let* ((js-file (concat (file-name-sans-extension (buffer-file-name)) ".js"))
         (js-file-existed (file-exists-p js-file)))
    (coffee-compile-file)
    (when (and (not js-file-existed) (file-exists-p js-file))
      (delete-file js-file))))

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
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode)))


;;
;; Rainbow-delimiters: highlight parentheses in rainbow colors.
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;
;; Smartparens utility functions
;;

(require 'smartparens)

(require 'smartparens-config)
(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem)
(setq sp-autoescape-string-quote nil)

(sp-with-modes '(clojure-mode)
  (sp-local-pair "`" "`" :when '(sp-in-string-p)))

(sp-with-modes '(org-mode)
  (sp-local-pair "=" nil :actions :rem)
  (sp-local-pair "~" nil :actions :rem))

(global-set-key (kbd "M-H") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-L") 'sp-forward-barf-sexp)

(defun shift-sexp-backward ()
  (interactive)
  (let* ((next (save-excursion (sp-forward-sexp)))
         (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
    (sp--transpose-objects prev next))
  ;; Focus the cursor correctly.
  (sp-backward-sexp)
  (sp-backward-sexp))

(defun shift-sexp-forward ()
  (interactive)
  (sp-forward-sexp)
  (let* ((next (save-excursion (sp-forward-sexp)))
         (prev (save-excursion (goto-char (sp-get next :beg-prf)) (sp-backward-sexp))))
    (sp--transpose-objects prev next))
  ;; Focus the cursor correctly.
  (sp-backward-sexp))


;;
;; Clojure
;;

(require 'clojure-mode)
(require 'clojure-mode-personal)
(require 'cider-test-personal)

(evil-leader/set-key-for-mode 'clojure-mode
  "ett" 'cider-test/run-test-at-point
  "etb" 'cider-test/run-tests-in-ns)

(which-key-add-major-mode-key-based-replacements 'clojure-mode
  "SPC e t" "EvaluateTests")

(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


;;
;; HTML mode
;;

(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

(defun preview-html ()
  "Pipes the buffer's contents into a script which opens the HTML in a browser."
  (interactive)
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c" "bcat"))

(defun indent-html-buffer ()
  (interactive)
  ;; html-beautify is a program defined here: https://github.com/beautify-web/js-beautify
  ;; To install: cd ~; npm install js-beautify; add ~/node_modules/.bin to your PATH.
  ;; I don't know why, but save-excursion does not maintain the cursor position.
  ;; (save-excursion
  (let ((p (point))
        (scroll-y (window-start)))
    (call-process-region (point-min) (point-max) "html-beautify" t (buffer-name) t
                         "--file" "-" ; STDIN
                         "--indent-size" "2"
                         "--wrap-line-length" "110")
    (set-window-start (selected-window) scroll-y)
    (goto-char p)))

(evil-leader/set-key-for-mode 'html-mode
  "i" 'indent-html-buffer
  "rr" 'preview-html)

;; TODO(harry) If this doesn't work out try multi-web-mode or mmm-mode. See:
;; http://www.emacswiki.org/emacs/MultipleModes
;; http://stackoverflow.com/questions/4462393/how-do-i-configure-emacs-for-editing-html-files-that-contain-javascript
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)


;;
;; SCSS mode, for editing SCSS files.
;;

(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;;
;; YAML mode, for editing YAML files
;;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;
;; Go mode, for writing Go code
;;
(with-eval-after-load "go-mode"
  (evil-define-key 'normal go-mode-map
    "K" 'godef-describe))

(defun go-save-and-compile-fn (command)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command command))
    #'(lambda ()
        (interactive)
        (go-save-and-compile command))))

;; Note that this function uses (projectile-project-root) to determine the directory to run `go` commands,
;; which requires that the go project have a .projectile file in it or that it be at the root of a git repo.
(defun go-save-and-compile (command)
  "Saves the current buffer before invoking the given command."
  (lexical-let ((has-makefile (file-exists-p (concat (projectile-project-root) "Makefile"))))
    (save-buffer)
    (message command)
    (util/without-confirmation
     (lambda () (compile (concat "cd " (projectile-project-root) " && " command))))))

(evil-leader/set-key-for-mode 'go-mode
  ;; "r" is a namespace for run-related commands.
  "rr" (go-save-and-compile-fn "make run")
  "rb" (go-save-and-compile-fn "make synthetic-benchmark")
  "rt" (go-save-and-compile-fn "make test")
  "rw" (go-save-and-compile-fn "make run-web")
  ;; "c" is a namespace for compile-related commands.
  "cn" 'next-error
  "cp" 'previous-error
  "cw" (go-save-and-compile-fn "make web")
  "cb" (go-save-and-compile-fn "make benchmark")
  "cc" (go-save-and-compile-fn "make compile")
  ;; "cc" (go-save-and-compile-fn "go build")

  ;; "ai" 'go-import-add
  )

;; goimports formats your code and also adds or removes imports as needed.
;; goimports needs to be on your path. See https://godoc.org/code.google.com/p/go.tools/cmd/goimports
(setq gofmt-command "goimports")

(setq gofmt-in-progress nil)

(defun gofmt-before-save-ignoring-errors ()
  "Don't pop up syntax errors in a new window when running gofmt-before-save."
  (interactive)
  ;; Note that `gofmt-before-save` triggers this save-hook for some reason, so we lock on gofmt-in-progress to
  ;; to protect from infinite recurision.
  (when (not gofmt-in-progress)
    (setq gofmt-in-progress 't)
    (cl-letf (((symbol-function #'gofmt--process-errors) (lambda (&rest args) t)))
      (gofmt-before-save))
    (setq gofmt-in-progress nil)))

(defun init-go-buffer-settings ()
  ;; I have Emacs configured to save when switching buffers, so popping up errors when I switch buffers is
  ;; really jarring.
  (add-hook 'before-save-hook 'gofmt-before-save-ignoring-errors nil t)
  ;; Make it so comments are line-wrapped properly when filling. It's an oversight that this is missing from
  ;; go-mode.
  (setq-local fill-prefix "// "))

(add-hook 'go-mode-hook 'init-go-buffer-settings)

(defun go-package-of-current-buffer ()
  "Returns the go package name defined in the current buffer. Returns nil if no package has been defined."
  (let ((file-contents (buffer-string)))
    (let ((match-exists (string-match "^package \\(.+\\)\w*" file-contents)))
      (when match-exists
        (buffer-substring-no-properties (+ 1 (match-beginning 1))
                                        (+ 1 (match-end 1)))))))


;;
;; Magit - for staging hunks and making commits to git
;;

(require 'magit-config)


;;
;; Javascript
;;

(setq js-indent-level 2)


;;
;; Lua
;;

(setq lua-indent-level 2)


;;
;; Misc
;;

;; Don't use the native OSX full screen support, because it uses OSX Spaces which don't play well with
;; CMD-tabbing to applications which are behind Emacs. Invoke fullscreen with `toggle-frame-fullscreen`.
(setq ns-use-native-fullscreen nil)
(evil-leader/set-key "wf" 'toggle-frame-fullscreen)

;; NOTE(harry) Using web-mode instead
;; (add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode))


;;
;; From Dmac - https://github.com/dmacdougall/dotfiles/blob/master/.emacs
;;

(global-undo-tree-mode t)
(global-font-lock-mode t)
(global-linum-mode t)
;; (line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "RET") 'comment-indent-new-line)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(eval-after-load 'paren
  '(setq show-paren-delay 0))
(show-paren-mode t)


;;
;; Emacs general autocompletion
;;
;; One nice feature of Emac's autocomplete mode is that it auto-populates likely completions as you type.
;; However, in doing so, it causes my cursors to flicker in other splits while I'm typing. This makes the mode
;; unusable.
;; (setq ac-auto-start nil)
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(ac-linum-workaround)


;;
;; All new!
;;

(setq find-function-C-source-directory "$HOME/workspace/external_codebases/emacs/src")

;; Switch across both windows (i.e. panes/splits) and frames (i.e. OS windows)!
;; FIXME(harry) Find a replacement package; emacs says it can't find this package...
;(require 'framemove)
;(setq framemove-hook-into-windmove t)
(define-key evil-normal-state-map (kbd "C-h") (lambda () (interactive) (ignore-errors (evil-window-left 1))))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (ignore-errors (evil-window-down 1))))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (ignore-errors (evil-window-up 1))))
(define-key evil-normal-state-map (kbd "C-l") (lambda () (interactive) (ignore-errors (evil-window-right 1))))

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

(define-key evil-normal-state-map (kbd "zz") 'evil-scroll-line-to-center)

;; TODO(harry) Why doesn't this work?
;; (dolist (state-map '(evil-normal-state-map evil-visual-state-map))
;;   (define-key state-map (kbd ";") 'evil-ex)
;;   (define-key state-map (kbd "H") 'evil-first-non-blank)
;;   (define-key state-map (kbd "L") 'evil-end-of-line))

(dolist (i (number-sequence 1 9))
  (lexical-let ((tab-index (- i 1)))
    (global-set-key (kbd (concat "M-" (number-to-string i)))
                    (lambda () (interactive) (elscreen-goto tab-index)))))

(define-key osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M-+") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-increase 0)))

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

(require 'fill-column-indicator)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'gfm-mode-hook 'fci-mode)

;; TODO(harry) Move into a .projectile file since I'm no longer using fiplr
(eval-after-load 'fiplr
  '(setq fiplr-ignored-globs '((directories (".git" ".svn" "target" "log" ".sass-cache" "Build" ".deps"
                                             "vendor" "MoPubSDK" "output" "checkouts"
                                             "elpa"))
                               (files (".#*" "*.so" ".DS_Store" "*.class")))))

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode " SN"))
(setq source-directory "$HOME/workspace/external_codebases/emacs-mac/src")

(setq tramp-default-method "pscp")

(setq ag-reuse-buffers 't)
(setq ag-reuse-window 't)

; From http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-path-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(setq explicit-shell-file-name "bash")

(require 'dumb-jump)
(setq dumb-jump-selector 'ivy)
;; Override the standard tags-based go-to-definition key bindings:
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "M-,") 'dumb-jump-back)

(require 'avy)
(setq avy-keys (number-sequence ?a ?z))
(setq avy-all-windows 'all-frames)
(evil-leader/set-key "z" 'avy-goto-word-0)
(define-key evil-motion-state-map (kbd "z") 'avy-goto-word-0)
(define-key evil-visual-state-map (kbd "z") 'avy-goto-word-0)
(evil-leader/set-key "Z" 'avy-goto-line)
(define-key evil-motion-state-map (kbd "Z") 'avy-goto-line)
(define-key evil-visual-state-map (kbd "Z") 'avy-goto-line)

;; Open links vimium-style with `o` in various help-like modes
(require 'ace-link)
(ace-link-setup-default)

;; Notational Velocity-style note taking for emacs
(require 'deft)
(setq deft-directory "~/Dropbox/notes")
(setq deft-default-extension "org")
(setq deft-use-filter-string-for-filename t)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 0)
(evil-set-initial-state 'deft-mode 'insert)
(evil-define-key 'insert deft-mode-map (kbd "C-h") 'deft-filter-decrement)
(evil-define-key 'insert deft-mode-map (kbd "C-w") 'deft-filter-decrement-word)
(evil-define-key 'insert deft-mode-map (kbd "C-n") 'next-line)
(evil-define-key 'insert deft-mode-map (kbd "C-p") 'previous-line)
(add-to-list 'auto-mode-alist '("/Notational Data/.*\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("/Notational Data/.*\\.txt_archive\\'" . org-mode))

;; Flycheck syntax checking
;; TODO: Broken after Emacs 25 upgrade
;(add-hook 'after-init-hook #'global-flycheck-mode)
;(with-eval-after-load 'flycheck
  ;(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy)))

;; NERDTree for Emacs
(require 'neotree)
(evil-leader/set-key "vn" 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
(add-hook 'neotree-mode-hook 'truncate-lines-hook-fun)
(custom-set-variables '(neo-window-width 40))
(custom-set-variables '(neo-banner-message nil))
(custom-set-variables '(neo-theme 'nerd))

;; Company mode for autocompletion
(add-hook 'org-mode-hook #'company-mode)
(setq company-idle-delay nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
;; (evil-define-key 'insert org-mode-map (kbd "TAB") 'company-complete-common-or-cycle)
;; (evil-define-key 'insert org-mode-map (kbd "<tab>") 'company-complete-common-or-cycle)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1))))
;; Make company play nice with yasnippet, from
;; https://github.com/company-mode/company-mode/blob/master/company-yasnippet.el#L104
(add-hook 'ord-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev :with company-yasnippet)))))
