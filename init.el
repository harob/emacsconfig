;; Uncomment this if there are any errors on startup:
;; (setq debug-on-error t)


;;;; Package management

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)


;;;; General settings

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'emacs-utils)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Make it so that the scratch buffer uses markdown. By default it uses Emacs Lisp mode.
(use-package markdown-mode :defer t)
(setq initial-major-mode 'markdown-mode)

;; Sync environment variables.
;; NOTE(harry) On OSX, run the commands from https://gist.github.com/mcandre/7235205
;; to properly set the PATH when launching from Spotlight, LaunchBar, etc.
(defun sync-env ()
  (when (memq window-system '(mac ns x))
    ;; NOTE(harry) Use ZSH instead of shell-file-name because that points to sh
    (let ((zsh-path "/opt/homebrew/bin/zsh")
          (env-pair-re "^\\([^=[:space:]]+\\)=\\(.*\\)$"))
      (with-temp-buffer
        (shell-command (concat zsh-path " -i -c env") t)
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

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      ring-bell-function 'ignore
      mac-option-modifier 'alt
      mac-command-modifier 'meta)
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Require typing only "y" or "n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.

(savehist-mode t) ; Save your minibuffer history across Emacs sessions. UX win!
(setq savehist-additional-variables '(buffer-name-history kill-ring))

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

;; The preference file for Emac's "Customize" system. M-x `customize' to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Colorscheme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-bright t))
(set-face-attribute 'default nil :family "Consolas" :height 150)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     ;; (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace mode by default marks all whitespace. Show only tabs, trailing space, and trailing lines.
     (setq whitespace-style '(face empty trailing tabs))))
(add-hook 'before-explicit-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'gfm-mode-hook 'display-fill-column-indicator-mode)

;; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word.
(global-visual-line-mode t)

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
(global-hl-line-mode t)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example. If indent-tabs-mode is off, untabify before saving.
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
(util/define-keys minibuffer-local-map
                  (kbd "C-k") 'kill-line
                  (kbd "C-e") 'end-of-line
                  (kbd "C-d") 'delete-char
                  (kbd "C-w") 'backward-delete-word
                  (kbd "C-h") 'backward-delete-char)

;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration with `winner-undo'
(winner-mode t)

;; Save buffers whenever they lose focus.
;; This obviates the need to hit the Save key thousands of times a day. Inspired by http://goo.gl/2z0g5O
(add-hook 'focus-out-hook 'util/save-buffer-if-dirty)
(defadvice windmove-up (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-down (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-left (before other-window-now activate) (util/save-buffer-if-dirty))
(defadvice windmove-right (before other-window-now activate) (util/save-buffer-if-dirty))

; This is fired whenever the buffer list is updated, which is a reasonably robust way to detect that the
; window config has changed and the current buffer should be saved.
(add-hook 'buffer-list-update-hook 'util/save-buffer-if-dirty)

(setq create-lockfiles nil)
(setq eldoc-echo-area-use-multiline-p nil)

;; Don't use the native OSX full screen support, because it uses OSX Spaces which don't play well with
;; CMD-tabbing to applications which are behind Emacs. Invoke fullscreen with `toggle-frame-fullscreen'.
(setq ns-use-native-fullscreen nil)

(global-font-lock-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)

(global-set-key (kbd "RET") 'comment-indent-new-line)

(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode t))


;;;; Evil mode -- Vim keybindings for Emacs.

;; Provide configuration functions for assigning actions to a Vim leader key.
(use-package evil-leader
  :custom
  (evil-leader/leader "SPC")
  ;; Access leader with C-SPC in insert mode:
  (evil-leader/in-all-states t)
  ;; Ensure evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
  (evil-leader/no-prefix-mode-rx '("magit-.*-mode"))
  :config
  ;; This needs to be *before* calling `evil-mode' for the leader key to work in
  ;; all buffer types:
  (global-evil-leader-mode 1))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  ;; Use the "symbol" as the text object for `*' and `#' rather than the "word"
  ;; (e.g. the full variable in Python including underscores, rather than the part
  ;; between underscores). This corresponds to the `o' text object over `w'
  (evil-symbol-word-search t)
  ;; When opening new lines, indent according to the previous line.
  (evil-auto-indent t)
  :config
  (evil-set-undo-system 'undo-redo)
  ;; Unbind these keys in evil so they can instead be used for code navigation.
  (define-key evil-normal-state-map (kbd "M-,") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (evil-mode t))

;; Use M-u since I use vim's C-u for page-up
(global-set-key (kbd "M-u") 'universal-argument)

(use-package goto-last-change)

(use-package which-key
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-operator-state-maps t)
  :config
  (which-key-mode 1))

(defmacro my-which-key-with-evil-leader (&rest key-desc-pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(which-key-add-key-based-replacements
                    (concat evil-leader/leader " " ,(car pair)) ,(cadr pair)))
               (seq-partition key-desc-pairs 2))))

;; Move up and down through long, wrapped lines one visual line at a time.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)

;; I use this shortcut for manually splitting lines. Note that it does not put you in insert mode.
(define-key evil-normal-state-map (kbd "s") 'newline-and-indent)

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
  ;; `forward-paragraph', which uses these variables.
  (let ((paragraph-start "\f\\|[     ]*$")
        (paragraph-separate "[  ]*$"))
    (evil-select-an-object 'evil-paragraph beg end type count)))

(define-key evil-outer-text-objects-map "p" 'evil-paragraph-from-newlines)
(define-key evil-outer-text-objects-map "P" 'evil-a-paragraph)

(evil-leader/set-key
  "SPC" 'execute-extended-command
  "h" 'help
  ";" 'eval-expression
  "t" 'affe-find
  "b" 'consult-buffer ; Includes all buffers and recent files by default. Type p SPC to narrow to just the current project
  "a" 'consult-ripgrep
  "/" 'consult-line
  "V" 'consult-yank-from-kill-ring
  "u" 'universal-argument
  "\\" (lambda () (interactive) (split-window-horizontally) (other-window 1) (balance-windows))
  "-" (lambda () (interactive) (split-window-vertically) (other-window 1) (balance-windows))
  "gs" (lambda() (interactive) (util/save-buffer-if-dirty) (magit-status))
  "gl" 'magit-log-current
  ;; "v" is a mnemonic prefix for "view X".
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "vh" (lambda () (interactive) (find-file "~/workspace/src/liftoff/haggler/src/haggler/handler.clj"))
  "vi" (lambda () (interactive) (find-file "~/Dropbox/notes/inbox.org") (org-mode))
  "vs" (lambda () (interactive) (find-file "~/Dropbox/notes/scratch.org") (org-mode))
  "vt" (lambda () (interactive) (find-file "~/Dropbox/notes/tasks.org") (org-mode))
  "vz" (lambda () (interactive) (find-file "~/dotfiles/.zshrc")))

(my-which-key-with-evil-leader
  "c" "Comment"
  "e" "Evaluate"
  "g" "Git"
  "i" "Insert"
  "r" "Render"
  "v" "View"
  "w" "Window"
  "C" "Copilot chat")

;; Enable the typical Bash/readline keybindings when in insert mode.
(util/define-keys evil-insert-state-map
                  (kbd "C-h") 'backward-delete-char
                  (kbd "C-k") 'kill-line
                  (kbd "C-a") 'beginning-of-line
                  (kbd "C-e") 'end-of-line
                  (kbd "C-d") 'delete-char
                  (kbd "C-w") 'backward-delete-word
                  (kbd "C-p") 'previous-line
                  (kbd "C-n") 'next-line)

;; Press % to jump to matching delimiter
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map " cc" 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map " cc" 'evilnc-comment-operator))

(use-package evil-surround
  :config
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gs" 'evil-Surround-region)
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode 1))


;;;; Window manipulation, switching, & management.

;; Settings for window splits.
(setq split-height-threshold 40
      split-width-threshold 200
      split-window-preferred-function 'split-window-sensibly-reverse)

;; "I manage my windows in a 4x4 grid. I want ephemeral or status-based buffers to always show in the
;; lower-right or right window, in that order of preference."
(setq special-display-buffer-names '("*Help*" "*compilation*" "COMMIT_EDITMSG" "*Messages*"
                                     "*magit-process*" "*magit-commit*" "*Compile-Log*" "*Gofmt Errors*"))
(setq special-display-regexps '("*cider.*" "magit: .*" "magit-log: .*"))
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
;; The implementation of this function is based on `special-display-popup-frame' in window.el.
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
  "Dismisses any visible windows in the current frame identifiedy by `special-display-buffer-names' and
   `special-display-regexps'. I use this to quickly dismiss help windows, compile output, etc."
  (interactive)
  (save-excursion
    (let ((original-window (selected-window)))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (special-display-p (buffer-name buffer))
            (quit-window nil window))))
      (select-window original-window))))

(defun split-window-sensibly-reverse (&optional window)
  "Identical to the built-in function `split-window-sensibly', but prefers horizontal splits over vertical splits."
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


;;;; Incremental search (isearch)

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
  perform the same function as `kill-word', but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on `isearch-del-char', from isearch.el."
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

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))


;;;; Mac OS X keybindings minor mode.

;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
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
                  (kbd "M-W") 'evil-quit ; Close all tabs in the current frame
                  )

(define-minor-mode osx-keys-minor-mode
  "A minor-mode for emulating osx keyboard shortcuts."
  :global t
  :lighter " osx"
  :keymap osx-keys-minor-mode-map)

(osx-keys-minor-mode t)

(defadvice load (after give-osx-keybindings-priority activate)
  "Try to ensure that osx keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'osx-keys-minor-mode))
      (let ((osx-keys (assq 'osx-keys-minor-mode minor-mode-map-alist)))
        (setq minor-mode-map-alist (assq-delete-all 'osx-keys-minor-mode minor-mode-map-alist))
        (add-to-list 'minor-mode-map-alist osx-keys))))
(ad-activate 'load)

(defun open-folder-in-finder ()
  "Opens the folder of the current file in OSX's Finder."
  (interactive)
  (call-process-region nil nil "/usr/bin/open" nil nil nil "."))

(defun vimlike-quit ()
  "Closes the current window, tab, or if there's only one tab, use the `:q' Evil
   command. This simulates the `:q' behavior of Vim when used with tabs."
  (interactive)
  (let ((one-tab (= 1 (length (tab-bar-tabs))))
        (one-window (one-window-p)))
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      )
     ; if there are multiple tabs, close the current tabs
     ((not one-tab)
      (tab-close))
     (one-tab
      (evil-quit)))))

(defvar before-explicit-save-hook nil)
(defvar after-explicit-save-hook nil)

(defun explicitly-save-buffer ()
  (interactive)
  (run-hooks 'before-explicit-save-hook)
  (save-buffer)
  (run-hooks 'after-explicit-save-hook))


;;;; Fancy fuzzy-finding for buffers, files, text, and more!

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 20))

(setq enable-recursive-minibuffers t)

;; Marginalia for rich annotations in the minibuffer
(use-package marginalia :after vertico
  :init
  (marginalia-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  ;; (corfu-preselect 'prompt) ;; Preselect the prompt
  ;; NOTE(harry) Try this for a while. If it's too annoying, turn it off and
  ;; switch to using S-SPC to call `corfu-insert-separator' to activate orderless
  ;; matching instead.
  (corfu-quit-at-boundary nil) ;; Do orderless matching upon pressing SPC rather than exiting
  :bind (:map corfu-map
              ("TAB" . corfu-next) ;; Navigate candidates with TAB
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous) ;; Navigate backward with Shift+TAB
              ([backtab] . corfu-previous)))

(evil-define-key 'insert prog-mode-map (kbd "TAB") 'completion-at-point)
(evil-define-key 'insert prog-mode-map (kbd "<tab>") 'completion-at-point)
(evil-define-key 'insert org-mode-map (kbd "TAB") 'completion-at-point)
(evil-define-key 'insert org-mode-map (kbd "<tab>") 'completion-at-point)

(use-package corfu-popupinfo :after corfu
  :ensure nil; Included with corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.1 . 0.1)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

;; For some reason org-mode overwrites `completion-at-point-functions', so
;; re-overwrite it here:
(defun my-org-mode-completion-setup ()
  (setq-local completion-at-point-functions (list #'cape-file #'cape-dabbrev)))
(add-hook 'org-mode-hook #'my-org-mode-completion-setup)

(use-package consult
  :custom
  ; Sample usage: in consult-buffer, press < then b to narrow to just open buffers.
  ; The same can be achieved by pressing b then SPC
  (consult-narrow-key "<")
  :bind
  (("M-x" . execute-extended-command)))

(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; Increase the number of saved items
(setq recentf-auto-cleanup 'never) ;; Disable automatic cleanup

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :bind
  (;; Pops up a giant menu of options. Press C-h to search within it
   ("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure nil
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; For fzf-style `affe-find'
(use-package affe
  :custom
  (affe-regexp-compiler #'affe-orderless-regexp-compiler))

;; Mysterious code recommended at https://github.com/minad/affe
(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))

;; Allows batch find-and-replace with
;; `consult-ripgreg' -> `embark-export' -> `wgrep-change-to-wgrep-mode' -> ZZ or ZQ
(use-package wgrep :defer t
  :custom
  (wgrep-auto-save-buffer t))

(add-to-list 'evil-insert-state-modes 'wgrep-mode)
(evil-define-key 'normal wgrep-mode-map "ZQ" 'wgrep-abort-changes)
(evil-define-key 'normal wgrep-mode-map "ZZ" 'wgrep-finish-edit)


;;;; Emacs Lisp (elisp)

;; Treat hyphens as part of words:
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))

(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "M-h") 'shift-sexp-backward
  (kbd "M-l") 'shift-sexp-forward
  "K" 'describe-function)

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
  "eb" (lambda () (interactive) (util/save-buffer-if-dirty) (eval-buffer))
  "es" (lambda () (interactive) (util/save-buffer-if-dirty) (elisp-eval-current-sexp))
  "ex" (lambda () (interactive) (util/save-buffer-if-dirty) (call-interactively 'eval-defun)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local completion-at-point-functions
                                 (list #'elisp-completion-at-point
                                       #'cape-elisp-symbol
                                       #'cape-elisp-block
                                       #'cape-file
                                       #'cape-dabbrev))))

;; Run `git clone https://git.savannah.gnu.org/git/emacs.git` to get the emacs
;; source code.
(setq find-function-C-source-directory "~/workspace/external_codebases/emacs/src")


;;;; Org mode, for GTD and note taking.

(require 'org-mode-personal)


;;;; tab-bar-mode (tabs on the window).

(use-package tab-bar
  :ensure nil
  :custom
  tab-bar-select-tab-modifiers '(meta)
  tab-bar-tab-hints t
  tab-bar-show 1 ; hide bar if <= 1 tabs open
  tab-bar-close-button-show nil
  :bind (:map evil-normal-state-map
              ("M-t" . 'tab-new)
              ("M-}" . 'tab-next)
              ("M-{" . 'tab-previous))
  :config
  (tab-bar-mode 1))



;;;; Diminish - hide or shorten the names of minor modes in your modeline.

;; To see which minor modes you have loaded and what their modeline strings are run `(message minor-mode-alist)`
(use-package diminish
  :config
  (diminish 'visual-line-mode "")
  (diminish 'global-whitespace-mode "")
  (diminish 'auto-fill-function "")
  (diminish 'osx-keys-minor-mode ""))


;;;; Markdown

(setq markdown-command
      (concat
       "/opt/homebrew/bin/pandoc"
       " --from markdown --to html"
       " --metadata title='-'"
       ;; Use Gmail's default styling, so I can copy exported HTML into the Compose window with no reformatting:
       " --include-in-header $HOME/.emacs.d/resources/gmail.css"))

(setq markdown-fontify-code-blocks-natively t)


;;;; CSS

(add-hook 'css-mode-hook (lambda ()
                           ;; (autopair-mode 1) ; Auto-insert matching delimiters.
                           ;; Properly unindent a closing brace after you type it and hit enter.
                           (electric-indent-mode)))


;;;; Ruby

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; Insert matching delimiters; unindent end blocks after you type them.
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode)))


;;;; Rainbow-delimiters: highlight parentheses in rainbow colors.

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))


;;;; Smartparens utility functions

(use-package smartparens :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem)

  (sp-with-modes '(clojure-mode)
    (sp-local-pair "`" "`" :when '(sp-in-string-p)))

  (sp-with-modes '(org-mode)
    (sp-local-pair "=" nil :actions :rem)
    (sp-local-pair "~" nil :actions :rem))

  :bind (("M-H" . sp-forward-slurp-sexp)
         ("M-L" . sp-forward-barf-sexp)))

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


;;;; Clojure

(use-package clojure-mode :defer t)
(require 'clojure-mode-personal)
(require 'cider-test-personal)

(evil-leader/set-key-for-mode 'clojure-mode
  "ett" 'cider-test/run-test-at-point
  "etb" 'cider-test/run-tests-in-ns)

(which-key-add-major-mode-key-based-replacements 'clojure-mode
  "SPC e t" "EvaluateTests")

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


;;;; HTML

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
  "ii" 'indent-html-buffer
  "rr" 'preview-html)

;; NOTE(harry) If this doesn't work out try multi-web-mode or mmm-mode. See:
;; http://www.emacswiki.org/emacs/MultipleModes
;; http://stackoverflow.com/questions/4462393/how-do-i-configure-emacs-for-editing-html-files-that-contain-javascript
(use-package web-mode :defer t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)


;;;; YAML

(use-package yaml-mode :defer t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;;; Go

(use-package go-mode :defer t
  :config
  (evil-define-key 'normal go-mode-map "K" 'godef-describe))

(defun go-save-and-compile-fn (command)
  "Returns a function for the purpose of binding to a key which saves the current buffer and then
   runs the given command in the root of the go project."
  (lexical-let ((command command))
    #'(lambda ()
        (interactive)
        (go-save-and-compile command))))

(defun go-save-and-compile (command)
  "Saves the current buffer before invoking the given command."
  (when-let ((project-dir (locate-dominating-file default-directory ".git")))
    (save-buffer)
    (message command)
    (util/without-confirmation
     (lambda () (compile (concat "cd " project-dir " && " command))))))

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
  ;; Note that `gofmt-before-save' triggers this save-hook for some reason, so we lock on gofmt-in-progress to
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


;;;; Magit - for staging hunks and making commits to git

;; For large repos, check what's slow with M-x `magit-toggle-verbose-refresh'
;; and looking at the times in *Messages*. Then disable the slowest sections
;; by filling in /path/to/repo/.dir-locals.el similarly to
;; ./.dir-locals.el.example_for_big_repos (or just directly symlink it).
(use-package magit :defer t
  :config
  (setq magit-commit-show-diff nil)
  (add-hook 'git-commit-mode-hook (lambda () (interactive) (evil-append-line 1))))

;; NOTE(harry) Turning off due to some display bugs
;; (use-package magit-delta :after (magit) :defer t
;;   :hook (magit-mode . magit-delta-mode))


;;;; Javascript

(setq js-indent-level 2)


;;;; Lua

(setq lua-indent-level 2)


;;;; Misc

;; Switch across windows (i.e. panes/splits)
(define-key evil-normal-state-map (kbd "C-h") (lambda () (interactive) (ignore-errors (evil-window-left 1))))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (ignore-errors (evil-window-down 1))))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (ignore-errors (evil-window-up 1))))
(define-key evil-normal-state-map (kbd "C-l") (lambda () (interactive) (ignore-errors (evil-window-right 1))))

(use-package buffer-move
  :bind (("C-S-k" . buf-move-up)
         ("C-S-j" . buf-move-down)
         ("C-S-h" . buf-move-left)
         ("C-S-l" . buf-move-right)))

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)

(define-key evil-normal-state-map (kbd "zz") 'evil-scroll-line-to-center)

(define-key osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M-+") 'text-scale-increase)
(define-key osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-increase 0)))

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

(setq tramp-default-method "pscp")

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

;; Go-to-definition for all languages, using rg
(use-package dumb-jump :defer t
  :config
  ;; Forcing the use of ripgrep because for some reason the default git-grep doesn't work
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package avy
  :config
  (setq avy-keys (number-sequence ?a ?z)
        avy-all-windows 'all-frames)
  (evil-leader/set-key "z" 'avy-goto-word-0)
  (define-key evil-motion-state-map (kbd "z") 'avy-goto-word-0)
  (define-key evil-visual-state-map (kbd "z") 'avy-goto-word-0)
  (evil-leader/set-key "Z" 'avy-goto-line)
  (define-key evil-motion-state-map (kbd "Z") 'avy-goto-line)
  (define-key evil-visual-state-map (kbd "Z") 'avy-goto-line))

;; Open links vimium-style with `o' in various help-like modes
(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package typescript-mode :defer t)
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

(use-package browse-at-remote :defer t)

;; Treemacs file browser pane -- start with M-x treemacs-projectile
(use-package treemacs :defer t)
(use-package treemacs-evil :after (treemacs evil))
;; (use-package treemacs-projectile :after (treemacs projectile) :defer t)


;; Custom modeline -- run M-x nerd-icons-install-fonts to install the fonts
(use-package evil-anzu
  :config
  ;; To display search result count in modeline
  (global-anzu-mode t))

(use-package doom-modeline :after (evil-anzu)
  :config
  (doom-modeline-mode 1))


;;;; Generic insertion of TODO et al

(defun insert-todo ()
  "Insert a TODO comment appropriate for the current mode."
  (interactive)
  (insert (concat (comment-string) " TODO(harry) ")))

(defun insert-note ()
  "Insert a NOTE comment appropriate for the current mode."
  (interactive)
  (insert (concat (comment-string) " NOTE(harry) ")))

(defun insert-fixme ()
  "Insert a FIXME comment appropriate for the current mode."
  (interactive)
  (insert (concat (comment-string) " FIXME(harry) ")))

(defun comment-string ()
  "Get the comment string appropriate for the current mode."
  (cond
   ((member major-mode '(emacs-lisp-mode lisp-mode scheme-mode clojure-mode))
    ";;")
   ((member major-mode '(python-mode ruby-mode sh-mode conf-toml-mode))
    "#")
   (t "//") ; Default
   ))

(evil-leader/set-key
  "it" 'insert-todo
  "in" 'insert-note
  "if" 'insert-fixme)



;;;; Python

;; Configuration inspired by https://www.naiquev.in/understanding-emacs-packages-for-python.html

;; First run `pip install -U jedi-language-server`
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  :hook (python-mode . eglot-ensure))

(use-package flymake-ruff
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  ;; This makes flymake only run on save:
  ;; (flymake-no-changes-timeout nil)
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package flymake-cursor :after (flymake)
  :hook (python-mode-hook . flymake-cursor-mode))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (message "Setting up eglot-managed-mode-hook")
            ;; Show flymake diagnostics first.
            (setq eldoc-documentation-functions
                  (cons #'flymake-eldoc-function
                        (remove #'flymake-eldoc-function eldoc-documentation-functions)))
            ;; Show all eldoc feedback.
            (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))

(use-package reformatter :defer t
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

(add-hook 'python-mode-hook (lambda () (interactive) (set-fill-column 88)))

(defun my-patch-python-pytest-executable ()
  "My work git repo has many projects inside it, so pytest needs to know to use
   the nested project rather than the parent repo as its working directory."
  (when-let ((dir (locate-dominating-file default-directory "build.toml")))
    (setq python-pytest-executable (concat "cd " dir " && kirin test")
          project-compilation-dir dir)))

(use-package python-pytest :defer t
  :custom
  (python-pytest-unsaved-buffers-behavior 'save-current)
  ;; This is projectile by default, which has bad test-finding logic:
  (python-pytest-preferred-project-manager 'project)
  :hook
  (python-mode . my-patch-python-pytest-executable))

(defun ruff-fix-imports ()
  "Reorder imports in the current buffer using ruff."
  (interactive)
  (if buffer-file-name
      (let ((command (format "ruff check %s --select I --fix" (shell-quote-argument buffer-file-name))))
        (shell-command command))
    (message "No file is associated with the current buffer.")))


;;;; AI

;; To set up gptel, add these lines to ~/.authinfo:
;; machine api.openai.com login apikey password <openai dev token>
;; machine api.anthropic.com login apikey password <anthropic API key>
;;
;; Main commands: gptel-send, gptel-rewrite, gptel-menu
(use-package gptel :defer t
  :custom
  (gptel-org-branching-context t)
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-backend (gptel-make-anthropic "Claude"
                        :key (gptel-api-key-from-auth-source "api.anthropic.com")
                        :stream t)))

(use-package copilot-chat :after (magit) :defer t
  :config
  (evil-leader/set-key
    "CC" 'copilot-chat-transient           ; Show menu
    "Ca" 'copilot-chat-add-current-buffer  ; Add the current buffer to the Copilot chat list
    "Cc" 'copilot-chat-display             ; Display the Copilot chat window
    "Ce" 'copilot-chat-explain             ; Explain the selected region using Copilot chat
    "Cp" 'copilot-chat-custom-prompt-selection))  ; Send a custom prompt followed by
                                               ; the selected region to Copilot chat

;; Github Copilot autocomplete support
;; Run M-x `copilot-install-server' then M-x `copilot-login' first.
;; Check M-x `copilot-diagnose' if there's an issue.
;; (use-package editorconfig)
;; (use-package jsonrpc)
;; (use-package copilot :after (editorconfig jsonrpc)
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")
;;   :init
;;   ;; copilot is broken for me by default because the Github Copilot Server requires
;;   ;; node 18, but my work repo forces node 16. I'm hacking it by hardcoding the
;;   ;; path here:
;;   (setq copilot-node-executable "/opt/homebrew/Cellar/node/23.9.0/bin/node")
;;   :bind
;;   (:map copilot-completion-map
;;         ("A-<tab>" . 'copilot-accept-completion)
;;         ("A-TAB" . 'copilot-accept-completion)
;;         ("A-S-<tab>" . 'copilot-accept-completion-by-word)
;;         ("A-S-TAB" . 'copilot-accept-completion-by-word))

;;   :config
;;   (add-hook 'prog-mode-hook 'copilot-mode)
;;   (setq copilot-indent-offset-warning-disable t
;;         copilot-max-char-warning-disable t))
