;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)
(require 'ox-html-personal)
(require 'ox-md-personal)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "t" 'org-todo
  "T" '(lambda () (interactive)
         (let ((original-val org-log-done))
           (setq org-log-done nil)
           (org-todo 'done)
           (setq org-log-done original-val)))
  "o" '(lambda () (interactive)
         (org-insert-heading-after-current)
         (evil-end-of-line)
         (evil-append nil))
  "O" '(lambda () (interactive)
         (org-insert-heading-after-current)
         (evil-end-of-line)
         (org-metaup)
         (evil-append nil))
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  "gu" 'outline-up-heading
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle
  (kbd "RET") 'org-open-at-point)

(use-package org-mac-link :ensure t :defer t)

(evil-leader/set-key-for-mode 'org-mode
  "li" 'org-insert-link
  "ly" 'org-store-link
  "lp" 'org-insert-link
  "lt" 'org-toggle-link-display
  "lc" 'org-mac-link-get-link
  "lo" 'ace-link-org
  "oa" 'org-archive-subtree
  "od" 'org-time-stamp
  "oo" 'my-org-insert-subheading
  "or" 'avy-org-refile-as-child
  "ot" 'org-set-tags-command
  "va" 'org-agenda
  "rr" 'org-export-dispatch)

(which-key-add-major-mode-key-based-replacements 'org-mode
  "SPC l" "org-Links"
  "SPC o" "Org")

(defun my-org-insert-subheading ()
  (interactive)
  (evil-end-of-line)
  (evil-append nil)
  (org-insert-subheading nil))

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-o") 'org-insert-todo-heading))
      '(normal insert))
(evil-define-key 'normal org-mode-map
  (kbd "M-L") 'org-shiftmetaright
  (kbd "M-H") 'org-shiftmetaleft)

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

(define-key org-mode-map "\M-t" nil)

;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
;; many stars.
(setq org-startup-indented t)
(setq org-fontify-done-headline nil)
(setq org-startup-folded t)

(setq org-src-fontify-natively t)
(setq org-todo-keywords '((sequence "TODO" "IP" "WAIT" "|" "DONE")))
(setq org-todo-keyword-faces '(("IP" . (:foreground "cyan3" :weight bold))
                               ("WAIT" . (:foreground "MediumOrchid3" :weight bold))))

(setq org-log-done 'time)
(setq org-agenda-files '("~/Dropbox/notes/tasks.org" "~/Dropbox/notes/inbox.org"))

(setq org-link-search-must-match-exact-headline nil)

(custom-set-variables
 '(org-confirm-babel-evaluate nil)
 '(org-babel-load-languages '((clojure . t)
                              (emacs-lisp . t)
                              (R . t))))

(use-package org-download :ensure t :defer t
  :config
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "images")
  (setq org-image-actual-width '(400)))

(setq org-timer-display 'both)

;; Minimalistic export settings:
(setq org-export-with-title nil)
(setq org-export-with-toc nil)
(setq org-export-headline-levels 0)
(setq org-export-with-section-numbers 0)
(setq org-export-with-author nil)
(setq org-export-with-date nil)
(setq org-export-time-stamp-file nil)
(setq org-html-validation-link nil)
;; Use Gmail's default styling, so I can copy exported HTML into the Compose window with no reformatting:
(setq org-html-head "<style type=\"text/css\">body {font-size: small; font-family: arial, helvetica, sans-serif; line-height: 1.5;}</style>")

;; Workaround for global-auto-revert-mode apparently not working for org buffers when it's called upfront on
;; startup:
(add-hook 'org-mode-hook (lambda () (global-auto-revert-mode)))

(provide 'org-mode-personal)
