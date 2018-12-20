;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)
(require 'ox-md-personal)

(provide 'org-mode-personal)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(defun init-org-mode-personal ()
  ;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
  ;; many stars.
  (setq org-startup-indented t))

(eval-after-load 'org '(init-org-mode-personal))

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "t" 'org-todo
  "T" '(lambda () (interactive)
         (let ((original-val org-log-done))
           (setq org-log-done nil)
           (org-todo 'done)
           (setq org-log-done original-val)))
  "o" '(lambda () (interactive)
         (evil-end-of-line nil) ; Necessary to handle multi-visible-line headings
         (evil-insert nil)
         (org-end-of-line)
         (if (outline-invisible-p)
             (org-insert-heading-respect-content)
           (org-insert-heading)))
  "O" '(lambda () (interactive)
         (evil-end-of-line nil) ; Necessary to handle multi-visible-line headings
         (org-insert-heading-respect-content)
         (org-metaup)
         (evil-append nil))
  "^" 'org-beginning-of-line
  "$" 'org-end-of-line
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  "gu" 'outline-up-heading
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle
  (kbd "RET") 'org-open-at-point)

(defun preview-org (beg end)
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max))))
    ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
    (call-process-region beg end "/bin/bash" nil nil nil "-c"
                         "convert_org_to_markdown.rb | markdown_page.rb --css gmail | browser")))

(evil-leader/set-key-for-mode 'org-mode
  "ly" 'org-store-link
  "lp" 'org-insert-link
  "lt" 'org-toggle-link-display
  "lc" 'org-mac-grab-link
  "lo" 'ace-link-org
  "oa" 'org-archive-subtree
  "or" 'avy-org-refile-as-child
  "ot" 'org-set-tags-command
  "vT" 'org-show-todo-tree
  "va" 'org-agenda
  "rr" 'preview-org)

(which-key-add-major-mode-key-based-replacements 'org-mode
  "SPC l" "org-Links"
  "SPC o" "Org")

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

(setq org-src-fontify-natively t)
(setq org-todo-keywords '((sequence "TODO" "IP" "WAIT" "|" "DONE")))
(setq org-todo-keyword-faces '(("IP" . (:foreground "cyan3" :weight bold))
                               ("WAIT" . (:foreground "MediumOrchid3" :weight bold))))

(setq org-log-done 'time)
(setq org-agenda-files '("~/Dropbox/notes/tasks.org" "~/Dropbox/notes/inbox.org"))

(setq org-link-search-must-match-exact-headline nil)

;; linum is horribly slow on large org files
;; Cf https://emacs.stackexchange.com/questions/27771/emacs-24-5-cant-handle-large-org-files
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))

(custom-set-variables
 '(org-confirm-babel-evaluate nil)
 '(org-babel-load-languages '((clojure . t)
                              (emacs-lisp . t)
                              (R . t))))

(add-to-list 'load-path "~/.emacs.d/vendor/org-mac-link")
(require 'org-mac-link)

(require 'org-download)
(setq org-download-method 'directory)
(setq-default org-download-image-dir "images")
(setq org-image-actual-width '(400))

(setq org-timer-display 'both)
