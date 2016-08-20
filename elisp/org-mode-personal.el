;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'evil)
(require 'org)

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
  "T" 'org-set-tags-command
  "o" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "O" '(lambda () (interactive) (evil-org-eol-call
                                 (lambda ()
                                   (org-insert-heading)
                                   (org-metaup))))
  "^" 'org-beginning-of-line
  "$" 'org-end-of-line
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "{" 'org-backward-heading-same-level
  "}" 'org-forward-heading-same-level
  ; I use "gl" for this because it behaves similarly to "goto label" in gmail and elsewhere
  "gl" 'org-goto-top-level-heading
  "gu" 'outline-up-heading
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

(defun preview-org ()
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive)
  ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
  (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c"
                       "convert_org_to_markdown.rb | markdown_page.rb | bcat"))

(evil-leader/set-key-for-mode 'org-mode
  "oa" 'org-archive-subtree
  "vT" 'org-show-todo-tree
  "va" 'org-agenda
  "vv" 'preview-org)

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

(defun always-insert-item ()
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  (end-of-line) ;; This jumps to the end of a potentially wrapped line
  (org-end-of-line) ;; This jumps over the "..." hiding elided bullet points
  (funcall fun)
  (evil-append nil))

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

(setq org-default-notes-file "~/Dropbox/tasks.org")
(define-key org-mode-map "\C-cc" 'org-capture)

(setq org-src-fontify-natively t)
(setq org-todo-keywords '((sequence "TODO" "IP" "|" "DONE")))
(setq org-todo-keyword-faces '(("IP" . (:foreground "cyan3" :weight bold))))
