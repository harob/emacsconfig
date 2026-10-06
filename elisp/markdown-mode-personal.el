;;; markdown-mode-personal.el --- My additions and customizations to markdown-mode -*- lexical-binding: t; -*-

(require 'evil)
(require 'evil-leader)
(require 'markdown-mode)

(use-package markdown-mode
  :custom
  (markdown-command
   (concat
    "/opt/homebrew/bin/pandoc"
    " --from markdown --to html"
    " --metadata title='-'"
    ;; Use Gmail's default styling, so I can copy exported HTML into the Compose
    ;; window with no reformatting:
    " --include-in-header $HOME/.emacs.d/resources/gmail.css"))
  (markdown-fontify-code-blocks-natively t)
  ;; Defer fontification so typing isn't blocked by markdown-mode's slow
  ;; `markdown-match-italic' (which re-scans surrounding text on every change
  ;; to verify candidates aren't inside inline-code spans).
  ;;
  ;; Setting `jit-lock-defer-time' in any buffer creates a *global* idle
  ;; timer that, once present, makes `jit-lock-function' defer fontification
  ;; in every buffer where `jit-lock-defer-time' is nil (because the
  ;; `(not (eq nil 0))' branch of its defer test is t). Bumping the default
  ;; to 0 keeps that test off in those buffers — they fontify immediately
  ;; unless input is pending — so e.g. text pulled in by auto-revert in an
  ;; org buffer is highlighted on the next redisplay rather than sitting
  ;; un-fontified until a scroll forces it.
  :hook (markdown-mode . (lambda () (setq-local jit-lock-defer-time 0.05)))
  :init (setq-default jit-lock-defer-time 0))

(defun my-markdown-render-and-open ()
  "Export the current Markdown buffer to HTML in /tmp and open it in a browser."
  (interactive)
  (let ((output-file (expand-file-name
                      (concat (file-name-base (or (buffer-file-name) "markdown"))
                              ".html")
                      temporary-file-directory)))
    (shell-command
     (concat markdown-command " " (shell-quote-argument (buffer-file-name))
             " > " (shell-quote-argument output-file)))
    (browse-url (concat "file://" output-file))))

(evil-leader/set-key-for-mode 'markdown-mode
  "rr" 'my-markdown-render-and-open)

;; Org-style structure editing. As in my org setup, M-h/M-l promote/demote just
;; the current heading or list item, while M-H/M-L take its children along.
;; Headings are shifted by editing their #s directly, because
;; `markdown-cycle-atx' (used by `markdown-promote') also inserts blank lines
;; around every heading it touches.
(defun my-markdown--atx-heading-at-point-p ()
  "Non-nil (with match data set) if point is on an ATX heading."
  (and (thing-at-point-looking-at markdown-regex-header-atx)
       (not (markdown-code-block-at-point-p))))

(defun my-markdown--shift-heading (delta subtree)
  "Shift the heading at point by DELTA levels; with SUBTREE, its children too."
  (save-excursion
    (let* ((level (length (match-string 1)))
           (headings (list (cons (copy-marker (match-beginning 1)) level))))
      (when subtree
        (end-of-line)
        (catch 'done
          (while (re-search-forward markdown-regex-header-atx nil t)
            (unless (markdown-code-block-at-point-p)
              (let ((child-level (length (match-string 1))))
                (when (<= child-level level) (throw 'done nil))
                (push (cons (copy-marker (match-beginning 1)) child-level)
                      headings))))))
      (dolist (heading headings)
        (unless (<= 1 (+ (cdr heading) delta) 6)
          (user-error "Cannot %s heading further"
                      (if (< delta 0) "promote" "demote"))))
      (dolist (heading headings)
        (goto-char (car heading))
        (delete-char (cdr heading))
        (insert (make-string (+ (cdr heading) delta) ?#))))))

(defun my-markdown--shift-list-item (delta subtree)
  "Shift the list item at point by DELTA levels; with SUBTREE, its children too."
  (let ((bounds (markdown-cur-list-item-bounds)))
    (unless subtree
      ;; Stop before the first nested item, so only the item's own lines move.
      (let ((own-end (save-excursion
                       (goto-char (nth 0 bounds))
                       (forward-line)
                       (while (and (< (point) (nth 1 bounds))
                                   (not (looking-at-p markdown-regex-list)))
                         (forward-line))
                       (min (point) (nth 1 bounds)))))
        (setq bounds (append (list (nth 0 bounds) own-end) (nthcdr 2 bounds)))))
    (if (< delta 0)
        (markdown-promote-list-item bounds)
      (markdown-demote-list-item bounds))))

(defun my-markdown--shift (delta subtree)
  "Shift the heading or list item at point by DELTA; with SUBTREE, its children.
Elsewhere (e.g. tables) fall back to `markdown-promote'/`markdown-demote'."
  (cond
   ((my-markdown--atx-heading-at-point-p) (my-markdown--shift-heading delta subtree))
   ((markdown-cur-list-item-bounds) (my-markdown--shift-list-item delta subtree))
   ((< delta 0) (markdown-promote))
   (t (markdown-demote))))

(defun my-markdown-promote-element ()
  "Promote the heading or list item at point, leaving its children alone."
  (interactive)
  (my-markdown--shift -1 nil))

(defun my-markdown-demote-element ()
  "Demote the heading or list item at point, leaving its children alone."
  (interactive)
  (my-markdown--shift 1 nil))

(defun my-markdown-promote-subtree ()
  "Promote the heading or list item at point along with its children."
  (interactive)
  (my-markdown--shift -1 t))

(defun my-markdown-demote-subtree ()
  "Demote the heading or list item at point along with its children."
  (interactive)
  (my-markdown--shift 1 t))

(dolist (state '(normal insert))
  (evil-define-key state markdown-mode-map
    (kbd "M-h") #'my-markdown-promote-element
    (kbd "M-l") #'my-markdown-demote-element
    (kbd "M-k") #'markdown-move-up
    (kbd "M-j") #'markdown-move-down))
(evil-define-key 'normal markdown-mode-map
  (kbd "M-H") #'my-markdown-promote-subtree
  (kbd "M-L") #'my-markdown-demote-subtree)

(provide 'markdown-mode-personal)
