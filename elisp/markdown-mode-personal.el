;;; markdown-mode-personal.el --- My additions and customizations to markdown-mode -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'evil)
(require 'evil-leader)
(require 'markdown-mode)
(require 'reformatter)

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
  ;; Match dprint, which re-indents nested list items by 2 on every save.
  (markdown-list-indent-width 2)
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

;; Org-style folding. TAB on a heading or list item cycles it through
;; FOLDED -> CHILDREN -> SUBTREE, and S-TAB cycles the whole buffer through
;; OVERVIEW -> CONTENTS -> SHOW ALL. Headings and list items form one tree: a
;; heading's children are its sub-headings and the top-level items under it,
;; and an item's children are its nested items. Depth in that tree (rather than
;; the number of #s) decides what OVERVIEW shows, so a file whose outermost
;; headings are ## shows those.
(cl-defstruct (my-markdown--node (:constructor my-markdown--make-node))
  start   ; beginning of the node's line
  eol     ; end of the node's line, where its hidden body begins
  end     ; end of the last line of its subtree
  depth   ; number of enclosing nodes
  heading-level  ; number of #s, or nil for a list item
  indent) ; indentation of a list item

(defun my-markdown--nodes ()
  "Return the buffer's headings and list items as nodes, in buffer order."
  (save-excursion
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (let (nodes open (last-text-eol (point-min)))
      (cl-flet ((close-while (pred end)
                  (while (and open (funcall pred (car open)))
                    (setf (my-markdown--node-end (pop open)) end))))
        (while (not (eobp))
          (let ((bol (point))
                (eol (line-end-position))
                (code (markdown-code-block-at-point-p)))
            (cond
             ((and (not code) (looking-at markdown-regex-header-atx))
              (let ((level (length (match-string 1))))
                ;; A heading ends every open list item and every heading at its
                ;; level or deeper. Headings keep their trailing blank lines so
                ;; folded headings sit together.
                (close-while (lambda (n)
                               (let ((l (my-markdown--node-heading-level n)))
                                 (or (null l) (>= l level))))
                             (max (point-min) (1- bol)))
                (push (my-markdown--make-node
                       :start bol :eol eol :depth (length open)
                       :heading-level level)
                      nodes)
                (push (car nodes) open)))
             ((looking-at-p "[ \t]*$"))
             (t
              ;; Any other text ends the list items it isn't indented under.
              (let ((indent (current-indentation)))
                (close-while (lambda (n)
                               (let ((i (my-markdown--node-indent n)))
                                 (and i (>= i indent))))
                             last-text-eol)
                (when (and (not code) (looking-at-p markdown-regex-list))
                  (push (my-markdown--make-node
                         :start bol :eol eol :depth (length open)
                         :indent indent)
                        nodes)
                  (push (car nodes) open)))))
            (unless (looking-at-p "[ \t]*$")
              (setq last-text-eol eol)))
          (forward-line 1))
        (close-while #'identity last-text-eol))
      (nreverse nodes))))

(defun my-markdown--node-at-line (nodes)
  "Return the node in NODES that starts on the current line, if any."
  (let ((bol (line-beginning-position)))
    (cl-find bol nodes :key #'my-markdown--node-start)))

(defun my-markdown--fold (node &optional end)
  "Hide NODE's body, up to END if given."
  (let ((end (min (or end (point-max)) (my-markdown--node-end node))))
    (when (> end (my-markdown--node-eol node))
      (outline-flag-region (my-markdown--node-eol node) end t))))

(defun my-markdown--unfold (node)
  "Show NODE's whole subtree."
  (outline-flag-region (my-markdown--node-eol node) (my-markdown--node-end node) nil))

(defvar-local my-markdown--cycle-state nil
  "State `my-markdown-cycle' left the last node it cycled in.")

(defun my-markdown-cycle ()
  "Cycle the heading or list item at point like `org-cycle'.
Goes FOLDED -> CHILDREN -> SUBTREE."
  (interactive)
  (let* ((nodes (my-markdown--nodes))
         (node (or (my-markdown--node-at-line nodes)
                   (user-error "Not on a heading or list item")))
         (depth (my-markdown--node-depth node))
         (children (cl-remove-if-not
                    (lambda (n)
                      (and (= (my-markdown--node-depth n) (1+ depth))
                           (< (my-markdown--node-start node)
                              (my-markdown--node-start n)
                              (my-markdown--node-end node))))
                    nodes)))
    (setq my-markdown--cycle-state
          (cond
           ((<= (my-markdown--node-end node) (my-markdown--node-eol node))
            (message "EMPTY ENTRY")
            'empty)
           ((and (invisible-p (my-markdown--node-eol node)) children)
            (my-markdown--unfold node)
            (mapc #'my-markdown--fold children)
            (message "CHILDREN")
            'children)
           ((or (invisible-p (my-markdown--node-eol node))
                (and (eq last-command this-command)
                     (eq my-markdown--cycle-state 'children)))
            (my-markdown--unfold node)
            (message (if children "SUBTREE" "SUBTREE (NO CHILDREN)"))
            'subtree)
           (t
            (my-markdown--fold node)
            (message "FOLDED")
            'folded)))))

(defun my-markdown--cycle-filter (cmd)
  "Return CMD when point is on a heading or list item, so TAB falls through
to its usual binding everywhere else."
  (save-excursion
    (beginning-of-line)
    (and (or (looking-at-p markdown-regex-header-atx)
             (looking-at-p markdown-regex-list))
         (not (markdown-code-block-at-point-p))
         cmd)))

(defun my-markdown--show-depth (max-depth)
  "Show only headings and list items at most MAX-DEPTH deep, or everything if nil."
  (outline-flag-region (point-min) (point-max) nil)
  (when max-depth
    (let ((visible (cl-remove-if (lambda (n) (> (my-markdown--node-depth n) max-depth))
                                 (my-markdown--nodes))))
      ;; Hide each visible node's body up to the next visible node, so loose
      ;; text outside every node stays visible.
      (cl-loop for (node next) on visible
               do (my-markdown--fold
                   node (and next (1- (my-markdown--node-start next))))))))

(defvar-local my-markdown--global-cycle-state nil
  "State `my-markdown-global-cycle' last left the buffer in.")

(defun my-markdown-global-cycle ()
  "Cycle the whole buffer like `my-org-global-cycle'.
Repeated invocations go: outermost headings and items -> two levels ->
everything. In a table, move to the previous cell instead."
  (interactive)
  (if (markdown-table-at-point-p)
      (call-interactively #'markdown-table-backward-cell)
    (let ((next (if (eq last-command this-command)
                    (pcase my-markdown--global-cycle-state
                      ('overview 'contents)
                      ('contents 'all)
                      (_ 'overview))
                  'overview)))
      (pcase next
        ('overview (my-markdown--show-depth 0) (message "OVERVIEW"))
        ('contents (my-markdown--show-depth 1) (message "CONTENTS (2 levels)"))
        ('all (my-markdown--show-depth nil) (message "SHOW ALL")))
      (setq my-markdown--global-cycle-state next))))

(evil-define-key 'normal markdown-mode-map
  (kbd "TAB") '(menu-item "" my-markdown-cycle :filter my-markdown--cycle-filter)
  (kbd "<backtab>") #'my-markdown-global-cycle)

;; Format with dprint on save. Outside projects with their own dprint.json this
;; uses the global ~/.config/dprint/dprint.jsonc (from ~/dotfiles). Passing the
;; file's path makes dprint apply a project config's include/exclude rules.
(reformatter-define dprint-markdown
  :program "dprint"
  :args (list "fmt" "--stdin" (or buffer-file-name "md"))
  :lighter " dprint")

(when (executable-find "dprint")
  (add-hook 'markdown-mode-hook #'dprint-markdown-on-save-mode))

(provide 'markdown-mode-personal)
