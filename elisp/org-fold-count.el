;;; org-fold-count.el --- Show line counts on folded org headings -*- lexical-binding: t; -*-

(require 'org)

(defcustom my-org-fold-count-format " (%d)"
  "Format string for the fold-count suffix. Receives the line count."
  :type 'string
  :group 'org)

(defface my-org-fold-count-face
  '((((background dark)) :foreground "gray35")
    (t :foreground "gray60"))
  "Face for the fold-count suffix on folded org headings."
  :group 'org)

(defun my-org-fold-count--refresh-buffer ()
  "Refresh fold-count overlays across the whole buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (remove-overlays (point-min) (point-max) 'my-org-fold-count t)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((eol (line-end-position)))
          (if (org-invisible-p eol)
              (let* ((fold-end (next-single-char-property-change eol 'invisible))
                     (line-count (count-lines (min (1+ eol) (point-max)) fold-end))
                     (last-pos (max (point-min) (1- eol)))
                     (orig (buffer-substring last-pos eol))
                     (ov (make-overlay last-pos eol)))
                (overlay-put ov 'my-org-fold-count t)
                (overlay-put ov 'display
                             (concat orig
                                     (propertize (format my-org-fold-count-format line-count)
                                                 'face 'my-org-fold-count-face)))
                (goto-char fold-end))
            (goto-char eol)))))))

(defun my-org-fold-count--on-cycle (&rest _)
  (my-org-fold-count--refresh-buffer))

(define-minor-mode my-org-fold-count-mode
  "Show line counts as a suffix on folded org headings."
  :lighter nil
  (if my-org-fold-count-mode
      (progn
        (add-hook 'org-cycle-hook #'my-org-fold-count--on-cycle nil t)
        (my-org-fold-count--refresh-buffer))
    (remove-hook 'org-cycle-hook #'my-org-fold-count--on-cycle t)
    (remove-overlays (point-min) (point-max) 'my-org-fold-count t)))

(provide 'org-fold-count)
;;; org-fold-count.el ends here
