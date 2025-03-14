(require 'ox-html)

;; NOTE(harry) This function has been changed to remove linebreaks after "heading" bullets.
(defun org-html-format-list-item (contents type checkbox info
             &optional term-counter-id
             headline)
  "Format a list item into HTML."
  (let ((class (if checkbox
       (format " class=\"%s\""
         (symbol-name checkbox)) ""))
  (checkbox (concat (org-html-checkbox checkbox info)
        (and checkbox " ")))
  (br (org-html-close-tag "br" nil info))
  (extra-newline (if (and (org-string-nw-p contents) headline) "\n" "")))
    (concat
     (pcase type
       (`ordered
  (let* ((counter term-counter-id)
         (extra (if counter (format " value=\"%s\"" counter) "")))
    (concat
     (format "<li%s%s>" class extra)
     headline)))
       (`unordered
  (let* ((id term-counter-id)
         (extra (if id (format " id=\"%s\"" id) "")))
    (concat
     (format "<li%s%s>" class extra)
     headline)))
       (`descriptive
  (let* ((term term-counter-id))
    (setq term (or term "(no term)"))
    ;; Check-boxes in descriptive lists are associated to tag.
    (concat (format "<dt%s>%s</dt>"
        class (concat checkbox term))
      "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     extra-newline
     (and (org-string-nw-p contents) (org-trim contents))
     extra-newline
     (pcase type
       (`ordered "</li>")
       (`unordered "</li>")
       (`descriptive "</dd>")))))

(provide 'ox-html-personal)
