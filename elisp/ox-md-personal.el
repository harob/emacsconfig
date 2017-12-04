(require 'ox-md)

(provide 'ox-md-personal)

(defun org-md-separate-elements (tree backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are three
exceptions to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. Outside of plain lists, preserve blank lines between
     a paragraph and a plain list,

  3. In an item, remove any blank line before the very first
     paragraph and the next sub-list.

Assume BACKEND is `md'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (cond
       ((not (and (eq (org-element-type e) 'paragraph)
                  (eq (org-element-type (org-export-get-next-element e info))
                      'plain-list)))
        (org-element-put-property e :post-blank 1))
       ((not (eq (org-element-type (org-element-property :parent e)) 'item)))
       (t (org-element-put-property
           e :post-blank (if (org-export-get-previous-element e info) 1 0))))))
  ;; Return updated tree.
  tree)

;; Override ox-md function to fix up heading rendering to act more like a regular markdown list:
(defun org-md-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "     :%s:"
                                     (mapconcat 'identity tag-list ":"))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           (anchor
            (when (plist-get info :with-toc)
              (org-html--anchor
               (or (org-element-property :CUSTOM_ID headline)
                   (concat "sec-"
                           (mapconcat 'number-to-string
                                      (org-export-get-headline-number
                                       headline info) "-"))))))
           ;; Headline text without tags.
           (heading (concat todo priority title)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq org-md-headline-style '(atx setext)))
            (and (eq org-md-headline-style 'atx) (> level 6))
            (and (eq org-md-headline-style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet " " heading tags
                  "\n"
                  (and contents
                       (replace-regexp-in-string "^" "    " contents)))))
       ;; Use "Setext" style.
       ((eq org-md-headline-style 'setext)
        (concat heading tags anchor "\n"
                (make-string (length heading) (if (= level 1) ?= ?-))
                "\n\n"
                contents))
       ;; Use "atx" style.
       (t (concat (make-string level ?#) " " heading tags anchor "\n\n" contents))))))
