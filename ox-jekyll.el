;; ox-jekyll -- is a minor mode, which help to export org-mode file
;; as an jekyll-formatted html.

;; Author: Zech XU
;; Created: 2014-05-27
;; Version: 1.0
;; Package-Requires: ((org "8.0"))
;; Keywords: org, jekyll

;; This file is not part of GNU Emacs.

(require 'ox-html)

(defgroup org-export-jekyll nil
  "Options for exporting Org mode files to jekyll HTML."
  :tag "Org Jekyll"
  :group 'org-export
  :version "24.3")


(defcustom org-jekyll-include-yaml-front-matter t
  "If true, then include yaml-front-matter when exporting to html.

If false, then you should include the yaml front matter like this at the top of the file:

#+BEGIN_HTML
---
layout: post
title: Org-mode to GitHub pages with Jekyll
excerpt: Introduce how to use Emacs's Org-mode with Jekyll to generate GitHub Pages
categories: Emacs
tags: [Emacs, org-mode, GitHub, Jekyll]
date: 2013-09-15 22:08
comments: true
keywords: blog
description: Instructions on export org file with Jekyll
---
#+END_HTML"
  :group 'org-export-jekyll
  :type 'boolean)


(defcustom org-jekyll-layout "post"
  "Default layout used in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-categories ""
  "Default space-separated categories in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-published "true"
  "Default publish status in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-path ""
  "Default publish dir for Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-comments "true"
  "Default comments (disqus) flag in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-use-src-plugin nil
   "If t, org-jekyll exporter eagerly uses plugins instead of
org-mode's original HTML stuff. For example:

#+BEGIN_SRC ruby
puts \"Hello world\"
#+END_SRC

makes:

{% codeblock ruby %}
puts \"Hello world\"
{% endcodeblock %}"
  :group 'org-export-jekyll-use-src-plugin
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'jekyll 'html
  :export-block '("HTML" "JEKYLL")
  :menu-entry
  '(?j "Jekyll: export to HTML with YAML front matter."
       ((?H "As HTML buffer" org-jekyll-export-as-html)
        (?h "As HTML file" org-jekyll-export-to-html)))
  :translate-alist
  '((template . org-jekyll-template) ;; add YAML front matter.
    (src-block . org-jekyll-src-block)
    (timestamp . org-jekyll-timestamp)
    (inner-template . org-jekyll-inner-template)) ;; force body-only
  :options-alist
  '((:jekyll-layout "JEKYLL_LAYOUT" nil org-jekyll-layout)
    (:jekyll-categories "JEKYLL_CATEGORIES" nil org-jekyll-categories)
    (:jekyll-published "JEKYLL_PUBLISHED" nil org-jekyll-published)
    (:jekyll-path "JEKYLL_PATH" nil org-jekyll-path)
    (:jekyll-comments "JEKYLL_COMMENTS" nil org-jekyll-comments)))


;;; Internal Filters

(defun org-jekyll-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into jekyll code template format
if `org-jekyll-use-src-plugin` is t. Otherwise, perform as
`org-html-src-block`. CONTENTS holds the contents of the item.
INFO is a plist used as a communication channel."
  (if org-jekyll-use-src-plugin
      (let ((language (org-element-property :language src-block))
            (value (org-remove-indentation
                    (org-element-property :value src-block))))
        (format "{%% codeblock lang:%s %%}\n%s{%% endcodeblock %%}"
                language value))
    (org-export-with-backend 'html src-block contents info)))



;;; Template

(defun org-jekyll-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (if org-jekyll-include-yaml-front-matter
      (concat
       (org-jekyll--yaml-front-matter info)
       contents)
    contents))

(defun org-jekyll-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; PREVIEW mark on the top of article.
   (unless (equal "true" (plist-get info :jekyll-published))
     "<span style=\"background: red;\">PREVIEW</span>")
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))



;;; YAML Front Matter

(defun org-jekyll--get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (format "%s" (if (or (string= property "") (not property))
		     (or default "")
		   property))))

(defun org-jekyll--yaml-front-matter (info)
  (let ((title
         (org-jekyll--get-option info :title))
        (date
         (org-jekyll--get-option info :date
				 (format-time-string "%Y-%m-%d %a %H:%M:%S"
						     (current-time))))
        (layout
         (org-jekyll--get-option info :jekyll-layout
				 org-jekyll-layout))
        (categories
         (org-jekyll--get-option info :jekyll-categories
				 org-jekyll-categories))
        (published
         (org-jekyll--get-option info :jekyll-published 
				 org-jekyll-published))
        (comments
         (org-jekyll--get-option info :jekyll-comments
				 org-jekyll-comments)))
    (unless (equal published "true")
      (setq title (concat "[PREVIEW] " title)))
    (concat
     "---"
     "\ntitle: \"" title
     "\"\ndate: " date
     "\nlayout: " layout
     "\ncategories: " categories
     "\npublished: " published
     "\ncomments: " comments
     "\n---\n")))

;;; Timestamps

(defun org-jekyll-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information.

Return empty string."
  ;; no timestamps
  "")

;;; Filename and Date Helper

(defun org-jekyll-date-from-filename (&optional filename)
  "Get the date string from filename FILENAME."
  (let ((fn (file-name-nondirectory (or filename (buffer-file-name)))))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      nil)))

(defun org-jekyll-property-list (&optional filename)
  "Get property list of the current buffer or in the file named FILENAME."
  (let ((backend 'jekyll) plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename)))
      (setq plist (org-export-get-environment backend))
      plist)))

(defun org-jekyll-property (keys &optional filename)
  "Get the property by the KEYS in the file named FILENAME.

Example: (org-jekyll-property '(:jekyll-layout) \"index.org\")"
  (let ((plist (org-jekyll-property-list filename)))
    (mapcar (lambda (key) (org-export-data-with-backend (plist-get plist key) 'jekyll plist))
            keys)))

(defun org-jekyll-date-from-property (&optional filename)
  (let ((plist (org-jekyll-property-list filename)))
    (org-read-date
     nil nil
     (org-export-data-with-backend (plist-get plist :date) 'jekyll plist))))

(defun org-jekyll-create-filename ()
  "Return the string of current buffer file name by adding or
replacing the heading date."
  (let ((date (org-jekyll-date-from-property))
        (file (file-name-nondirectory (buffer-file-name)))
        (dir (file-name-directory (buffer-file-name))))
    (expand-file-name
     (replace-regexp-in-string "^\\([0-9]+-[0-9]+-[0-9]+\\)?" date file)
     dir)))



;;; End-User functions

;;;###autoload
(defun org-jekyll-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML buffer adding some YAML front matter."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Jekyll HTML Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (funcall org-html-display-buffer-mode)
              (org-export-add-to-stack (current-buffer) 'jekyll)))
        `(org-export-as 'jekyll ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                   'jekyll "*Org Jekyll HTML Export*"
                   subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-jekyll-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file adding some YAML front matter."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    ;; add the time stamp prefix required by jekyll to the filename
    (setq file (concat
		(file-name-directory file)
		(format-time-string "%Y-%m-%d-" (current-time))
		(file-name-nondirectory file)))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'jekyll))
          (let ((org-export-coding-system org-html-coding-system))
            `(expand-file-name
              (org-export-to-file
               'jekyll ,file ,subtreep ,visible-only ,body-only ',ext-plist))))
      (let ((org-export-coding-system org-html-coding-system))
        (org-export-to-file
         'jekyll file subtreep visible-only body-only ext-plist)))))



;;;###autoload
(defun org-jekyll-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML with YAML front matter.

FILENAME is the filename of the Org file to be published. PLIST
is the property list for the given project. PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'jekyll filename ".html" plist pub-dir))

;;;###autoload
(defun org-jekyll-insert-export-options-template
  (&optional title date setupfile categories published layout)
  "Insert a settings template for Jekyll exporter."
  (interactive)
  (let ((layout (or layout org-jekyll-layout))
        (published (or published org-jekyll-published))
        (categories (or categories org-jekyll-categories)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: " title
                       "\n#+DATE: " date
                       "\n#+SETUPFILE: " setupfile
                       "\n#+JEKYLL_LAYOUT: " layout
                       "\n#+JEKYLL_CATEGORIES: " categories
                       "\n#+JEKYLL_PUBLISHED: " published
                       "\n\n* \n\n{{{more}}}"))))))



(defun org-jekyll-export-headline (&optional properties)
  "Export current headline"
  (interactive "p")
  (let* ((props (org-entry-properties))
	 (time (cdr (assoc "TIMESTAMP_IA" props)))
	 (private (cdr (assoc "private" props)))
	 (tags (org-get-tags-at)))
    (if private
	(message "This is a private entry - stopped exporting.")
      (when time
	;; each headline with timestamp can be exported as a
	;; jekyll blog post
	(or properties
	    (setq properties (org-jekyll-property-list)))

	;; (message "time=%s" time)
	(let* ((heading (org-no-properties (org-get-heading t t)))
	       (title (replace-regexp-in-string
		       "[:=\(\)\? \t]" "_"
		       ;; remove the heading timestamp
		       (substring heading 17)))
	       (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
			      (match-string 1 time)))
	       (to-file (expand-file-name (format "%s-%s.html" str-time title)
					  (or (plist-get properties :jekyll-path)
					      org-jekyll-path)))
	       (org-buffer (current-buffer))
	       ;; (org-time-stamp-custom-formats
	       ;;  '("<%m/%d/%y %a>" . ""))
	       ;; (org-display-custom-times t)
	       html)
	  (org-narrow-to-subtree)
	  (setq html (org-export-as 'jekyll nil nil t nil))
	  (set-buffer org-buffer)
	  (widen)
	  (with-temp-file to-file
	    (insert (format
		     "---\ntitle: \"%s\"
date: %s
layout: %s
categories: %s
published: %s
comments: %s
---\n" 
		     title
		     str-time
		     (or (plist-get properties :jekyll-layout)
			 org-jekyll-layout)
		     (or (plist-get properties :jekyll-categories)
			 org-jekyll-categories)
		     (or (plist-get properties :jekyll-published)
			 org-jekyll-published)
		     (or (plist-get properties :jekyll-comments)
			 org-jekyll-comments)))

	    (insert html))
	  (get-buffer org-buffer)
	  (message "Exported entry: %s." title))))))


;;;###autoload
(defun org-jekyll-export-to-blog ()
  (interactive)
  (let ((properties (org-jekyll-property-list))
	position)
    (save-excursion
      (goto-char (point-min))
      (while (not (eq position (point)))
	(setq position (point))
	(org-forward-heading-same-level 1)
	;; (message "%d" (point))
	(org-jekyll-export-headline properties)))))


(provide 'ox-jekyll)

;;; ox-jekyll end here
