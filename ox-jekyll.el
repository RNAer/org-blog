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
categories: [lessons, beginner]
tags: [Emacs, org-mode, GitHub, Jekyll]
tagline: \"Supporting tagline\"
date: 2013-09-15 22:08
comments: true
keywords: blog
description: Instructions on export org file with Jekyll
---
#+END_HTML"
  :group 'org-export-jekyll
  :type 'boolean)


(defcustom org-jekyll-yaml-layout "post"
  "Default layout used in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-yaml-categories "other"
  "Default space-separated categories in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-yaml-tags nil
  "Default space-separated categories in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-yaml-published "true"
  "Default publish status in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-yaml-comments "true"
  "Default comments (disqus) flag in Jekyll article."
  :group 'org-export-jekyll
  :type 'string)

(defcustom org-jekyll-path "~/Desktop/blog/rnaer.github.io/_posts"
  "Default publish dir for Jekyll article."
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
    (inner-template . org-jekyll-inner-template)) ;; force body-only
  :options-alist
  '((:jekyll-path "JEKYLL_PATH" nil org-jekyll-path)
    (:jekyll-layout "JEKYLL_LAYOUT" nil org-jekyll-yaml-layout)
    (:jekyll-categories "JEKYLL_CATEGORIES" nil org-jekyll-yaml-categories)
    (:jekyll-tags "JEKYLL_TAGS" nil org-jekyll-yaml-tags)
    (:jekyll-published "JEKYLL_PUBLISHED" nil org-jekyll-yaml-published)
    (:jekyll-comments "JEKYLL_COMMENTS" nil org-jekyll-yaml-comments)))


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

(defun org-jekyll--yaml-front-matter (info)
  (let ((title (plist-get info :title))
        (date  (or (plist-get info :date)
		   (format-time-string "%Y-%m-%d %a %H:%M:%S"
				       (current-time))))
        (layout (plist-get info :jekyll-layout))
        (categories (plist-get info :jekyll-categories))
        (published  (plist-get info :jekyll-published))
	(tags (cons (plist-get info :jekyll-tags)
		    (plist-get info :tag)))
        (comments (plist-get info :jekyll-comments)))
    (concat
     "---"
     "\ntitle: " (org-element-interpret-data title)
     "\ndate: " (org-element-interpret-data date)
     "\nlayout: " layout
     "\ncategories: " categories
     "\ntags: " (mapconcat 'identity tags ", ")
     "\npublished: " published
     "\ncomments: " comments
     "\n---\n")))



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
  (org-export-to-buffer 'jekyll "*Org Jekyll HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-jekyll-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file adding some YAML front matter."
  (interactive)
  (let ((extension (concat "." org-html-extension))
         ;; (file (org-export-output-file-name extension subtreep))
	(org-export-coding-system org-html-coding-system)
	file props heading time)
    ;; add the time stamp prefix required by jekyll to the filename
    ;; (setq file (concat
    ;; 		(file-name-directory file)
    ;; 		(format-time-string "%Y-%m-%d-" (current-time))
    ;; 		(file-name-nondirectory file)))
    (if subtreep
	(progn (setq heading (org-get-heading 't 't))
	       (setq time (and (string-match org-ts-regexp-both heading)
			       (match-string 0 heading)))
	       (setq heading (replace-regexp-in-string
			      "[:=\(\)\? \t]" "_"
			      (replace-regexp-in-string
			       org-ts-regexp-both "" heading)))
	       (setq file (expand-file-name
			   (concat heading extension)
			   org-jekyll-path))
	       (setq tag (org-get-tags-at))
	       (setq props (org-entry-properties))

	       (if ext-plist
		   (progn (plist-put ext-plist :tag tag)
			  (plist-put ext-plist :date time))
		 (setq ext-plist (plist-put ext-plist :tag tag))))
      (setq file (org-export-output-file-name extension subtreep org-jekyll-path)))

    (org-export-to-file 'jekyll file
      async subtreep visible-only body-only ext-plist)))



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
  (&optional title date setupfile categories tags published layout)
  "Insert a settings template for Jekyll exporter."
  (interactive)
  (let ((layout     (or layout org-jekyll-yaml-layout))
        (published  (or published org-jekyll-yaml-published))
	(tags       (or tags org-jekyll-yaml-tags))
        (categories (or categories org-jekyll-yaml-categories)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: " title
                       "\n#+DATE: " date
                       "\n#+SETUPFILE: " setupfile
                       "\n#+JEKYLL_LAYOUT: " layout
                       "\n#+JEKYLL_CATEGORIES: " categories
		       "\n#+JEKYLL_TAGS: " tags
                       "\n#+JEKYLL_PUBLISHED: " published
                       "\n\n* \n\n{{{more}}}"))))))


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
