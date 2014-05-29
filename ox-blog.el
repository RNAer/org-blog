;;; ox-blog -- is a minor mode, which help to export org-mode file
;;; as an Jekyll- or Hexo-formatted html.

;; Author: Zech XU
;; Created: 2014-05-27
;; Version: 1.0
;; Package-Requires: ((org "8.0"))
;; Keywords: org, blog, Jekyll, Hexo

;; This file is not part of GNU Emacs.

(require 'ox-html)

(defgroup org-export-blog nil
  "Options for exporting Org mode files to blog HTML using Jekyll or Hexo."
  :tag "Org Blog"
  :group 'org-export
  :version "24.3")


(defcustom org-blog-include-yaml-front-matter t
  "If true, then include yaml-front-matter when exporting to html.

If false, then you should include the yaml front matter like this at the top of the file:

#+BEGIN_HTML
---
layout: post
title: Org-mode to GitHub pages with Jekyll/Hexo
excerpt: Introduce how to use Emacs's Org-mode to generate GitHub Pages
categories: [lessons, beginner]
tags: [Emacs, org-mode, GitHub, Jekyll]
tagline: \"Supporting tagline\"
date: 2013-09-15 22:08
comments: true
keywords: blog
description: Instructions on export org file with Jekyll or Hexo
---
#+END_HTML"
  :group 'org-export-blog
  :type 'boolean)

(defcustom org-blog-engine "hexo"
  "Default static webpage generator to use.

This variable will be used to guide the accommodation
for different generators."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-yaml-layout "post"
  "Default layout used in Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-yaml-categories "other"
  "Default space-separated categories in Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-yaml-tags nil
  "Default space-separated categories in Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-yaml-published "true"
  "Default publish status in Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-yaml-comments "true"
  "Default comments (disqus) flag in Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-path
  (cond ((string= org-blog-engine "jekyll")
	 "~/Desktop/blog/rnaer.github.io/_posts")
	(t
	 "~/Desktop/blog/hexo-blog/source/_posts"))
  "Default publish dir for Blog article."
  :group 'org-export-blog
  :type 'string)

(defcustom org-blog-use-src-plugin nil
   "If t, org-blog exporter eagerly uses plugins instead of
org-mode's original HTML stuff. For example:

#+BEGIN_SRC ruby
puts \"Hello world\"
#+END_SRC

makes:

{% codeblock ruby %}
puts \"Hello world\"
{% endcodeblock %}"
  :group 'org-export-blog-use-src-plugin
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'blog 'html
  :export-block '("HTML" "BLOG")
  :menu-entry
  '(?b "Blog: export to HTML with YAML front matter."
       ((?H "As HTML buffer" org-blog-export-as-html)
        (?h "As HTML file" org-blog-export-to-html)))
  :translate-alist
  '((template . org-blog-template) ;; add YAML front matter.
    (src-block . org-blog-src-block)
    (inner-template . org-blog-inner-template)) ;; force body-only
  :options-alist
  '((:blog-engine "BLOG_ENGINE" nil org-blog-engine)
    (:blog-path "BLOG_PATH" nil org-blog-path)
    (:blog-layout "BLOG_LAYOUT" nil org-blog-yaml-layout)
    (:blog-categories "BLOG_CATEGORIES" nil org-blog-yaml-categories)
    (:blog-tags "BLOG_TAGS" nil org-blog-yaml-tags)
    (:blog-published "BLOG_PUBLISHED" nil org-blog-yaml-published)
    (:blog-comments "BLOG_COMMENTS" nil org-blog-yaml-comments)))


;;; Internal Filters

(defun org-blog-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into blog code template format
if `org-blog-use-src-plugin` is t. Otherwise, perform as
`org-html-src-block`. CONTENTS holds the contents of the item.
INFO is a plist used as a communication channel."
  (if org-blog-use-src-plugin
      (let ((language (org-element-property :language src-block))
            (value (org-remove-indentation
                    (org-element-property :value src-block))))
        (format "{%% codeblock lang:%s %%}\n%s{%% endcodeblock %%}"
                language value))
    (org-export-with-backend 'html src-block contents info)))



;;; Template

(defun org-blog-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (if org-blog-include-yaml-front-matter
      (concat
       (org-blog--yaml-front-matter info)
       contents)
    contents))

(defun org-blog-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; PREVIEW mark on the top of article.
   (unless (equal "true" (plist-get info :blog-published))
     "<span style=\"background: red;\">PREVIEW</span>")
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))



;;; YAML Front Matter

(defun org-blog--yaml-front-matter (info)
  (let ((title (plist-get info :title))
        (date  (or (plist-get info :date)
		   (format-time-string "%Y-%m-%d %a %H:%M:%S"
				       (current-time))))
        (layout (plist-get info :blog-layout))
        (categories (plist-get info :blog-categories))
        (published  (plist-get info :blog-published))
	(tags (cons (plist-get info :blog-tags)
		    (plist-get info :tag)))
        (comments (plist-get info :blog-comments)))
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

(defun org-blog-date-from-filename (&optional filename)
  "Get the date string from filename FILENAME."
  (let ((fn (file-name-nondirectory (or filename (buffer-file-name)))))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      nil)))

(defun org-blog-property-list (&optional filename)
  "Get property list of the current buffer or in the file named FILENAME."
  (let ((backend 'blog) plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename)))
      (setq plist (org-export-get-environment backend))
      plist)))

(defun org-blog-property (keys &optional filename)
  "Get the property by the KEYS in the file named FILENAME.

Example: (org-blog-property '(:blog-layout) \"index.org\")"
  (let ((plist (org-blog-property-list filename)))
    (mapcar (lambda (key) (org-export-data-with-backend (plist-get plist key) 'blog plist))
            keys)))

(defun org-blog-date-from-property (&optional filename)
  (let ((plist (org-blog-property-list filename)))
    (org-read-date
     nil nil
     (org-export-data-with-backend (plist-get plist :date) 'blog plist))))

(defun org-blog-create-filename ()
  "Return the string of current buffer file name by adding or
replacing the heading date."
  (let ((date (org-blog-date-from-property))
        (file (file-name-nondirectory (buffer-file-name)))
        (dir (file-name-directory (buffer-file-name))))
    (expand-file-name
     (replace-regexp-in-string "^\\([0-9]+-[0-9]+-[0-9]+\\)?" date file)
     dir)))



;;; End-User functions

;;;###autoload
(defun org-blog-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML buffer adding some YAML front matter."
  (interactive)
  (org-export-to-buffer 'blog "*Org Blog HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-blog-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file adding some YAML front matter."
  (interactive)
  (let ((extension (concat "." org-html-extension))
         ;; (file (org-export-output-file-name extension subtreep))
	(org-export-coding-system org-html-coding-system)
	file props heading time)

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
			   org-blog-path))
	       (setq tag (org-get-tags-at))
	       (setq props (org-entry-properties))

	       (if ext-plist
		   (progn (plist-put ext-plist :tag tag)
			  (plist-put ext-plist :date time))
		 (setq ext-plist (plist-put ext-plist :tag tag))))
      (setq file (org-export-output-file-name extension subtreep org-blog-path)))
    ;; add the time stamp prefix required by Jekyll to the filename
    (if (string= org-blog-engine "jekyll")
	(setq file (concat
		    (file-name-directory file)
		    (format-time-string "%Y-%m-%d-" (current-time))
		    (file-name-nondirectory file))))
    (org-export-to-file 'blog file
      async subtreep visible-only body-only ext-plist)))



;;;###autoload
(defun org-blog-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML with YAML front matter.

FILENAME is the filename of the Org file to be published. PLIST
is the property list for the given project. PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blog filename ".html" plist pub-dir))

;;;###autoload
(defun org-blog-insert-export-options-template
  (&optional title date setupfile categories tags published layout)
  "Insert a settings template for Blog exporter."
  (interactive)
  (let ((layout     (or layout org-blog-yaml-layout))
        (published  (or published org-blog-yaml-published))
	(tags       (or tags org-blog-yaml-tags))
        (categories (or categories org-blog-yaml-categories)))
    (save-excursion
      (insert (concat
	       "#+TITLE: " title
	       "\n#+DATE: " date
	       "\n#+SETUPFILE: " setupfile
	       "\n#+BLOG_LAYOUT: " layout
	       "\n#+BLOG_CATEGORIES: " categories
	       "\n#+BLOG_TAGS: " tags
	       "\n#+BLOG_PUBLISHED: " published
	       "\n\n* \n")))))


;;;###autoload
(defun org-blog-export-to-blog ()
  (interactive)
  (let ((properties (org-blog-property-list))
	position)
    (save-excursion
      (goto-char (point-min))
      (while (not (eq position (point)))
	(setq position (point))
	(org-forward-heading-same-level 1)
	;; (message "%d" (point))
	(org-blog-export-headline properties)))))


(provide 'ox-blog)

;;; ox-blog end here
