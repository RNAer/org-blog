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

(defvar org-jekyll-yaml-attr
  '(layout categories tags published comments date title)
  "The attributes of yaml front matter")

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

(defcustom org-jekyll-path nil
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
    (timestamp . org-jekyll-timestamp)
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
       (org-jekyll--yaml-front-matter-print (org-jekyll--yaml-front-matter info))
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
  ;; set in the org file or default to the file name w/o extension
  (let ((my-hash (make-hash-table :test 'equal)))
    (mapc (lambda (key)
	    (puthash key
		     (plist-get
		      info
		      (intern (format ":jekyll-%S" key)))
		     my-hash))
	  org-jekyll-yaml-attr)
    (puthash 'date (plist-get info :date) my-hash)
    (puthash 'title (plist-get info :title) my-hash)
    my-hash))

(defun org-jekyll--yaml-front-matter-print (my-hash)
  (let (value)
    (concat
     "---\n"
     (mapconcat (lambda (key)
		  (setq value (gethash key my-hash))
		  (if value
		      (format "%S: %s\n" key value)
		    ""))
		org-jekyll-yaml-attr
		"")
     "---\n")))


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
    (org-export-to-file
	'jekyll file async subtreep visible-only body-only ext-plist)))



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



(defun org-jekyll-export-headline (&optional properties)
  "Export current headline of cursor position."
  (interactive "P")
  (let* ((props      (org-entry-properties))
	 (time       (or (cdr (assoc "TIMESTAMP_IA" props)) ; inactive timestamp
		     (cdr (assoc "TIMESTAMP" props))))  ; active timestamp
	 (tags       (org-get-tags-at))
	 ;; if any current tag exists in `org-export-exclude-tags'
	 (private    (catch 'flag
		       (mapc (lambda (x) (if (member x org-export-exclude-tags)
					     (throw 'flag t)))
			     tags)
		       (throw 'flag nil)))
	 my-hash)
    (if private
	(message "This is a private entry - stopped exporting.")
      ;; each headline with timestamp can be exported as a jekyll blog post
      (when time
	(or properties
	    (setq properties (org-jekyll-property-list)))
	(setq my-hash  (org-jekyll--yaml-front-matter properties))

	(mapc (lambda (key)
		(setq attr (cdr (assoc (symbol-name key) props)))
		(if attr
		    (puthash key attr my-hash)))
	      org-jekyll-yaml-attr)
	(puthash 'tags (concat (mapconcat 'identity tags " ") " "
			       (gethash 'tags my-hash))
		 my-hash)

	(let* ((heading (org-no-properties (org-get-heading 't 't)))
	       (title  (replace-regexp-in-string
			;; remove the heading timestamp
			"\\(\\[\\|<\\)[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\).*\\(\\]\\|>\\) +"
			"" heading))
	       (filename (replace-regexp-in-string
			  ;; replace awkward char in filename
			  "[:=\(\)\? \t]" "_" title))
	       (str-time (if (string-match "\\([[:digit:]\-]+\\) " time)
			      (match-string 1 time)
			   (format-time-string "%Y-%m-%d" (current-time))))
	       (to-file (expand-file-name (format "%s-%s.html" str-time filename)
					  (plist-get properties :jekyll-path)))

	       (org-buffer (current-buffer))

	       html)
	  (puthash 'date str-time my-hash)
	  (puthash 'title title my-hash)
	  ;; (org-narrow-to-subtree)
	  ;; (forward-line)
	  (setq html (org-export-as 'jekyll t nil t nil))
	  (set-buffer org-buffer)
	  ;; (widen)
	  (with-temp-file to-file
	    (insert (org-jekyll--yaml-front-matter-print my-hash))
	    (insert html))
	  (get-buffer org-buffer)
	  (message "Wrote %s." to-file))))))


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
