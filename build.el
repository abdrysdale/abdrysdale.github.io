;; Delete the docs directory
(when (file-exists-p "docs")
  (delete-directory "docs" t))

;; Delete tag files
(dolist (file (directory-files "src" nil "^tags-.*\\.org$"))
  (when (file-regular-p (expand-file-name file "src"))
    (delete-file (expand-file-name file "src"))))

;; Copy resources to the docs directory
(make-directory "docs" t)
(copy-directory "src/resources" "docs/resources" t t)

;; Tell github this isn't a Jekyll cite and insert the CNAME
(with-temp-file (expand-file-name "docs/.nojekyll"))
(with-temp-file (expand-file-name "docs/CNAME")
  (insert "abdrysdale.phd"))

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package--initialized (package-initialize))

(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))

(defun get-org-property (prop file)
  "Extract PROP from the org FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      ;; No need for error handling here as (cdr nil) and (car nil) return nil
      (car (cdr (car (org-collect-keywords `(,prop))))))))

(defun get-titles-from-org-files (directory)
  "Extract titles and dates from org files in DIRECTORY."
  (let ((files (directory-files directory t "^[[:alnum:]-_]+.org$")))
    (mapcar (lambda (file)
              (let ((rel-file (file-relative-name
                               file (expand-file-name directory))))
                `(,rel-file . ,(list
                                (get-org-property "TITLE" file)
                                (get-org-property "DATE" file)
                                (get-org-property "FILETAGS" file)))))
            files)))

(defun build-index (author)
  "Build the index for the AUTHOR."
  ;; Copies the README.org to the index.
  (let ((dir "src")
        (index "src/index.org")
        (ignore-files '("index.org"
                        "about.org"
                        "sitemap.org"
                        "index-template.org"))
        (used-tags nil)
        (articles nil))

    ;; Inserts title and template
    (with-current-buffer (find-file-noselect index)
      (erase-buffer)
      (insert (format "#+title: %s\n\n" author))
      (insert-file-contents "../index-template.org")
      (end-of-buffer)
      (insert "\n\n* Articles\n")
      (save-buffer))

    ;; Gets all of the articles
    (dolist (result (get-titles-from-org-files dir))
      (let ((path (car result))
            (info (cdr result)))
        (unless (member path ignore-files)
          (push `(,path ,(car info) ,(cadr info) ,(caddr info)) articles))))

    ;; Sort articles by date
    (setq articles (sort articles (lambda (a b)
                              (let ((date-a (caddr a))
                                    (date-b (caddr b)))
                                (string> (substring date-a 1 -1)
                                         (substring date-b 1 -1))))))

    ;; Insert links to articles in the index and tag files
    (dolist (article articles)
      (let ((path (car article))
            (title (cadr article))
            (date (caddr article))
            (tags (cadddr article)))
        (with-current-buffer (find-file-noselect index)
          (goto-char (point-max))
          (let ((link (format
                       "- [[file:%s][%s]] %s\t%s\n"
                       path
                       title
                       date
                       (replace-regexp-in-string ":" "/" tags))))
            (insert link)
            (save-buffer)

            ;; Insert a link to article in each of the tags file.
            (dolist (tag (split-string tags ":"))
              (unless (string-empty-p tag)
                (let* ((tag-file (concat "tags-" tag ".org"))
                       (tag-entry `(,tag . ,tag-file)))
                  (unless (member tag-entry used-tags)
                    (push tag-entry used-tags))
                  (with-current-buffer (find-file-noselect tag-file)
                    (unless (file-exists-p tag-file)
                      (erase-buffer)
                      (insert (format "#+title:%s\n\n" tag)))
                    (goto-char (point-max))
                    (insert link)
                    (save-buffer)))))))))

          ;; Insert a link to the tag files in the index
          (with-current-buffer (find-file-noselect index)
            (insert "\n* Tags\n\n")
            (dolist (tag-info used-tags)
              (let ((tag (car tag-info))
                    (file (cdr tag-info)))
                (insert (format "- [[file:%s][%s]]\n" file tag))))
            (save-buffer))))

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; 2010-09-02 thanks to Pascal J Bourguignon and TheFlyingDutchman

(defun get-rss-feed-item (title link date &optional desc)
  "Return an rss feed item with TITLE, LINK, DATE and DESCRIPTION."
  (concat
   "<item>\n"
   "<title>" title "</title>\n"
   "<link>" link "</link>\n"
   "<pubDate>" date "</pubDate>\n"
   "<description><![CDATA[" (if desc desc "") "]]></description>\n"
   "</item>\n"))

(defun build-rss-feed (title link desc src out)
  "Build a rss feed for TITLE (DESC) at LINK using the posts in SRC to OUT."
  (with-current-buffer (find-file-noselect (concat out "feed.xml"))
    (erase-buffer)
    (insert (concat
             "<rss version=\"2.0\">\n"
             "<channel>\n"
             "<title>" title "</title>\n"
             "<description>" desc "</description>\n"
             "<link>" link "</link>\n"))
    (dolist (file (directory-files src nil "^[[:alnum:]-_]+.org$"))
      (let ((date (get-org-property "EXPORT_DATE" (concat src "/" file))))
        (insert (get-rss-feed-item (get-org-property "TITLE"
                                                     (concat src "/" file))
                                   (concat link "/"
                                           (car (split-string file ".org"))
                                           ".html")
                                   date
                                   (get-string-from-file
                                    (concat src "/" file))))))
    (insert "</channel>\n</rss>")
    (save-buffer)))

(require 'ox-publish)
(require 'whitespace)
(require 'htmlize)
(let ((current-theme (if custom-enabled-themes
                         (car custom-enabled-themes)
                       'modus-operandi))
      (publish-theme 'modus-operandi)
      (whitespace-style nil)
      (whitespace-mode 0)
      (org-html-validation-link nil)
      (org-html-head-include-scripts nil)
      (org-html-head-include-default-style nil)
      (org-html-head (concat
                      "<link rel=\"stylesheet\""
                      "href=\"resources/org.css\""
                      "type=\"text/css\" />"
                      "<header>"
                      "<a href=\"index.html\">Home</a>"
                      "&emsp;<a href=\"about.html\">About Me</a>"
                      "&emsp;<a href=\"https://github.com/abdrysdale/abdrysdale.github.io\">Source</a>"
                      "&emsp;<a href=\"sitemap.html\">Sitemap</a>"
                      "&emsp;<a href=\"feed.xml\">RSS</a>"
                      "</header>\n"))
      (org-src-fontify-natively t)
      (org-publish-project-alist
       '(("blog"
          :base-directory "src"
          :recursive nil
          :publishing-directory "docs"
          :auto-sitemap t
          :with-author nil
          :with-creator t
          :with-toc t
          :headline-levels 1
          :section-numbers nil
          :time-stamp-file nil
          :publishing-function org-html-publish-to-html))))
  (copy-file "README.org" "src/colophon.org" t)
  (build-index "Alex Drysdale")
  (build-rss-feed
   "Alex Drysdale"
   "https://abdrysdale.phd"
   "Blog posts by Alex Drysdale"
   "../src/" "docs/")
  (load-theme publish-theme)
  (org-publish-all t)
  (load-theme current-theme)
  (message "Site built at %s"
           (format-time-string "%Y-%m-%d %H:%M:%S")))
