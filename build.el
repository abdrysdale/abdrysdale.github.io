(defun get-titles-from-org-files (directory)
  "Extract titles from org files in DIRECTORY."
  (let ((files (directory-files directory t "\\.org$")))
    (mapcar (lambda (file)
              (let ((rel-file (file-relative-name
                               file (expand-file-name directory))))
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+title: \\(.*\\)$" nil t)
                      (cons rel-file (match-string 1))
                    (cons rel-file nil)))))
              files)))

(defun build-index (author)
  "Build the index in DIR."
  ;; Copies the README.org to the index.
  (let ((dir "src")
        (index "src/index.org")
        (ignore-files '("index.org" "aboutme.org")))
    (with-current-buffer (find-file-noselect index)
      (erase-buffer)
      (insert (format "#+title: %s's Personal Website\n" author))
      (insert "\nThank you for visiting my little patch of the internet.")
      (insert "  All views are my own and not that of my employer.\n")
      (insert "  You can expect content on programming, Emacs, philosophy,")
      (insert " ethics, magnets and bread.")
      (insert "\n\n/This site is intentionally minimal")
      (insert " and is left as an exercise to the reader./\n")
      (insert "\n* Copying\n")
      (insert "All material is licensed under the GNU/GPLv3 license")
      (insert " - which can be found")
      (insert " [[https://github.com/abdrysdale/abdrysdale.github.io/blob/main/LICENSE][here]]")
      (insert "\n* Articles\n")
      (save-buffer))
    (dolist (result (get-titles-from-org-files dir))
      (let ((path (car result))
            (title (cdr result)))
        (with-current-buffer (find-file-noselect index)
          (goto-char (point-max))
          (unless (member path ignore-files)
            (insert (format "- [[file:%s][%s]]\n" path title))
            (save-buffer)))))))

(require 'ox-publish)
(let ((org-html-validation-link nil)
      (org-html-head-include-scripts nil)
      (org-html-head-include-default-style nil)
      (org-html-head (concat
                      "<link rel=\"stylesheet\""
                      "href=\"resources/org.css\""
                      "type=\"text/css\" />"
                      "<header>"
                      "<a href=\"index.html\">Home</a>&emsp;"
                      "<a href=\"aboutme.html\">About Me</a>"
                      "</header>\n"))
      (org-src-fontify-natively t)
      (org-publish-project-alist
       '(("blog"
          :base-directory "src"
          :publishing-directory "docs"
          :auto-sitemap nil
          :recursive t
          :with-author nil
          :with-creator t
          :with-toc t
          :section-numbers nil
          :time-stamp-file nil
          :publishing-function org-html-publish-to-html))))
  (copy-file "README.org" "src/build-process.org" t)
  (build-index "Alex Drysdale")
  (org-publish-all t)
  (message "Site built at %s"
           (format-time-string "%Y-%m-%d %H:%M:%S")))
