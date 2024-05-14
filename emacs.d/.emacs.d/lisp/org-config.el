;; org-config.el --- Org-mode configurations
;; Code:

(defun wrrn-org-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (or
   (org-entry-get 1 name t)
   (org-with-point-at 1
     (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                              (point-max) t)
       (buffer-substring-no-properties
        (match-beginning 1)
        (match-end 1))))))

(defun org-roam-agenda-category ()
    "Get category of item at point for agenda.

Category is defined by one of the following items:
- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:
	(setq org-agenda-prefix-format
		'((agenda . \" %(org-roam-agenda-category) %?-12t %12s\")))
Refer to `org-agenda-prefix-format' for more information

Credit: https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html"
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
         (title (wrrn-org-buffer-prop-get "title"))
         (category (org-get-category)))
    (or title
        category
        file-name)))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  ;; Source: https://github.com/bastibe/org-journal/blob/master/README.org#journal-capture-template
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(use-package org
  :custom
  (org-startup-indented t) ; cleaner looking org-mode
  (org-tags-column 80) ; calling org-align-all-tags puts all the tags on line 80
  (org-startup-with-inline-images t) ; Show images inline any time there is a link to an image
  (org-enforce-todo-dependencies t) ;; Force everything to DONE before marking a parent done.
  (org-hide-emphasis-markers t) ; Hide the emphasis markers for bold, strike-through, italic, underlined, verbatim, and code
  (org-todo-keywords
        '((sequence "TODO(t)" "TO TEST(e)" "TO DEPLOY(i)" "IN PROGRESS(p)" "In Peer Review(r)" "Waiting(w)" "HOLD(h)" "|" "DONE(d)")
          (sequence "QUESTION(q)" "|" "ANSWERED(a)")
          (sequence "|" "NOT DOING(n)")))
  (org-todo-keyword-faces
        '(("Waiting" . org-warning)
          ("HOLD" . org-warning)
          ("In Peer Review" . org-warning)
          ("TO DEPLOY" . org-warning)
          ("IN PROGRESS" . (:foreground "#f1fa8c" :bold t :background "#373844"))))
  (org-log-done 'note) ;; Log when something was marked as done
  (org-fontify-done-headline t) ;; Allow strike throughs for DONE items
  (org-enforce-todo-checkbox-dependencies t) ;; Force checkboxes to be a dependency before moving TODO's to DONE
  (org-hierarchical-todo-statistics nil) ;; Recursive count of todos
  (org-checkbox-hierarchical-statistics nil) ;; Recursive count of todos
  (org-src-fontify-natively t) ;; Syntax highlighting in code blocks
  (appt-display-format 'window)   ;; Opens appointment reminders in current window
  (appt-display-duration 30) ;; Display the appointment reminder for 30 seconds
  (org-export-with-toc nil)   ;; Org to markdown conversion options
  (org-export-headline-levels 5)


  ;; Org agenda configs
  (org-agenda-files (list (concat (getenv "HOME") "/.roam")))
  (org-agenda-prefix-format
   '((agenda . " %i %-50 (org-roam-agenda-category)%?-12t% s")
     (todo   . "%i %-50 (org-roam-agenda-category) ")
     (tags   . "%i %-50 (org-roam-agenda-category) ")
     (search . "%i %-50 (org-roam-agenda-category) ")))

  ;; Org Capture
  (org-capture-templates '(
                           ("j"
                            "Journal entry"
                            plain (function org-journal-find-location)
                            "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?")

                           ("t"
                            "TODO"
                            plain (function org-journal-find-location)
                            "** TODO %^{Title} %^g\n%i%?")
                           ))

  ;; Specify how org-refile suggests files. Source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  ;;;; Tell org to list this files when given the opportunity to refile.
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;;;; Tell org to include the files so that we can add thing to the toplevel
  (org-refile-use-outline-path 'file)
  ;;;; List everything at once
  (org-outline-path-complete-in-steps nil)
  ;;;; Allow the creation of new parent nodes
  (org-refile-allow-creating-parent-nodes 'confirm)





  
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-p" . org-metaup)
         ("M-n" . org-metadown))

  :config (progn

            ;; Strike through DONE
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-headline-done nil
                                :strike-through t)

            ;; Add languages to code blocks
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (js . t)
               (org . t)
               (python . t)
               (shell . t)
               (sql . t)))
            (add-hook 'org-after-todo-statistics-hook (lambda(n-done n-not-done)
                                                        "Switch entry to DONE when all subentries are done, to TODO otherwise"
                                                        (let (org-log-done org-log-status)
                                                          (org-todo (if (= n-not-done 0)
                                                                        "DONE"
                                                                      (org-get-todo-sequence-head (org-get-todo-state)))))))
            ;; Export org files to github markdown
            (use-package ox-gfm
              :ensure t)

            (setq org-export-backends (quote (ascii html icalendar latex md gfm)))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat (getenv "HOME") "/.roam"))
  (org-roam-v2-ack t)
  (org-roam-db-update-on-save t)
  (org-roam-database-connector 'sqlite-builtin)
  
  :bind  (("C-c n f" . org-roam-node-find)
          ("C-c n b" . org-roam-switch-to-buffer)
          ("C-c n g" . org-roam-graph)
          ("C-o" . nil)
          ("C-o r f" . org-roam-node-find)
          ("C-o r g" . org-roam-graph)
  :map org-mode-map
  (("C-c n i" . org-roam-node-insert)
   ("C-o r n i")
   ("C-o r r" . org-roam-refile)
   ;; TODO: Use the variable here
   ("C-j" . nil))))

;; Deft helps me look up org-roam files quickly. Like rg or grep but on the fly.
(use-package deft
  :straight (deft :type git :host github :repo "jrblevin/deft"
                  :fork (:host github
                               :repo "wrrn/deft")
                  :build (:not compile))
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (concat (getenv "HOME") "/.roam")))

(defun org-journal-new-note ()
  "Insert todays date a drawer at point"
  (interactive)
  (org-insert-drawer nil (format-time-string "%Y-%m-%d")))

(use-package org-journal
  :ensure t
  :bind (:map wh-keymap
              ("j o" . org-journal-open-current-journal-file)
              ("j e" . org-journal-new-entry)
              ("j n" . org-journal-new-note)

          :map org-journal-mode-map
              (("C-j" . nil)))
  :custom
  (org-journal-file-header "#+TITLE: %B %Y")
  (org-journal-file-format "%Y-%m.org")
  (org-journal-dir (concat (getenv "HOME") "/.roam"))
  
  (org-journal-date-format "%A, %F")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "/!")
  (org-journal-hide-entries-p t))


;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode))

(use-package org-tree-slide
  :ensure t
  :custom
  (org-image-actual-width nil))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path (concat (getenv "HOME") "/node_modules/.bin/mmdc"))
  :init
  (append org-babel-load-languages '((go . t))))

(use-package org-modern
  :ensure t
  :after (org)
  :config (global-org-modern-mode)
  )

(use-package org-timeblock
  :ensure t)

(provide 'org-config)
;; org-config.el ends here
