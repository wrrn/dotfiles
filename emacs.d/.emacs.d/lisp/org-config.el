;; org-config.el --- Org-mode configurations
;; Code:
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
  (org-agenda-files (list (concat (getenv "HOME") "/.roam")))
  :bind (("C-c l" . org-store-link)
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
  (org-insert-drawer nil (format-time-string "%Y-%m-%d"))
  )

(use-package org-journal
  :ensure t
  :bind (:map wh-keymap
              ("j o" . org-journal-open-current-journal-file)
              ("j e" . org-journal-new-entry)
              ("j n" . org-journal-new-note))
  :custom
  (org-journal-file-header "#+TITLE: %B %Y")
  (org-journal-file-format "%Y-%m.org")
  (org-journal-dir (concat (getenv "HOME") "/.roam"))
  
  (org-journal-date-format "%A, %F")
  (org-journal-time-format "")
  (org-journal-file-type 'monthly)
  (org-journal-carryover-items "/!")
  (org-journal-hide-entries-p nil))


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

(provide 'org-config)
;; org-config.el ends here
