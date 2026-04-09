;;; git-config.el --- configure version control packages
(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (setq magit-diff-refine-hunk t))
  :bind (:map wh-keymap
              ("g s" . magit)
              ("g d" . magit-diff-range)))

(use-package git-link
  :ensure t
  :config
  (defun wh/jj-run (&rest args)
    "Run jj with ARGS, returning non-empty output lines or nil on failure."
    (ignore-errors
      (with-temp-buffer
        (when (zerop (apply #'process-file "jj" nil (current-buffer) nil args))
          (goto-char (point-min))
          (cl-loop until (eobp)
                   for line = (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                   do (forward-line 1)
                   unless (string-empty-p line)
                   collect line)))))

  (defun wh/in-jj-workspace-p ()
    (and (fboundp 'vc-jj-root) (vc-jj-root default-directory)))

  (defun wh/git-link--repo-root-jj (orig-fn)
    "Fall back to jj workspace root when not in a plain git repo."
    (or (funcall orig-fn)
        (when-let ((root (and (fboundp 'vc-jj-root) (vc-jj-root default-directory))))
          (directory-file-name (expand-file-name root)))))

  (defun wh/git-link--current-branch-jj (orig-fn)
    "Fall back to jj local bookmark name when HEAD is detached."
    (or (funcall orig-fn)
        (when (wh/in-jj-workspace-p)
          (let* ((output (car (wh/jj-run "log" "-r" "@" "--no-graph"
                                         "-T" "separate(\" \", self.local_bookmarks().map(|b| b.name()))")))
                 (bookmark (when output (car (split-string output " " t)))))
            (when (and bookmark (not (string-empty-p bookmark)))
              bookmark)))))

  (defun wh/git-link--remotes-jj (orig-fn)
    "Fall back to jj git remotes when git remote list fails."
    (or (funcall orig-fn)
        (when (wh/in-jj-workspace-p)
          (mapcar (lambda (line) (car (split-string line)))
                  (wh/jj-run "git" "remote" "list")))))

  (defun wh/git-link--remote-url-jj (orig-fn name)
    "Fall back to jj git remote URL when git remote get-url fails."
    (or (funcall orig-fn name)
        (when (wh/in-jj-workspace-p)
          (cadr (split-string
                 (car (seq-filter
                       (lambda (line) (string-prefix-p (concat name " ") line))
                       (wh/jj-run "git" "remote" "list"))))))))

  (advice-add 'git-link--repo-root :around #'wh/git-link--repo-root-jj)
  (advice-add 'git-link--current-branch :around #'wh/git-link--current-branch-jj)
  (advice-add 'git-link--remotes :around #'wh/git-link--remotes-jj)
  (advice-add 'git-link--remote-url :around #'wh/git-link--remote-url-jj)
  :bind ("C-c g l" . git-link))


(use-package majutsu
  :ensure t
  :straight (:host github :repo "0WD0/majutsu"))

(use-package vc-jj
  :ensure t)

(use-package jj-link
  :straight (:host sr.ht :repo "~warren/jj-link"
             :files ("*.el"))
  :bind (:map wh-keymap
              ("j l" . jj-link)
              ("j c" . jj-link-commit)
              ("j h" . jj-link-homepage)))

(provide 'git-config)
