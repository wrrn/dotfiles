;;; straight-wrrn.el --- Initialize and setup straight
(defvar bootstrap-version)
(defvar straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(add-to-list 'straight-hosts
             '(sr.ht
               :type git
               :protocol ssh
               :repo-url "git@git.sr.ht:%s"))

(provide 'straight-wrrn)
;; ;;; straight-wrrn.el ends here
