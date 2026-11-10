;;; straight-wrrn.el --- Initialize and setup straight  -*- lexical-binding: t; -*-
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

;; Mark built-in packages so straight doesn't try to install them.
;; These conflict with tree-sitter modes (tsx-ts-mode, etc.) which
;; expect the built-in versions.
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))
;; tramp-rpc's Package-Requires names tramp; keep Emacs's own TRAMP
;; instead of letting straight shadow it with GNU ELPA's.
(straight-use-package '(tramp :type built-in))
;; dape's Package-Requires names jsonrpc; keep the built-in one that
;; Eglot is written against.
(straight-use-package '(eglot :type built-in))
(straight-use-package '(jsonrpc :type built-in))


(provide 'straight-wrrn)
;; ;;; straight-wrrn.el ends here
