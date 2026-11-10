;;; verb-config.el  --- configure verb.el -*- lexical-binding: t; -*-
;; Verb configuration

(use-package verb
  :ensure t
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))



(provide 'verb-config)
