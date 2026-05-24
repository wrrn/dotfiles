;;; code-nav.el --- Install code navigation primitives
;;; Commentary:
;;; Code:
(use-package combobulate
  :ensure t
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(provide 'code-nav)
;;; gcloud.el ends here
