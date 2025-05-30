;;; gpt.el  --- configure gptel
(use-package gptel
  :ensure t
  :custom
  (gptel-model 'gemini-2.5-pro-preview-03-25)
  :bind (
         ("C-x C-M-a" . gptel-add)
         ("C-x C-M-m" . gptel-menu)
         ("C-x C-M-c" . gptel)
         )
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key)
  (setq gptel-backend (gptel-make-gemini "Gemini" :stream t :key #'gptel-api-key)))



(provide 'gpt)
