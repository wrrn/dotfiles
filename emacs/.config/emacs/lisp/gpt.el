;;; gpt.el  --- configure gptel
(use-package gptel
  :ensure t
  :custom
  (gptel-model 'claude-opus-4-20250514)
  :bind (
         ("C-x C-M-a" . gptel-add)
         ("C-x C-M-m" . gptel-menu)
         ("C-x C-M-c" . gptel)
         )
  :config
  (gptel-make-gemini "Gemini" :stream t :key #'gptel-api-key)
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key)))




(provide 'gpt)
