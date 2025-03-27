;;; gpt.el  --- configure gptel
(use-package gptel
  :ensure t
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key))



(provide 'gpt)
