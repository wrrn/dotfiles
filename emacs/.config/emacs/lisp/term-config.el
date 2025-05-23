;; term-config.el --- Configure Terminal specific packages

(use-package vterm
  :custom
  (vterm-min-window-width 20)
  ;; Remove C-j from the vterm-keymap so that it doesn't conflict with
  ;; wh-keymap.
  :bind (:map vterm-mode-map
              ("C-j" . nil))
  :ensure t)


(use-package multi-vterm
  :ensure t
  :requires vterm
  :bind (
         ("C-x p t"   . multi-vterm-project)
         :map vterm-mode-map
         ("C-c r" . multi-vterm-rename-buffer)
         :map wh-keymap
         ("t c" . multi-vterm-dedicated-toggle)
         ("t c" . multi-vterm)
         ("p t" . multi-vterm-project))
  )

(defun eat--meow-emacs-mode ()
  (interactive)
  (progn
    (eat-emacs-mode)
    (meow-insert-exit)))



(use-package eat
  :straight  (eat :type git
                  :host codeberg
                  :repo "akib/emacs-eat"
                  :files ("*.el" ("term" "term/*.el") "*.texi"
                          "*.ti" ("terminfo/e" "terminfo/e/*")
                          ("terminfo/65" "terminfo/65/*")
                          ("integration" "integration/*")
                          (:exclude ".dir-locals.el" "*-tests.el")))
  :ensure t
  :custom
  (eat-kill-buffer-on-exit t)
  :bind (
         :map eat-semi-char-mode-map
         ("i" . self-insert-command)
         ("C-j" . nil)
         ("<escape>" . eat--meow-emacs-mode)
         :map eat-mode-map
         ("C-c C-t" . eat-semi-char-mode)
         )
  )



(provide 'term-config)
