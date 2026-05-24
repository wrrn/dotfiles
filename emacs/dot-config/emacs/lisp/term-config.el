;; term-config.el --- Configure Terminal specific packages

(use-package ghostel
  :ensure t
  :custom
  (ghostel-kill-buffer-on-exit t)
  :bind (("C-x p t" . ghostel-project)
         :map ghostel-semi-char-mode-map
         ("<escape>" . meow--ghostel-emacs-mode)
         ("C-c r" . rename-buffer)
         :map wh-keymap
         ("t c" . ghostel)          ; create new terminal
         ("o t" . ghostel-other)      ; switch to or create terminal
         ("p t" . ghostel-project)))

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
