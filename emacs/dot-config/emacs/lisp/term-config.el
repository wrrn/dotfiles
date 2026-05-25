;; term-config.el --- Configure Terminal specific packages

(use-package with-editor
  :ensure t)

;; Helper function for ghostel-pre-spawn-hook
;; Uses with-editor's internal setup to configure EDITOR in spawned shells
(defun ghostel-with-editor-setup ()
  "Set up EDITOR environment variable for ghostel terminal.
  Calls the internal with-editor setup function to configure
  emacsclient as the EDITOR for git commits and other editor-opening commands."
  (with-editor--setup))

(use-package ghostel
  :ensure t
  :custom
  (ghostel-kill-buffer-on-exit t)
  ;; Without this, self-insert keys in Emacs/copy mode exit read-only and
  ;; forward the key to the shell — so meow's j/k/h/l/n would drop us out
  ;; of Emacs mode and cycle shell history instead of navigating the buffer.
  (ghostel-readonly-fast-exit nil)
  :hook (ghostel-pre-spawn . ghostel-with-editor-setup)
  :bind (("C-x p t" . ghostel-project)
         :map ghostel-semi-char-mode-map
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
