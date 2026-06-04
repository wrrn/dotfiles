;; term-config.el --- Configure Terminal specific packages

(use-package with-editor
  :ensure t
  :hook ((server-visit . with-editor-mode)
         (shell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor))
  :config
  ;; Ensure with-editor-mode is enabled when editing git commit messages
  ;; and other editor-invoked files. C-c C-c will then save and exit.
  (add-hook 'with-editor-mode-hook
            (lambda ()
              (when (derived-mode-p 'text-mode)
                (message "Git/editor buffer opened. Use C-c C-c to finish, C-c C-k to cancel."))
              ;; Override markdown-mode's C-c C-c binding to work with with-editor
              (when (derived-mode-p 'markdown-mode)
                (local-set-key (kbd "C-c C-c") #'with-editor-finish)))))

;; Helper function for ghostel-pre-spawn-hook
;; Uses with-editor's internal setup to configure EDITOR in spawned shells.
;;
;; We call the internal `with-editor--setup' directly (rather than the public
;; `with-editor-export-editor') because ghostel has a unique advantage: its
;; `ghostel-pre-spawn' hook fires *before* the shell process starts.  That lets
;; us mutate `process-environment' up front and have the new EDITOR inherited
;; cleanly by the child shell at spawn time -- no need to inject the variable
;; into an already-running process the way `with-editor-export-editor' does.
(defun ghostel-with-editor-setup ()
  "Set up EDITOR environment variable for ghostel terminal.
  Calls the internal with-editor setup function to configure
  emacsclient as the EDITOR for git commits and other editor-opening commands.

  `with-editor--setup' relies on the dynamic variable `with-editor--envvar'
  being bound to the name of the variable to export (normally done by the
  public `with-editor-export-editor').  Called directly it is nil, which makes
  with-editor export a garbage variable named "" instead of EDITOR.  Bind it
  explicitly here so the spawned shell gets a real EDITOR=emacsclient ..."
  (require 'with-editor)
  (let ((with-editor--envvar "EDITOR"))
    (with-editor--setup)))

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
