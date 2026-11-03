;;; herdr-config.el  --- configure herdr.el  -*- lexical-binding: t; -*-
;;; Commentary:
;; herdr.el shows the agents running under herdr and opens their panes
;; in ghostel buffers.  It ships no herdr.el; herdr-ui is the entry point.
;; Without a main file straight can't read its dependencies, so
;; magit-section is declared here.  ghostel comes from term-config.
;;; Code:

(use-package magit-section
  :defer t)

(defun wrrn-herdr--remote-command-any-shell (command)
  "Run the POSIX COMMAND under sh, whatever the remote login shell is.
sshd hands a remote command to the login shell, and fish can't parse
herdr.el's machine probe.  Base64 carries it past that shell untouched."
  (format "echo %s | base64 -d | sh"
          (base64-encode-string (encode-coding-string command 'utf-8) t)))

(use-package herdr-ui
  :straight (herdr :host github :repo "roman/herdr.el")
  :config
  ;; Without this, machines whose login shell is fish never come online.
  (advice-add 'herdr-machines--remote-command :filter-return
              #'wrrn-herdr--remote-command-any-shell)
  :bind (:map wh-keymap
              ("h h" . herdr-ui)
              ("h s" . herdr-spaces-visit)
              ("h a" . herdr-agents-visit)))

(provide 'herdr-config)
;;; herdr-config.el ends here
