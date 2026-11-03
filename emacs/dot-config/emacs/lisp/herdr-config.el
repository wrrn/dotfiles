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

(defun wrrn-herdr--bind-row-keys (map)
  "Move between rows of the herdr panel MAP with j and k.
The panels open in meow's motion state, whose j and k run `meow-next'
and `meow-prev' ahead of MAP, so remap those as well."
  (keymap-set map "j" #'herdr-panel-next-row)
  (keymap-set map "k" #'herdr-panel-previous-row)
  (keymap-set map "<remap> <meow-next>" #'herdr-panel-next-row)
  (keymap-set map "<remap> <meow-prev>" #'herdr-panel-previous-row))

(use-package herdr-ui
  :straight (herdr :host github :repo "roman/herdr.el")
  :bind (:map wh-keymap
              ("h h" . herdr-ui)
              ("h s" . herdr-spaces-visit)
              ("h a" . herdr-agents-visit)))

;; The visit commands autoload herdr-spaces and herdr-agents without
;; herdr-ui, so hook each library rather than herdr-ui's :config.
(with-eval-after-load 'herdr-machines
  ;; Without this, machines whose login shell is fish never come online.
  (advice-add 'herdr-machines--remote-command :filter-return
              #'wrrn-herdr--remote-command-any-shell))

(with-eval-after-load 'herdr-spaces
  (wrrn-herdr--bind-row-keys herdr-spaces-mode-map))

(with-eval-after-load 'herdr-agents
  (wrrn-herdr--bind-row-keys herdr-agents-mode-map))

(provide 'herdr-config)
;;; herdr-config.el ends here
