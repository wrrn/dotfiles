;;; herdr-config.el  --- configure herdr.el  -*- lexical-binding: t; -*-
;;; Commentary:
;; herdr.el shows the agents running under herdr and opens their panes
;; in ghostel buffers.  It ships no herdr.el; herdr-ui is the entry point.
;; Without a main file straight can't read its dependencies, so
;; magit-section is declared here.  ghostel comes from term-config.
;;; Code:

(use-package magit-section
  :defer t)

(use-package herdr-ui
  :straight (herdr :host github :repo "roman/herdr.el")
  :bind (:map wh-keymap
              ("h h" . herdr-ui)
              ("h s" . herdr-spaces-visit)
              ("h a" . herdr-agents-visit)))

(provide 'herdr-config)
;;; herdr-config.el ends here
