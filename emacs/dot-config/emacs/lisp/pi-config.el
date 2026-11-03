;;; pi-config.el  --- configure pi-coding-agent  -*- lexical-binding: t; -*-
;; Configuration

(use-package pilish
  :ensure t
  :bind ("C-c p" . pilish)
  )

;; In a TRAMP project, pilish runs pi on the remote host, and the agent's
;; commands inherit TRAMP's PATH.  Take that PATH from the remote login
;; shell: on NixOS hosts pi and the user's tools live in profile
;; directories TRAMP doesn't search by default.
(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'pi-config)
