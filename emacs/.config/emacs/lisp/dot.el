;;; dot.el --- load all of my custom packages
;;; Commentary:
;;; Code:
(defvar bootstrap-version)
(defvar straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)



(require 'use-package)

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package exec-path-from-shell
  ;; Used to get environment variables for mac
  :ensure t
  :init (progn
          (setq-default shell-file-name (getenv "SHELL"))
          (setq-default exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "HOME"))
          (setq-default default-directory (getenv "HOME"))
          (exec-path-from-shell-initialize)))

(use-package asdf
  :ensure t
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :custom
  (asdf-binary "/usr/local/opt/asdf/libexec/bin/asdf")
  :config
  (asdf-enable))


(require 'no-littering)
(require 'gcloud)
(require 'org-config)
(require 'lsp-config)
(require 'dap-config)
(require 'comments)
(require 'typescript)
(require 'go-config)
(require 'nix-config)
;; (require 'java-development)
;; (require 'scala-development)
(require 'writeroom-config)
(require 'term-config)
(require 'gpt)
(require 'claude)
(require 'ux-config)
(require 'meow)
(require 'ui-config)


(use-package magit
  :ensure t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (setq magit-diff-refine-hunk t))
  :bind (:map wh-keymap
              ("g s" . magit)
              ("g d" . magit-diff-range)))

(use-package forge
  :after magit
  :config (progn
            (add-to-list 'forge-alist
                         '("ghe.spotify.net"
                           "ghe.spotify.net/api/v3"
                           "ghe.spotify.net"
                           forge-github-repository))
            ))

(use-package view
  :bind (:map view-mode-map
              ("C-j" . nil)))

(use-package web-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
          (setq web-mode-engines-alist
                '(("go" . "\\.html\\'")))))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (add-hook 'js2-mode-hook (lambda()
                                     "Disable js2 toggle hide functions"
                                     (local-unset-key (kbd "C-c C-f"))
                                     (js2-minor-mode))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init (setq scss-sass-command "sass --style=compressed"))


(use-package haskell-mode
  :ensure t
  :config (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package desktop
  :ensure t
  :bind (:map wh-keymap
              ("d c" . desktop-clear)
              ("d s" . desktop-save)
              ("d r" . desktop-read)
              ("d d" . desktop-remove)))

(use-package time
  ;; Add time to modebar
  :init (setq display-time-string-forms '(24-hours "." minutes))
  :config (display-time))

(use-package wgrep
  :ensure t
  :init (require 'wgrep))

(use-package ansible
  :ensure t
  :defer t)

(use-package ansible-doc
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config (add-hook 'markdown-mode-hook 'auto-fill-mode))


(use-package nginx-mode
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((web-mode-hook . rainbow-mode)
         (scss-mode-hook . rainbow-mode)))


(use-package rust-mode
  :ensure t
  :init (setq rust-format-on-save t)
  :mode "\\.rs\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;; Editing gherkin files (.feature files)
;; BDD
(use-package feature-mode
  :ensure t
  :config (setq feature-default-lanaguage "en")
  :mode "\\.feature\\'")

;; jsonnet is a superset of json
(use-package jsonnet-mode
  :ensure t)

;; ;; Automatically create closing parens, braces, and quotes
;; (use-package elec-pair
;;   :ensure t
;;   :init
;;   :config (electric-pair-mode))

(use-package erc
  :init(progn
         (setq erc-fill-function 'erc-fill-static)
         (setq erc-fill-static-center 22)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")

(use-package load-dir
  :ensure t)

(use-package ztree
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode ".*\.proto\\'")
(use-package rg
  :ensure t
  :init  (progn
           (setq-default rg-command-line-flags '("--sort path")))
  :bind (:map wh-keymap
              ;; Search
              ("s d" . rg-dwim)))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         (:map wh-keymap
               ("w j" . ace-window))))

(use-package zoom-window
  :ensure t
  :bind (:map wh-keymap
              ("w z" . zoom-window-zoom)))

;; Show emojis in emacs
;; (use-package emojify
;;   :ensure t
;;   :hook (after-init . global-emojify-mode))

;; (use-package linum-off
;;   :ensure t
;;   :init (defvar linum-disabled-modes-list '(eshell-mode term-mode ivy-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode)))

(use-package flyspell
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-." . nil)
              :map wh-keymap
              ("w c" . flyspell-auto-correct-word))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package yasnippet
  :ensure t
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
  )


;; Ansi Color interpretation in the compilation buffer
(use-package ansi-color
  :ensure t
  :init (progn
          (defun colorize-compilation-buffer ()
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))
          (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(when (memq window-system '(mac ns))
  (use-package frame
    :straight f
    :init (progn
            (defun kill-fullscreen(frame)
              "When a FRAME is deleted, take it out of fullscreen first. This
fixes the bug where emacs dies when you try to kill a frame"
              (modify-frame-parameters
               frame
               `(
                 (fullscreen
                  . ,(if (eq (frame-parameter frame 'maximized) 'maximized)
                         'maximized)))))
            (add-to-list 'delete-frame-functions #'kill-fullscreen)
            )))

(use-package beacon
  :ensure t
  :config (progn
            (require 'beacon)
            (beacon-mode +1)))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package compile
  :bind (:map wh-keymap
              ("c c" . compile)))

(use-package tab-bar
  :bind ("C-x t s" . tab-bar-select-tab-by-name))

(use-package rainbow-delimiters
  :ensure t)

(use-package code-review
  :straight t
  :custom
  (code-review-github-host "ghe.spotify.net/api/v3")
  (code-review-github-graphql-host "ghe.spotify.net/api")
  (code-review-github-base-url "ghe.spotify.net")
  (code-review-log-raw-request-responses t)
  (code-review-github-diffheader '(("Accept" . "application/vnd.github.v3.diff")))
  :bind (:map forge-topic-mode-map
              ("C-c r" . code-review-forge-pr-at-point))
  )

(use-package git-link
  :ensure t
  :config (progn
            (add-to-list 'git-link-remote-alist '("ghe.spotify.net" git-link-github)))
  :bind ("C-c g l" . git-link))

(use-package project
  :ensure t
  :bind (:map wh-keymap
              ("p f" . project-find-file)
              ))

(use-package smartparens
  :ensure t
  :config (smartparens-global-mode))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'go-mode apheleia-mode-alist)
        '(gofumpt goimports))
  (apheleia-global-mode +1))



(provide 'dot)
;; ;;; dot.el ends here
