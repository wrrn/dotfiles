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
(require 'python-config)
(require 'docker-config)
(require 'yaml-config)
(require 'toml-config)
(require 'json-config)
(require 'nix-config)
;; (require 'java-development)
;; (require 'scala-development)
(require 'writeroom-config)
(require 'term-config)
(require 'gpt)
(require 'ux-config)
(require 'minibuffer-config)
(require 'meow)
(require 'ui-config)
(require 'git-config)
(require 'code-nav)

(use-package view
  :bind (:map view-mode-map
              ("C-j" . nil)))

(use-package editorconfig
  :config
  (editorconfig-mode))

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

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package desktop
  :ensure t
  :bind (:map wh-keymap
              ("d c" . desktop-clear)
              ("d s" . desktop-save)
              ("d r" . desktop-read)
              ("d d" . desktop-remove)))

(use-package wgrep
  :ensure t
  :init (require 'wgrep))

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

;; jsonnet is a superset of json
(use-package jsonnet-mode
  :ensure t)

(use-package load-dir
  :ensure t)

(use-package ztree
  :ensure t)

(use-package protobuf-ts-mode
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
