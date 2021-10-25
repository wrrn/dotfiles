;;; java-development.el  --- load all the java development packages


(use-package google-c-style
  :straight (google-c-style :type git :host github :repo "google/styleguide"))

(use-package lsp-java 
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package company :ensure t
  :init (company-mode -1)
  :bind ("C-c ." . company-yasnippet)
  :custom
  (company-backends '(company-semantic
                      company-capf
                      company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-ddabrev
                      company-yasnippet
                      ))
  (company-idle-delay 0))

;; Install projectile so that we get some project level commands
(use-package projectile 
  :ensure t
  :config (progn()
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-update-project-type 'maven
                                  :project-file "pom.xml"
                                  :compile "mvn clean compile com.coveo:fmt-maven-plugin:format"
                                  :test "mvn clean test com.coveo:fmt-maven-plugin:format"
                                  :package "mvn clean package"
                                  :test-suffix "Test"
                                  :src-dir "main/src/"
                                  :test-dir "main/test/")))


(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook
          (lambda ()
            (projectile-mode 1)
            (company-mode 1)
            (google-set-c-style)
            (setq c-basic-offset 2
                  tab-width      2)
            (set (make-local-variable 'compile-command) "mvn clean compile test com.coveo:fmt-maven-plugin:format")))


;; (use-package dap-mode
;;   :ensure t
;;   :after (lsp-mode)
;;   :function dap-hydra/nil
;;   :config)
(provide 'java-development)
;; lsp-ivy
