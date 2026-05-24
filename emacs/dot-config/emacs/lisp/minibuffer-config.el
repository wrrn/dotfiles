;; minibuffer-config.el -- Configure minibuffer packages
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; Isearch integration
         ("C-s" . consult-line)
         ("M-s l m" . consult-line-multi)

         :map wh-keymap
         ("f f" . consult-imenu)
         ("s s" . consult-ripgrep))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Set the root of the project so that consult-ripgrep starts searches there.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (consult-customize consult--source-recent-file :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (add-to-list 'consult-buffer-sources `(:name "vterminal"
                                               :hidden f
                                               :narrow ?v
                                               :category buffer
                                               :state    ,#'consult--buffer-state
                                               :items ,(lambda()
                                                         (when (boundp 'multi-vterm-buffer-name)
                                                           (mapcar #'buffer-name
                                                                   (persp-buffer-list-filter
                                                                    (match-buffers multi-vterm-buffer-name))))))
               'append)
  (add-to-list 'consult-buffer-sources `(:name "eat"
                                               :hidden f
                                               :narrow ?e
                                               :category buffer
                                               :state  ,#'consult--buffer-state
                                               :items ,(lambda()
                                                         (when (boundp 'eat-buffer-name)
                                                           (mapcar #'buffer-name
                                                                   (persp-buffer-list-filter
                                                                    (match-buffers "eat\\*"))))))
               'append)
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(provide 'minibuffer-config)
