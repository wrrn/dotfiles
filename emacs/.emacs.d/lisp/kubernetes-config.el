;; kubernetes-config.el -- Add kube.el packages
(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))
