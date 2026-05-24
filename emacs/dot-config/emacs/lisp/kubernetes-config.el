;; kubernetes-config.el -- Add kube.el packages with ghostel support
(use-package kubel
  :config
  (defun kubel-exec-ghostel-pod ()
    "Exec into the pod under the cursor -> ghostel."
    (interactive)
    (let* ((con-pod (kubel--get-container-under-cursor))
           (default-directory (kubel--tramp-url con-pod))
           (ghostel-buffer-name (kubel--shell-buffer-name "ghostel" (car con-pod) (cdr con-pod))))
      (ghostel)))
  (transient-append-suffix 'kubel-exec-popup "t"
    '("g" "Ghostel" kubel-exec-ghostel-pod)))
