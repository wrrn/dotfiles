;; darwin-config.el -- Configure windows for darwin to use.

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


(provide 'darwin-config)
