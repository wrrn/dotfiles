;;; wrrn-modeline.el --- Echo area info -*- lexical-binding: t -*-

;;; Commentary:
;; Display buffer info in echo area, minimal modeline as window border.

;;; Source: https://github.com/smallwat3r/emacs/blob/main/lisp/sw-modeline.el
;;; License:
;; MIT License

;; Copyright (c) 2026 Matthieu Petiteau

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;; Code:

;; Echo area info display
(defvar wrrn--echo-area-last-info nil
  "The info string last written to the inactive minibuffer.")

(defun wrrn--update-echo-area ()
  "Show buffer info right-aligned in the inactive minibuffer.
The minibuffer window shows \" *Minibuf-0*\" whenever the echo area
is empty.  Messages cover it for a while, and it reappears when
they're cleared, so it never flashes and never fights other messages."
  (unless (active-minibuffer-window)
    (let ((info (format "%s%s  %s %s,%d"
                        (if (buffer-modified-p) "** " "")
                        (buffer-name)
                        (format-mode-line "%p")
                        (format-mode-line "%l")
                        (current-column))))
      (with-current-buffer (get-buffer-create " *Minibuf-0*")
        (unless (and (equal info wrrn--echo-area-last-info)
                     (> (buffer-size) 0))
          (setq wrrn--echo-area-last-info info)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize
                     " " 'display
                     `(space :align-to (- right ,(1+ (string-width info)))))
                    info))))
      ;; Without this, the minibuffer window can keep showing old text
      (force-window-update (minibuffer-window)))))

(run-with-idle-timer 0.1 t #'wrrn--update-echo-area)
(add-hook 'post-command-hook #'wrrn--update-echo-area)

;; Modeline is only used to distinguish active window via color.
;; Hide it when there's only one window since there's nothing to distinguish.
(defun wrrn--update-mode-line-visibility ()
  "Show mode-line only when frame has multiple windows."
  (let ((fmt (if (> (count-windows) 1) " " nil)))
    (dolist (win (window-list))
      (with-current-buffer (window-buffer win)
        (unless (eq mode-line-format fmt)
          (setq mode-line-format fmt))))))

(add-hook 'window-configuration-change-hook #'wrrn--update-mode-line-visibility)

(defun wrrn--set-mode-line-faces (&rest _)
  "Thin mode-line bars colored from the current theme."
  (let* ((border (face-attribute 'vertical-border :foreground nil t))
         (border (if (stringp border) border
                   (face-attribute 'mode-line-inactive :background nil t)))
         (accent (face-attribute 'cursor :background nil t)))
    (set-face-attribute 'mode-line nil
                        :background accent :box nil :height 0.1)
    (set-face-attribute 'mode-line-inactive nil
                        :background border :box nil :height 0.1)))

(wrrn--set-mode-line-faces)
;; Enabling a theme resets face attributes, reapply after each one
(add-hook 'enable-theme-functions #'wrrn--set-mode-line-faces)


(provide 'wrrn-modeline)
;;; wrrn-modeline.el ends here
