;;; fish-config.el  --- configure meow mode
;; fish configuration

;; 
(defun wrrn/fish-capf ()
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (point)))
         (start (save-excursion
                  (skip-syntax-backward "^ " (line-beginning-position))
                  (point)))
         (pairs (with-temp-buffer
                  (call-process "fish" nil t nil "-c"
                                (concat "complete -C"
                                        (shell-quote-argument line)))
                  (mapcar (lambda (l) (split-string l "\t"))
                          (split-string (buffer-string) "\n" t))))
         (cands (mapcar #'car pairs))
         (descs (mapcar (lambda (p) (cadr p)) pairs)))
    (when cands
      (list start (point) cands
            :annotation-function
            (lambda (cand)
              (when-let ((i (cl-position cand cands :test #'string=)))
                (when-let ((d (nth i descs)))
                  (concat "  " d))))))))


(use-package fish-mode
  :ensure t
  :hook ((fish-mode . (lambda ()
                        (add-hook 'completion-at-point-functions
                                  #'wrrn/fish-capf nil t)))))


(provide 'fish-config)
