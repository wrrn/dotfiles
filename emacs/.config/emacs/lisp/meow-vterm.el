;;; meow-vterm.el  --- meow-mode for vterm
;; Fix issues that I'm running into in vterm

(defun meow--vterm-execute-kbd-macro (kbd-macro)
  ;; The reason we need this is because vterm--self-insert pulls the
  ;; last-command-event. In our case k is mapped to C-p, but because meow-prev
  ;; just reads command for the macro (which is vterm--self-insert) and the
  ;; last-command-event is k. Sooo we need to set the last-command-event to the
  ;; actual key strokes for the macro so that vterm forwards those key strokes
  ;; to the C compiled vterm.
  ;;
  ;; TODO: I couldn't figure out how to set the last-command-event directly but
  ;; (read-event) returns it in the correct format.
  (when-let* ((ret (key-binding (read-kbd-macro kbd-macro)))
              (is-self-insert (meow--is-self-insertp ret))
              (is-vterm-mode (equal major-mode 'vterm-mode)))
    (setq unread-command-events (listify-key-sequence (kbd kbd-macro)))
    (setq last-command-event (read-event))))


;; I want a way to e


(provide 'meow-vterm)
