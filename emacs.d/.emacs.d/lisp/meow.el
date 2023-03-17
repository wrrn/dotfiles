;;; meow.el  --- configure meow mode
;; Enable scala-mode for highlighting, indentation and motion commands

;; TODO
;; 1. Enable meow mode in the minibuffer
;; 2. Use sexp instead of meow-modes weird character implementation
;; 3. Add a meow-comment command to motion mode.

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

(use-package meow
  :ensure t
  :custom
  (meow-keypad-start-keys
   '((?c . ?c)
     (?h . ?h)
     (?x . ?x)
     (?j . ?j)))

  :hook (vterm-mode . (lambda ()
                        (interactive)
                        (advice-add 'meow--execute-kbd-macro :before #'meow--vterm-execute-kbd-macro)))

  :init (progn
          (require 'meow)
          (defun meow-setup ()
              (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
              (meow-motion-overwrite-define-key
               '("j" . meow-next)
               '("k" . meow-prev)
               '("<escape>" . ignore))
              (meow-leader-define-key
               ;; SPC j/k will run the original command in MOTION state.
               '("j" . "H-j")
               '("k" . "H-k")
               ;; Use SPC (0-9) for digit arguments.
               '("1" . meow-digit-argument)
               '("2" . meow-digit-argument)
               '("3" . meow-digit-argument)
               '("4" . meow-digit-argument)
               '("5" . meow-digit-argument)
               '("6" . meow-digit-argument)
               '("7" . meow-digit-argument)
               '("8" . meow-digit-argument)
               '("9" . meow-digit-argument)
               '("0" . meow-digit-argument)
               '("/" . meow-keypad-describe-key)
               '("?" . meow-cheatsheet))
              (meow-normal-define-key
               '("0" . meow-expand-0)
               '("9" . meow-expand-9)
               '("8" . meow-expand-8)
               '("7" . meow-expand-7)
               '("6" . meow-expand-6)
               '("5" . meow-expand-5)
               '("4" . meow-expand-4)
               '("3" . meow-expand-3)
               '("2" . meow-expand-2)
               '("1" . meow-expand-1)
               '("-" . negative-argument)
               '(";" . meow-reverse)
               '("," . meow-inner-of-thing)
               '("." . meow-bounds-of-thing)
               '("[" . meow-beginning-of-thing)
               '("]" . meow-end-of-thing)
               '("a" . meow-append)
               '("A" . meow-open-below)
               '("b" . meow-back-word)
               '("B" . meow-back-symbol)
               '("c" . meow-change)
               '("d" . meow-delete)
               '("D" . meow-backward-delete)
               '("e" . meow-next-word)
               '("E" . meow-next-symbol)
               '("f" . meow-find)
               '("g" . meow-cancel-selection)
               '("G" . meow-grab)
               '("h" . meow-left)
               '("H" . meow-left-expand)
               '("i" . meow-insert)
               '("I" . meow-open-above)
               '("j" . meow-next)
               '("J" . meow-next-expand)
               '("k" . meow-prev)
               '("K" . meow-prev-expand)
               '("l" . meow-right)
               '("L" . meow-right-expand)
               '("m" . meow-join)
               '("n" . meow-search)
               '("o" . meow-block)
               '("O" . meow-to-block)
               '("p" . meow-yank)
               '("q" . meow-quit)
               '("Q" . meow-goto-line)
               '("r" . meow-replace)
               '("R" . meow-swap-grab)
               '("s" . meow-kill)
               '("S" . meow-kill-whole-line)
               '("t" . meow-till)
               '("u" . meow-undo)
               '("U" . meow-undo-in-selection)
               '("v" . meow-visit)
               '("w" . meow-mark-word)
               '("W" . meow-mark-symbol)
               '("x" . meow-line)
               '("X" . meow-goto-line)
               '("y" . meow-save)
               '("Y" . meow-sync-grab)
               '("z" . meow-pop-selection)
               '("'" . repeat)
               '("<escape>" . ignore)))
            (meow-setup)
            (meow-global-mode 1))

  :bind (("C-x C-o" . ace-window)
         ("C-x C-b" . consult-buffer))
)
(provide 'meow)
