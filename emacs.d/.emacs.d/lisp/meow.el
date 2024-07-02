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

(defcustom meow-selection-char-bounds-table
  '((?i . inner)
    (?o . outer)
    (?b . beginning)
    (?e . end))
  "Mapping from char to bounds."
  :group 'meow
  :type '(alist :key-type (character :tag "Char")
                :key-value (symbol :tag "Thing")))

(defvar meow--bounds-registry
  '((inner . meow-inner-of-thing)
    (outer . meow-bounds-of-thing)
    (beginning . meow-beginning-of-thing)
    (end . meow-end-of-thing))
  "Mapping from bounds to function")

(defun meow--selection-render-char-bounds-table ()
  (let* ((ww (frame-width))
         (w 25)
         (col (min 5 (/ ww w))))
    (thread-last
      meow-selection-char-bounds-table
      (seq-group-by #'cdr)
      (seq-sort-by #'car #'string-lessp)
      (seq-map-indexed
       (lambda (th-pairs idx)
         (let* ((th (car th-pairs))
                (pairs (cdr th-pairs))
                (pre (thread-last
                       pairs
                       (mapcar (lambda (it) (char-to-string (car it))))
                       (meow--string-join " "))))
           (format "%s%s%s%s"
                   (propertize
                    (meow--string-pad pre 8 32 t)
                    'face 'font-lock-constant-face)
                   (propertize " → " 'face 'font-lock-comment-face)
                   (propertize
                    (meow--string-pad (symbol-name th) 13 32 t)
                    'face 'font-lock-function-name-face)
                   (if (= (1- col) (mod idx col))
                       "\n"
                     " ")))))
      (string-join)
      (string-trim-right))))


(defun meow--selection-prompt ()
  (read-char
   (concat (meow--selection-render-char-bounds-table) "\n" "Bounds:")))

(defun meow-selection (bounds)
  "Start the selection of a THING by selecting it BOUNDS"
  (interactive (list (meow--selection-prompt)))
  (when-let* ((bounds-type (assoc bounds meow-selection-char-bounds-table))
              (bounds-fn (assoc (cdr bounds-type) meow--bounds-registry)))
    (call-interactively (cdr bounds-fn))))

(defun meow-normal-self-insert ()
  "Insert a character in Meow's normal mode."
  (interactive)
  (let ((current-state (meow--current-state)))
    (meow-insert-mode 1)
    (call-interactively 'self-insert-command)
    (meow--switch-state current-state)
    ))

(use-package meow
  :ensure t
  :custom
  (meow-keypad-start-keys
   '((?c . ?c)
     (?h . ?h)
     (?x . ?x)
     (?j . ?j)
     (?l . ?l)))
  (meow-mode-state-list
   '((authinfo-mode . normal)
     (beancount-mode . normal)
     (bibtex-mode . normal)
     (cider-repl-mode . normal)
     (cider-test-report-mode . normal)
     (cider-browse-spec-view-mode . motion)
     (cargo-process-mode . normal)
     (conf-mode . normal)
     (deadgrep-edit-mode . normal)
     (deft-mode . normal)
     (diff-mode . normal)
     (ediff-mode . motion)
     (gud-mode . normal)
     (haskell-interactive-mode . normal)
     (help-mode . normal)
     (helpful-mode . normal)
     (json-mode . normal)
     (jupyter-repl-mode . normal)
     (mix-mode . normal)
     (occur-edit-mode . normal)
     (pass-view-mode . normal)
     (prog-mode . normal)
     (py-shell-mode . normal)
     (restclient-mode . normal)
     (telega-chat-mode . normal)
     (term-mode . normal)
     (text-mode . normal)
     (vterm-mode . insert)
     (magit-mode . insert)
     (Custom-mode . normal)))
  (meow-char-thing-table
   '((?\( . round)
     (?\) . round)
     (?\{ . curly)
     (?\} . curly)
     (?\[ . square)
     (?\] . square)
     (?\" . string)
     (?\' . string)
     (?e . symbol)
     (?w . window)
     (?b . buffer)
     (?p . paragraph)
     (?l . line)
     (?v . visual-line)
     (?d . defun)
     (?. . sentence)))

  ;; :custom-face
  ;; (meow-normal-indicator ((t (:inherit nano-modeline-status))))
  ;; (meow-motion-indicator ((t (:inherit nano-modeline-status))))
  ;; (meow-keypad-indicator ((t (:inherit nano-modeline-status))))
  ;; (meow-insert-indicator ((t (:inherit nano-modeline-status))))
  ;; (meow-beacon-indicator ((t (:inherit nano-modeline-status))))



  :hook
  ;; (vterm-mode . (lambda ()
  ;;                 (interactive)
  ;;                 (advice-add 'meow--execute-kbd-macro :before #'meow--vterm-execute-kbd-macro)))
  (vterm-copy-mode . meow-insert-exit)

  :init (progn
          (require 'meow)
          (defun meow-setup ()
            (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
            (add-to-list 'meow-expand-exclude-mode-list 'magit-mode)
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
             '("," . meow-selection)
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
             '("\"" . meow-normal-self-insert)
             '("["  . meow-normal-self-insert)
             '("]"  . meow-normal-self-insert)
             '("{"  . meow-normal-self-insert)
             '("}"  . meow-normal-self-insert)
             '("("  . meow-normal-self-insert)
             '(")"  . meow-normal-self-insert)
             '("<escape>" . ignore)))
          (meow-setup)
          (meow-global-mode 1)
          ;; Unbind the C-x C-p (default mark-page) so that we can use meow-command pallete with project specific commands
          (unbind-key "C-x C-p"))
  :bind (("C-x C-o" . ace-window)
         ("C-x C-b" . consult-buffer)
         ("C-x C-p f" . project-find-file)
         ("C-x C-p t" . multi-vterm-project)

         :map persp-mode-map
         ("C-x M-p C-s" . persp-switch)))
(provide 'meow)
