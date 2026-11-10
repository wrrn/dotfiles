;;; fish-config.el  --- configure meow mode  -*- lexical-binding: t; -*-
;; fish configuration

;; 
(defun wrrn/fish--complete (line)
  "Return fish completions for LINE as (CANDIDATE DESCRIPTION) lists."
  ;; An async process lets `while-no-input' (which corfu wraps around
  ;; candidate computation) abort the wait when a key arrives.
  (let* ((buf (generate-new-buffer " *fish-complete*" t))
         (proc (make-process
                :name "fish-complete" :buffer buf :noquery t
                :connection-type 'pipe :sentinel #'ignore
                :command (list "fish" "-c"
                               (concat "complete -C"
                                       (shell-quote-argument line)
                                       " 2>/dev/null")))))
    (unwind-protect
        (progn
          (while (accept-process-output proc))
          (with-current-buffer buf
            (mapcar (lambda (l) (split-string l "\t"))
                    (split-string (buffer-string) "\n" t))))
      (delete-process proc)
      (kill-buffer buf))))

(defun wrrn/fish--base (word)
  "Return the part of WORD that fish needs to list its candidates.
That is the directory part of a path, or a leading - or $ for
options and variables.  Fish lists everything else from an empty word."
  (if (string-match "\\`\\(?:.*/\\|[-$]\\)" word)
      (match-string 0 word)
    ""))

(defun wrrn/fish--escape (s)
  "Backslash-escape S the way `string escape --no-quoted' does in fish.
Also escape #, which starts a comment at the beginning of a token."
  (replace-regexp-in-string
   "[] \t\n$*~%#(){}[<>&|;\"'\\]"
   (lambda (c) (pcase c ("\t" "\\t") ("\n" "\\n") (_ (concat "\\" c))))
   s t t))

(defun wrrn/fish--unescape (s)
  "Remove the backslash escapes from S."
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" s t))

(defun wrrn/fish--word-start ()
  "Return the start of the fish word before point.
A backslash-escaped space does not end the word."
  (save-excursion
    (let ((bol (line-beginning-position)))
      (skip-syntax-backward "^ " bol)
      (while (and (> (point) (1+ bol))
                  (eq (char-before (1- (point))) ?\\))
        (backward-char)
        (skip-syntax-backward "^ " bol))
      (point))))

(defun wrrn/fish-capf ()
  (let* ((start (wrrn/fish--word-start))
         (before (buffer-substring-no-properties
                  (line-beginning-position) start))
         (cache (make-hash-table :test #'equal))
         (descs (make-hash-table :test #'equal))
         (fetch
          (lambda (base)
            (or (gethash base cache)
                (puthash
                 base
                 (let ((raw-base (wrrn/fish--unescape base)))
                   (mapcan (pcase-lambda (`(,cand ,desc))
                             (when (string-prefix-p raw-base cand)
                               (let ((field (wrrn/fish--escape
                                             (substring cand (length raw-base)))))
                                 (when desc (puthash field desc descs))
                                 (list field))))
                           (wrrn/fish--complete (concat before base))))
                 cache)))))
    ;; Fish lists every candidate after the base and orderless does the
    ;; matching.  The base is a completion boundary, so orderless only
    ;; matches the field after it and passes the base back to the table.
    ;; The base stays as typed; fish returns candidates unescaped, so the
    ;; field is escaped before it is inserted.
    (list start (point)
          (lambda (word pred action)
            (let ((base (wrrn/fish--base word)))
              (pcase action
                ('metadata '(metadata (category . fish)))
                (`(boundaries . ,suffix)
                 `(boundaries ,(length base)
                              . ,(or (string-search "/" suffix) (length suffix))))
                (_ (completion-table-with-context
                    base (funcall fetch base)
                    (substring word (length base)) pred action)))))
          :exclusive 'no
          :annotation-function
          (lambda (cand)
            (and-let* ((d (gethash cand descs)))
              (concat "  " d))))))


(use-package fish-mode
  :ensure t
  :hook ((fish-mode . (lambda ()
                        (add-hook 'completion-at-point-functions
                                  #'wrrn/fish-capf nil t)))))


(provide 'fish-config)
