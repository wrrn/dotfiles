;;; pi-config.el  --- configure pi-coding-agent  -*- lexical-binding: t; -*-
;; Configuration

(use-package pilish
  :ensure t
  :bind (("C-c p" . pilish)
         ("C-c P" . wrrn-pilish-jump))
  :config
  (add-hook 'pilish-activity-phase-functions #'wrrn-pilish--notify-done)
  (advice-add 'pilish--handle-extension-ui-request
              :before #'wrrn-pilish--notify-waiting)
  (advice-add 'pilish--list-project-files
              :around #'wrrn-pilish--remote-project-files))

;; In a TRAMP project, pilish runs pi on the remote host, and the agent's
;; commands inherit TRAMP's PATH.  Take that PATH from the remote login
;; shell: on NixOS hosts pi and the user's tools live in profile
;; directories TRAMP doesn't search by default.
(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Keep the ssh master for 10 minutes after its last session closes,
  ;; so a TRAMP reconnect doesn't take the remote pi down with it.
  ;; TRAMP passes these on the command line, which overrides
  ;; ~/.ssh/config.  %% because TRAMP treats the string as a format.
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=10m"))

;; /rpc:host: paths talk to a small server on the host instead of
;; driving a remote shell.  Processes start with no shell setup, and
;; fish as the login shell doesn't matter.  The server is built with Nix
;; on lona, from the same commit as this package (the protocol must
;; match exactly).  After bumping tramp-rpc, rebuild it there:
;;   nix build github:ArthurHeymans/emacs-tramp-rpc/COMMIT#tramp-rpc-server \
;;     -o ~/.local/state/tramp-rpc/server
;; The path is the same for every host, so other hosts need the server
;; at that path too.
(use-package tramp-rpc
  :straight (tramp-rpc :host github :repo "ArthurHeymans/emacs-tramp-rpc"
                       :files ("lisp/*.el"))
  :after tramp
  :custom
  (tramp-rpc-deploy-never-deploy t)
  (tramp-rpc-deploy-remote-binary-path
   "/home/warren/.local/state/tramp-rpc/server/bin/tramp-rpc-server"))

;; No `tramp-direct-async-process': it hands the remote login shell a
;; POSIX command line, which fish on lona rejects.  In TRAMP 2.8.2 it
;; also skips the ControlMaster options above (tramp.el still calls the
;; renamed `tramp-ssh-controlmaster-options').

;; Pilish's path and @file completions ask TRAMP for directory listings
;; and run git or find, and Emacs waits for each answer.  In remote
;; sessions, keep corfu from running them on every keystroke; TAB still
;; completes on demand.  Checked per command because pilish sets the
;; input buffer's `default-directory' after `pilish-input-mode' runs.
(defun wrrn-pilish--guard-auto-completion ()
  "Stop corfu auto completion when this pilish input buffer is remote."
  (if (file-remote-p default-directory)
      (setq-local corfu-auto-commands nil)
    (kill-local-variable 'corfu-auto-commands)))

(add-hook 'pilish-input-mode-hook
          (lambda ()
            (add-hook 'pre-command-hook
                      #'wrrn-pilish--guard-auto-completion nil t)))

;; Typing @ lists project files for completion.  Outside a git
;; repository pilish falls back to `find' over the whole tree; in ~ on
;; lona that is ~1M files and 90 MB, and Emacs waits for all of it.
(defun wrrn-pilish--remote-project-files (list-files)
  "Call LIST-FILES locally; in remote sessions, list git files only."
  (let ((default-directory (pilish--session-directory)))
    (if (file-remote-p default-directory)
        (split-string
         (shell-command-to-string
          "git ls-files --cached --others --exclude-standard 2>/dev/null")
         "\n" t)
      (funcall list-files))))

;;; Sessions

(defun wrrn-pilish--chat-buffers ()
  "Live pilish chat buffers, most recently used first."
  (seq-filter (lambda (buf)
                (eq (buffer-local-value 'major-mode buf) 'pilish-chat-mode))
              (buffer-list)))

(defun wrrn-pilish--where (buf)
  "\"@host\" when BUF's session runs over TRAMP, else \"local\"."
  (let ((host (file-remote-p (buffer-local-value 'default-directory buf) 'host)))
    (if host (concat "@" host) "local")))

;;; Switcher: an Agents group in `consult-buffer' (narrow with a)

(defun wrrn-pilish--annotate (name)
  "Show the phase and host of the chat buffer NAME."
  (when-let* ((buf (get-buffer name)))
    (format "  %-9s %s"
            (buffer-local-value 'pilish--activity-phase buf)
            (wrrn-pilish--where buf))))

(defvar wrrn-pilish-consult-source
  `(:name     "Agents"
    :narrow   ?a
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :items    ,(lambda () (mapcar #'buffer-name (wrrn-pilish--chat-buffers)))
    :annotate ,#'wrrn-pilish--annotate
    ;; `pilish' from a chat buffer restores the input pane.
    :action   ,(lambda (name)
                 (switch-to-buffer name)
                 (pilish)))
  "Consult source listing pilish sessions.")

(with-eval-after-load 'consult
  (add-to-list 'consult-buffer-sources 'wrrn-pilish-consult-source 'append))

(defun wrrn-pilish-jump ()
  "Pick a pilish session and switch to it."
  (interactive)
  ;; `consult--multi' isn't autoloaded, and consult loads lazily.
  (require 'consult)
  (consult--multi '(wrrn-pilish-consult-source) :prompt "Agent: "))

;;; Desktop notifications

(defun wrrn-pilish--notify (title body)
  "Show a desktop notification with TITLE and BODY.
D-Bus on Linux, terminal-notifier on macOS."
  ;; Run notifiers on this machine even when the session is remote.
  (let ((default-directory temporary-file-directory))
    (cond ((featurep 'dbusbind)
           (require 'notifications)
           (notifications-notify :title title :body body))
          ((executable-find "terminal-notifier")
           (call-process "terminal-notifier" nil 0 nil
                         "-title" title "-message" body))
          (t (message "%s: %s" title body)))))

(defun wrrn-pilish--quiet-p (chat)
  "Non-nil when CHAT is on screen in the focused frame."
  (and (frame-focus-state)
       (get-buffer-window chat 'visible)))

(defun wrrn-pilish--notify-done (chat _input old new reason)
  "Notify when CHAT goes from busy to idle while it isn't on screen."
  (when (and (eq reason 'phase-change)
             (not (equal old "idle"))
             (equal new "idle")
             (buffer-live-p chat)
             (not (wrrn-pilish--quiet-p chat)))
    (wrrn-pilish--notify "Pi finished"
                         (format "%s %s" (buffer-name chat)
                                 (wrrn-pilish--where chat)))))

(defun wrrn-pilish--notify-waiting (event)
  "Notify when an extension prompt in EVENT needs an answer.
Runs before `pilish--handle-extension-ui-request', inside the chat buffer."
  (when (and (member (plist-get event :method)
                     '("confirm" "select" "input" "editor"))
             (not (wrrn-pilish--quiet-p (current-buffer))))
    (wrrn-pilish--notify "Pi needs you"
                         (string-join
                          (delq nil (list (buffer-name)
                                          (wrrn-pilish--where (current-buffer))
                                          (plist-get event :title)))
                          " "))))

(provide 'pi-config)
