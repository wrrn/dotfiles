;;; herdr-config.el  --- configure herdr.el  -*- lexical-binding: t; -*-
;;; Commentary:
;; herdr.el shows the agents running under herdr and opens their panes
;; in ghostel buffers.  It ships no herdr.el; herdr-ui is the entry point.
;; Without a main file straight can't read its dependencies, so
;; magit-section is declared here.  ghostel comes from term-config.
;;; Code:

(use-package magit-section
  :defer t)

(defun wrrn-herdr--remote-command-any-shell (command)
  "Run the POSIX COMMAND under sh, whatever the remote login shell is.
sshd hands a remote command to the login shell, and fish can't parse
herdr.el's machine probe.  Base64 carries it past that shell untouched."
  (format "echo %s | base64 -d | sh"
          (base64-encode-string (encode-coding-string command 'utf-8) t)))

(defun wrrn-herdr--bind-row-keys (map)
  "Move between rows of the herdr panel MAP with j and k.
The panels open in meow's motion state, whose j and k run `meow-next'
and `meow-prev' ahead of MAP, so remap those as well."
  (keymap-set map "j" #'herdr-panel-next-row)
  (keymap-set map "k" #'herdr-panel-previous-row)
  (keymap-set map "<remap> <meow-next>" #'herdr-panel-next-row)
  (keymap-set map "<remap> <meow-prev>" #'herdr-panel-previous-row))

(defun wrrn-herdr--section-value (type)
  "Return the value of the section of TYPE at or around point, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) type)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun wrrn-herdr--machine (id)
  "Return the remote machine called ID, or nil for this one."
  (unless (member id '(nil "local"))
    (seq-find (lambda (machine) (equal (plist-get machine :id) id))
              (herdr-machines-list))))

(defun wrrn-herdr--snapshot-node (machine key id-key id)
  "Return the node under KEY whose ID-KEY is ID in MACHINE's tree.
A nil MACHINE means this one."
  (seq-find (lambda (node) (equal (gethash id-key node) id))
            (gethash key (if machine
                             (plist-get machine :snapshot)
                           herdr-session--snapshot))))

(defun wrrn-herdr--request (machine method params)
  "Call METHOD with PARAMS on MACHINE's herdr server.
A nil MACHINE means this one; a remote one is reached through its tunnel.
The panels redraw themselves once the session or probe sees the change."
  (let ((herdr-api-socket (if machine
                              (plist-get (herdr-machines-connection machine)
                                         :socket)
                            herdr-api-socket)))
    (herdr-api-request method params)))

(defun wrrn-herdr-rename-workspace ()
  "Rename the workspace at point in the Spaces panel."
  (interactive)
  (let* ((id (or (wrrn-herdr--section-value 'herdr-workspace)
                 (user-error "No workspace at point")))
         (machine (wrrn-herdr--machine
                   (wrrn-herdr--section-value 'herdr-machine)))
         (workspace (wrrn-herdr--snapshot-node
                     machine "workspaces" "workspace_id" id))
         (label (string-trim
                 (read-string "Rename workspace to: "
                              (and workspace (gethash "label" workspace))))))
    (when (string-empty-p label)
      (user-error "A workspace needs a name"))
    (wrrn-herdr--request machine "workspace.rename"
                         (list :workspace_id id :label label))))

(defun wrrn-herdr-rename-agent ()
  "Rename the agent at point in the Agents panel.
An empty name clears the one set before.  A title the agent reports
for itself still shows ahead of the name."
  (interactive)
  (let* ((target (or (wrrn-herdr--section-value 'herdr-agent)
                     (user-error "No agent at point")))
         (machine (and (consp target)
                       (wrrn-herdr--machine
                        (plist-get (plist-get target :machine) :id))))
         (pane (if (consp target) (plist-get target :pane) target))
         (agent (wrrn-herdr--snapshot-node machine "agents" "pane_id" pane))
         (name (string-trim
                (read-string "Rename agent to (empty clears): "
                             (and agent (gethash "name" agent))))))
    (wrrn-herdr--request machine "agent.rename"
                         (list :target pane
                               :name (if (string-empty-p name) :null name)))))

(use-package herdr-ui
  :straight (herdr :host github :repo "roman/herdr.el")
  :bind (:map wh-keymap
              ("h h" . herdr-ui)
              ("h s" . herdr-spaces-visit)
              ("h a" . herdr-agents-visit)))

;; The visit commands autoload herdr-spaces and herdr-agents without
;; herdr-ui, so hook each library rather than herdr-ui's :config.
(with-eval-after-load 'herdr-machines
  ;; Without this, machines whose login shell is fish never come online.
  (advice-add 'herdr-machines--remote-command :filter-return
              #'wrrn-herdr--remote-command-any-shell))

(with-eval-after-load 'herdr-spaces
  (wrrn-herdr--bind-row-keys herdr-spaces-mode-map)
  (keymap-set herdr-spaces-mode-map "r" #'wrrn-herdr-rename-workspace))

(with-eval-after-load 'herdr-agents
  (wrrn-herdr--bind-row-keys herdr-agents-mode-map)
  (keymap-set herdr-agents-mode-map "r" #'wrrn-herdr-rename-agent))

(provide 'herdr-config)
;;; herdr-config.el ends here
