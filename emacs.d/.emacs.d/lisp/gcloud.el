;;; dot.el --- load all of my custom packages
;;; Commentary:
;;; Code:

;; make sure you've set your default project with:
;; gcloud config set project <project-name>
(require 'tramp)
(add-to-list 'tramp-methods
  '("gcssh"
    (tramp-login-program        "gcloud compute ssh")
    (tramp-login-args           (("%h")))
    (tramp-async-args           (("-q")))
    (tramp-remote-shell         "/bin/sh")
    (tramp-remote-shell-args    ("-c"))
    (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                 ("-o" "UserKnownHostsFile=/dev/null")
                                 ("-o" "StrictHostKeyChecking=no")))
    (tramp-default-port         22)))

;; ... after which it's as easy as:
;;
;; C-x C-f /gcssh:compute-instance:/path/to/filename.clj
(provide 'gcloud)
;;; dot.el ends here
