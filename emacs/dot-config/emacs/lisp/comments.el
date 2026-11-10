;; comments.el --- configure how comments are handled -*- lexical-binding: t; -*-
;; Configuring comments since 2023

;; Running comment-dwim will comment the line instead of adding a comment at the end.
(global-set-key (kbd "M-;") 'comment-line)
(provide 'comments)
