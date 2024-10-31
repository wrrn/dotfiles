;;; no-littering.el --- Help keeping your directories clean  -*- lexical-binding:t -*-
;; Pulled from https://github.com/emacscollective/no-littering/blob/main/no-littering.el#L483-L542
;; Copyright (C) 2016-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/no-littering
;; Keywords: convenience

;; Package-Requires: ((emacs "25.1") (compat "29.1.4.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;; Change where backups of various sorts are created.

;; The purpose of this package is to store data files of various
;; sorts in a handful of central locations, instead of spreading
;; them all over the place.  When doing that for temporary files,
;; which contain backups of some sort, that increases the odds that
;; sensitive data is written to disk in clear text and/or that such
;; clear text files persist longer, if they would be created anyway.


;; The default values of these variables cause additional files to
;; be created in the same directories as the files that are being
;; visited.  Calling this function changes the values of these
;; variables, so that this is only done for visited files located in
;; certain directories.  For all other visited files, the additional
;; files are created in files inside `no-littering-var-directory'.

;; Additional files are created in the same directory as the visited
;; file, for files located in:
;; - \"/tmp/\"
;; - \"/dev/shm\"
;; - `temporary-file-directory'

;; With these settings it is still possible that sensitive data is
;; written to additional files, but you are more likely to spot it,
;; and because these directories usually use a `tmpfs' file-system,
;; the leaked secrets should not persist after a reboot.

;; If you do *not* call this function, then these additional files
;; are always created in the same directory as the visited files,
;; regardless of the location of the visited files.  In other words,
;; even when using the default values, there is a significant risk
;; of leaking sensitive data, and if you want to reduce that, then
;; you must turn of these features completely.

(defvar no-littering-var-directory
  (expand-file-name (convert-standard-filename "var/") user-emacs-directory)
  "The directory where packages place their persistent data files.
This variable has to be set before `no-littering' is loaded.")

;;;###autoload
(defun no-littering-expand-var-file-name (file)
  "Expand filename FILE relative to `no-littering-var-directory'."
  (expand-file-name (convert-standard-filename file)
                    no-littering-var-directory))

(defvar no-littering-auto-save-directory
  (no-littering-expand-var-file-name "auto-save/"))

(defvar no-littering-lock-directory
  (no-littering-expand-var-file-name "lock/"))

(defvar no-littering-backup-directory
  (no-littering-expand-var-file-name "backup/"))


(defvar no-littering-undo-tree-hist-directory
  (no-littering-expand-var-file-name "undo-tree-hist/"))

(make-directory no-littering-var-directory t)
(make-directory no-littering-auto-save-directory t)
(make-directory no-littering-lock-directory t)
(make-directory no-littering-backup-directory t)
(make-directory no-littering-undo-tree-hist-directory t)

(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
        ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
        ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
        (".*" ,no-littering-auto-save-directory t)))

(setq lock-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
        ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
        ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
        (".*" ,no-littering-lock-directory t)))

(setq backup-directory-alist
      `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
        ("\\`/tmp/" . nil)
        ("\\`/dev/shm/" . nil)
        ("." . no-littering-backup-directory)))
(setq undo-tree-history-directory-alist
      `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
        ("\\`/tmp/" . nil)
        ("\\`/dev/shm/" . nil)
        ("." . no-littering-undo-tree-hist-directory)))

(provide 'no-littering)
