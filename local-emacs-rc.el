;;; local-emacs-rc.el --- Automatically load local Emacs RC files -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2024  James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/local-emacs-rc.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The `local-emacs-rc' package automates the loading of Emacs configuration
;; files from '.local-emacs-rc.el' files, allowing users to efficiently manage
;; settings for various projects or workspaces.
;;
;; Features:
;; - Recursive Search: Finds and loads '.local-emacs-rc.el' files from the
;;   current directory or one of its parent directories.
;; - Interactive Functions: Provides tools to check and navigate to loaded
;;   settings.
;; - Selective Loading: Restricts loading to directories specified in
;;   `local-emacs-rc-allowed-directories'.

;;; Code:

(defgroup local-emacs-rc nil
  "Non-nil if local-emacs-rc mode mode is enabled."
  :group 'local-emacs-rc
  :prefix "local-emacs-rc-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/local-emacs-rc.el"))

(defcustom local-emacs-rc-filename ".local-emacs-rc.el"
  "Name of the local Emacs RC file."
  :type 'string
  :group 'local-emacs-rc)

(defcustom local-emacs-rc-verbose nil
  "Enable verbose mode to log when a local Emacs RC file is loaded or ignored."
  :type 'boolean
  :group 'local-emacs-rc)

(defcustom local-emacs-rc-allowed-directories '()
  "List of directory names where '.local-emacs-rc.el' is allowed."
  :type '(repeat directory)
  :group 'local-emacs-rc)

(defcustom local-emacs-rc-denied-directories '()
  "List of directory names where '.local-emacs-rc.el' is denied."
  :type '(repeat directory)
  :group 'local-emacs-rc)

(defun local-emacs-rc--directory-allowed-p (file-list allowed-directories)
  "Check if all files in FILE-LIST are in one of the ALLOWED-DIRECTORIES.
Returns t if all files are within one of the allowed directories, nil
otherwise."
  (seq-some
   (lambda (allowed-dir)
     (let ((expanded-allowed-dir (expand-file-name allowed-dir)))
       (and (seq-every-p
             (lambda (file)
               (file-in-directory-p file expanded-allowed-dir))
             file-list))))
   allowed-directories))

(defun local-emacs-rc-get-dir ()
  "Return the directory of the currently loaded `.local-emacs-rc.el` file.
Return `nil` if the local Emacs RC file has not been loaded."
  (when (bound-and-true-p local-emacs-rc--dir)
    local-emacs-rc--dir))

(defun local-emacs-rc-get-file ()
  "Return the file of the currently loaded `.local-emacs-rc.el` file.
Return `nil` if the local Emacs RC file has not been loaded."
  (when (bound-and-true-p local-emacs-rc--file)
    local-emacs-rc--file))

(defun local-emacs-rc-status ()
  "Check if local Emacs RC have been loaded for the current buffer."
  (interactive)
  (if (and (bound-and-true-p local-emacs-rc--dir) local-emacs-rc--loaded)
      (message "[local-emacs-rc] Loaded: %s" local-emacs-rc--file)
    (message "[local-emacs-rc] Not loaded")))

(defun local-emacs-rc--buffer-cwd ()
  "Return the directory of the current buffer."
  (interactive)
  (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           (dired-current-directory))

          (buffer-file-name
           (if buffer-file-name
               (file-name-directory (buffer-file-name (buffer-base-buffer)))
             (expand-file-name default-directory)))

          (t default-directory))))

(defun local-emacs-rc-load ()
  "Load local Emacs RC file for CURRENT-FILE from the closest parent directory.
Only loads settings if the directory is allowed and not denied."
  (kill-local-variable 'local-emacs-rc--dir)
  (kill-local-variable 'local-emacs-rc--file)
  (let* ((current-file (local-emacs-rc--buffer-cwd))
         (dir (locate-dominating-file current-file local-emacs-rc-filename)))
    (unless current-file
      (error "[local-emacs-rc] Failed to read the current working directory"))
    (if dir
        (let* ((settings-dir (expand-file-name dir))
               (settings-file (expand-file-name local-emacs-rc-filename dir))
               (allowed-dir-p (local-emacs-rc--directory-allowed-p
                               (list current-file settings-file)
                               local-emacs-rc-allowed-directories))
               (denied-dir-p (local-emacs-rc--directory-allowed-p
                              (list current-file settings-file)
                              local-emacs-rc-denied-directories)))
          (setq-local local-emacs-rc--loaded nil)
          (setq-local local-emacs-rc--allowed-p (and allowed-dir-p
                                                     (not denied-dir-p)))
          (setq-local local-emacs-rc--dir settings-dir)
          (setq-local local-emacs-rc--file settings-file)
          (if local-emacs-rc--allowed-p
              (progn
                (load settings-file nil t nil)
                (setq-local local-emacs-rc--loaded t)
                (when local-emacs-rc-verbose
                  (message "[local-emacs-rc] Load: %s" settings-file)))
            (when local-emacs-rc-verbose
              (message "[local-emacs-rc] Ignore: %s" settings-file))))
      (message (concat "[local-emacs-rc] The file was not found in the "
                       "current directories or one of its parents: %s")
               local-emacs-rc-filename))))

;;;###autoload
(define-minor-mode local-emacs-rc-mode
  "Toggle `local-emacs-rc-mode'.
When enabled, `local-emacs-rc-mode' loads directory-specific settings automatically."
  :global t
  :lighter " LEmacsRC"
  :group 'local-emacs-rc
  (if local-emacs-rc-mode
      (add-hook 'find-file-hook #'local-emacs-rc-load)
    (remove-hook 'find-file-hook #'local-emacs-rc-load)))

(provide 'local-emacs-rc)
;;; local-emacs-rc.el ends here
