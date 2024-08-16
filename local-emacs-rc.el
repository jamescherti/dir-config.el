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

(defcustom local-emacs-rc-file-names '(".local-emacs-rc.el")
  "List of local Emacs RC file names to search for.
Each entry should be a string representing a filename. The configuration will
search for these files in the directory hierarchy of the current buffer."
  :type '(repeat string)
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

(defun local-emacs-rc--find-dominating-file (file-names start-dir)
  "Locate the first available dominating file from FILE-NAMES.

FILE-NAMES is a list of filenames to search for. This function searches upward
from START-DIR to find the first directory that contains one of the files in
FILE-NAMES. Returns the path to the found file or nil if none is found."
  (when file-names
    (let ((found-file nil))
      (dolist (file-name file-names)
        (let ((file-path (locate-dominating-file start-dir file-name)))
          (when file-path
            (setq found-file (expand-file-name file-name file-path)))))
      found-file)))

(defun local-emacs-rc-edit ()
  "Open the settings file that was loaded, if available."
  (interactive)
  (let ((local-emacs-rc-file (local-emacs-rc-get-file)))
    (if local-emacs-rc-file
        (find-file local-emacs-rc-file)
      (message "The local Emacs RC file was not found."))))

(defun local-emacs-rc-load ()
  "Load local Emacs RC file for CURRENT-FILE from the closest parent directory.
Only loads settings if the directory is allowed and not denied."
  (unless (bound-and-true-p local-emacs-rc-disable)
    (let* ((current-dir (local-emacs-rc--buffer-cwd))
           (local-emacs-rc-file
            (local-emacs-rc--find-dominating-file local-emacs-rc-file-names
                                                  current-dir)))
      (unless current-dir
        (error "[local-emacs-rc] Failed to read the current working directory"))
      (if local-emacs-rc-file
          (let* ((local-emacs-rc-dir (file-name-directory local-emacs-rc-file))
                 (allowed-dir-p (local-emacs-rc--directory-allowed-p
                                 (list current-dir local-emacs-rc-file)
                                 local-emacs-rc-allowed-directories))
                 (denied-dir-p (local-emacs-rc--directory-allowed-p
                                (list current-dir local-emacs-rc-file)
                                local-emacs-rc-denied-directories)))
            (setq-local local-emacs-rc--loaded nil)
            (setq-local local-emacs-rc--allowed-p (and allowed-dir-p
                                                       (not denied-dir-p)))
            (setq-local local-emacs-rc--dir local-emacs-rc-dir)
            (setq-local local-emacs-rc--file local-emacs-rc-file)
            (if local-emacs-rc--allowed-p
                (progn
                  (load local-emacs-rc-file nil t nil)
                  (setq-local local-emacs-rc--loaded t)
                  (when local-emacs-rc-verbose
                    (message "[local-emacs-rc] Load: %s" local-emacs-rc-file)))
              (when local-emacs-rc-verbose
                (message "[local-emacs-rc] Ignore: %s" local-emacs-rc-file))))
        (message (concat "[local-emacs-rc] The file was not found in the "
                         "current directories or one of its parents: %s")
                 current-dir)))))

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
