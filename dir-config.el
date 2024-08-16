;;; dir-config.el --- Automatically load directory configuration Elisp file -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2024  James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/dir-config.el
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
;; This `dir-config' Emacs package facilitates the search and loading of local
;; directory configuration files (`.dir-config.el`) within the directory of the
;; buffer or its parent directories.
;;
;; Features:
;; - Automatic Configuration Discovery: Searches for and loads
;;   '.dir-config.el' file from the directory of the current buffer or
;;   its parent directories.
;; - Selective Directory Loading: Restricts the loading of configuration files
;;   to directories listed in the variable `dir-config-allowed-directories'
;;   and `dir-config-denied-directories', ensuring control over where
;;   configuration files are sourced from.
;; - The `dir-config-mode' mode: Automatically loads the '.dir-config.el' file
;;   whenever a file is opened, leveraging the `find-file-hook' to ensure that
;;   the '.dir-config.el' configuration are applied to the buffer.

;;; Code:

(require 'dired)

(defgroup dir-config nil
  "Non-nil if dir-config mode mode is enabled."
  :group 'dir-config
  :prefix "dir-config-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/dir-config.el"))

(defcustom dir-config-file-names '(".dir-config.el")
  "List of filenames for the directory configuration files.

This list contains filenames that Emacs will search for in the directory
hierarchy of the current buffer. Emacs will look for these files starting
from the buffer's directory and moving upward through its parent directories.

Each entry in this list should be a string representing a filename. The
first existing file found in the hierarchy will be used for configuration.

For example, if the list contains the .dir-config.el and
.project-config.el files, Emacs will search for the .dir-config.el file
first, and if it is not found, it will then search for the .project-config.el
file'."
  :type '(repeat string)
  :group 'dir-config)

(defcustom dir-config-verbose nil
  "Enable verbose mode to log when a dir config file is loaded or ignored."
  :type 'boolean
  :group 'dir-config)

(defcustom dir-config-debug nil
  "Enable debug mode to log when a dir config file is loaded or ignored."
  :type 'boolean
  :group 'dir-config)

(defcustom dir-config-allowed-directories '()
  "List of directory names where dir config files are allowed."
  :type '(repeat directory)
  :group 'dir-config)

(defcustom dir-config-denied-directories '()
  "List of directory names where dir config files are denied."
  :type '(repeat directory)
  :group 'dir-config)

;; Internal variables
(defvar dir-config--loaded nil)
(defvar dir-config--allowed-p nil)
(defvar dir-config--dir nil)
(defvar dir-config--file nil)

(defun dir-config--directory-allowed-p (file-list allowed-directories)
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

(defun dir-config-get-dir ()
  "Return the directory of the currently loaded dir config file.
Return `nil` if the dir config file has not been loaded."
  (when (bound-and-true-p dir-config--dir)
    dir-config--dir))

(defun dir-config-get-file ()
  "Return the file of the currently loaded dir config file.
Return `nil` if the dir config file has not been loaded."
  (when (bound-and-true-p dir-config--file)
    dir-config--file))

(defun dir-config-status ()
  "Check if the dir config file have been loaded for the current buffer."
  (interactive)
  (if (and (bound-and-true-p dir-config--dir)
           (bound-and-true-p dir-config--loaded))
      (message "[dir-config] Loaded: %s" dir-config--file)
    (message "[dir-config] Not loaded")))

(defun dir-config--buffer-cwd ()
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

(defun dir-config--find-dominating-file (file-names start-dir)
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

(defun dir-config-edit ()
  "Open the settings file that was loaded, if available."
  (interactive)
  (let ((dir-config-file (dir-config-get-file)))
    (if dir-config-file
        (find-file dir-config-file)
      (message "[dir-config] The dir config file was not found."))))

(defun dir-config-load ()
  "Load the dir config file for CURRENT-FILE from the closest parent directory.
Only loads settings if the directory is allowed and not denied."
  (setq-local dir-config--loaded nil)
  (setq-local dir-config--allowed-p nil)
  (setq-local dir-config--dir nil)
  (setq-local dir-config--file nil)
  (unless (bound-and-true-p dir-config-disable)
    (let* ((current-dir (dir-config--buffer-cwd))
           (dir-config-file
            (dir-config--find-dominating-file dir-config-file-names
                                                current-dir)))
      (unless current-dir
        (error "[dir-config] Failed to read the current working directory"))
      (if dir-config-file
          (let* ((dir-config-dir (file-name-directory dir-config-file))
                 (allowed-dir-p (dir-config--directory-allowed-p
                                 (list current-dir dir-config-file)
                                 dir-config-allowed-directories))
                 (denied-dir-p (dir-config--directory-allowed-p
                                (list current-dir dir-config-file)
                                dir-config-denied-directories)))
            (setq-local dir-config--allowed-p (and allowed-dir-p
                                                     (not denied-dir-p)))
            (setq-local dir-config--dir dir-config-dir)
            (setq-local dir-config--file dir-config-file)
            (if dir-config--allowed-p
                (progn
                  (load dir-config-file nil t nil)
                  (setq-local dir-config--loaded t)
                  (when dir-config-verbose
                    (message "[dir-config] Load: %s" dir-config-file)))
              (when dir-config-debug
                (message "[dir-config] Not allowed: %s" dir-config-file))))
        (when dir-config-debug
          (message (concat "[dir-config] None of the dir config "
                           "files %s were found in '%s' "
                           "or one of its parents")
                   dir-config-file-names
                   current-dir))))))

;;;###autoload
(define-minor-mode dir-config-mode
  "Toggle `dir-config-mode'.
When enabled, `dir-config-mode' loads directory-specific settings
automatically."
  :global t
  :lighter " LocCfg"
  :group 'dir-config
  (if dir-config-mode
      (add-hook 'find-file-hook #'dir-config-load)
    (remove-hook 'find-file-hook #'dir-config-load)))

(provide 'dir-config)
;;; dir-config.el ends here
