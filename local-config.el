;;; local-config.el --- Automatically load local Elisp config file -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2024  James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/local-config.el
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
;; This `local-config' Emacs package facilitates the search and loading of local
;; configuration files (`.emacs-local-config.el`) within the directory of the
;; buffer or its parent directories.
;;
;; Features:
;; - Automatic Configuration Discovery: Searches for and loads
;;   '.emacs-local-config.el' file from the directory of the current buffer or
;;   its parent directories.
;; - Selective Directory Loading: Restricts the loading of configuration files
;;   to directories listed in the variable `local-config-allowed-directories'
;;   and `local-config-denied-directories', ensuring control over where
;;   configuration files are sourced from.
;; - The `local-config-mode' mode: Automatically loads the
;;   '.emacs-local-config.el' file whenever a file is opened, leveraging the
;;   `find-file-hook' to ensure that local configurations are applied.

;;; Code:

(require 'dired)

(defgroup local-config nil
  "Non-nil if local-config mode mode is enabled."
  :group 'local-config
  :prefix "local-config-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/local-config.el"))

(defcustom local-config-file-names '(".emacs-local-config.el")
  "List of filenames for local Emacs configuration files.

This list contains filenames that Emacs will search for in the directory
hierarchy of the current buffer. Emacs will look for these files starting
from the buffer's directory and moving upward through its parent directories.

Each entry in this list should be a string representing a filename. The
first existing file found in the hierarchy will be used for configuration.

For example, if the list contains the .emacs-local-config.el and
.project-config.el files, Emacs will search for the .emacs-local-config.el file
first, and if it is not found, it will then search for the .project-config.el
file'."
  :type '(repeat string)
  :group 'local-config)

(defcustom local-config-verbose nil
  "Enable verbose mode to log when a local config file is loaded or ignored."
  :type 'boolean
  :group 'local-config)

(defcustom local-config-debug nil
  "Enable debug mode to log when a local config file is loaded or ignored."
  :type 'boolean
  :group 'local-config)

(defcustom local-config-allowed-directories '()
  "List of directory names where local config files are allowed."
  :type '(repeat directory)
  :group 'local-config)

(defcustom local-config-denied-directories '()
  "List of directory names where local config files are denied."
  :type '(repeat directory)
  :group 'local-config)

;; Internal variables
(defvar local-config--loaded nil)
(defvar local-config--allowed-p nil)
(defvar local-config--dir nil)
(defvar local-config--file nil)

(defun local-config--directory-allowed-p (file-list allowed-directories)
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

(defun local-config-get-dir ()
  "Return the directory of the currently loaded local config file.
Return `nil` if the local config file has not been loaded."
  (when (bound-and-true-p local-config--dir)
    local-config--dir))

(defun local-config-get-file ()
  "Return the file of the currently loaded local config file.
Return `nil` if the local config file has not been loaded."
  (when (bound-and-true-p local-config--file)
    local-config--file))

(defun local-config-status ()
  "Check if local config file have been loaded for the current buffer."
  (interactive)
  (if (and (bound-and-true-p local-config--dir)
           (bound-and-true-p local-config--loaded))
      (message "[local-config] Loaded: %s" local-config--file)
    (message "[local-config] Not loaded")))

(defun local-config--buffer-cwd ()
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

(defun local-config--find-dominating-file (file-names start-dir)
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

(defun local-config-edit ()
  "Open the settings file that was loaded, if available."
  (interactive)
  (let ((local-config-file (local-config-get-file)))
    (if local-config-file
        (find-file local-config-file)
      (message "[local-config] The local Emacs config file was not found."))))

(defun local-config-load ()
  "Load local config file for CURRENT-FILE from the closest parent directory.
Only loads settings if the directory is allowed and not denied."
  (setq-local local-config--loaded nil)
  (setq-local local-config--allowed-p nil)
  (setq-local local-config--dir nil)
  (setq-local local-config--file nil)
  (unless (bound-and-true-p local-config-disable)
    (let* ((current-dir (local-config--buffer-cwd))
           (local-config-file
            (local-config--find-dominating-file local-config-file-names
                                                current-dir)))
      (unless current-dir
        (error "[local-config] Failed to read the current working directory"))
      (if local-config-file
          (let* ((local-config-dir (file-name-directory local-config-file))
                 (allowed-dir-p (local-config--directory-allowed-p
                                 (list current-dir local-config-file)
                                 local-config-allowed-directories))
                 (denied-dir-p (local-config--directory-allowed-p
                                (list current-dir local-config-file)
                                local-config-denied-directories)))
            (setq-local local-config--allowed-p (and allowed-dir-p
                                                     (not denied-dir-p)))
            (setq-local local-config--dir local-config-dir)
            (setq-local local-config--file local-config-file)
            (if local-config--allowed-p
                (progn
                  (load local-config-file nil t nil)
                  (setq-local local-config--loaded t)
                  (when local-config-verbose
                    (message "[local-config] Load: %s" local-config-file)))
              (when local-config-debug
                (message "[local-config] Not allowed: %s" local-config-file))))
        (when local-config-debug
          (message (concat "[local-config] None of the local config "
                           "files %s were found in '%s' "
                           "or one of its parents")
                   local-config-file-names
                   current-dir))))))

;;;###autoload
(define-minor-mode local-config-mode
  "Toggle `local-config-mode'.
When enabled, `local-config-mode' loads directory-specific settings
automatically."
  :global t
  :lighter " LocCfg"
  :group 'local-config
  (if local-config-mode
      (add-hook 'find-file-hook #'local-config-load)
    (remove-hook 'find-file-hook #'local-config-load)))

(provide 'local-config)
;;; local-config.el ends here
