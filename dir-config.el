;;; dir-config.el --- Automatically find and evaluate .dir-config.el (Flexible dir-locals alternative) -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2024  James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.1
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
;; The 'dir-config' Emacs package automatically loads and evaluates Elisp code
;; from a '.dir-config.el' file found in the buffer's current directory or its
;; closest parent directory. This enables Emacs to adjust settings or execute
;; functions specific to the directory structure of each buffer.
;;
;; For instance, you can use the 'dir-config' package to:
;; - Configure project-specific settings: Automatically set up environment
;;   variables, keybindings, or modes unique to each project.
;;
;; - Apply directory-specific customizations: Set specific behaviors or
;;   preferences for files in different directories, such as enabling or
;;   disabling certain minor modes based on security considerations. For
;;   example, you might disable linters that execute code in directories where
;;   you handle untrusted code.
;;
;; - Manage multiple environments: Switch between different coding
;;   environments or workflows by loading environment-specific configurations.
;;
;; Features:
;; - Automatic Configuration Discovery: Searches for and loads '.dir-config.el'
;;   file from the directory of the current buffer or its parent directories.
;;
;; - Selective Directory Loading: Restricts the loading of configuration files
;;   to directories listed in the variable `dir-config-allowed-directories',
;;   ensuring control over where configuration files are sourced from.
;;
;; - The `global-dir-config-mode' mode: Automatically loads the '.dir-config.el'
;;   file whenever a file or directory is opened, leveraging the
;;   `find-file-hook' to ensure that the dir configurations are applied.
;;
;; - The '.dir-config.el' file name can be changed by modifying the
;;   `dir-config-file-names' defcustom.

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
  "List of filenames for directory configuration files.

The `global-dir-config-mode' mode will search for these files in the directory
hierarchy of the current buffer, starting from the buffer's directory and moving
upward through parent directories. The first existing file found will be used
for configuration.

For example, if this list contains .dir-config.el and .project-config.el,
`global-dir-config-mode' will search for the .dir-config.el file first. If not
found, it will then search for the .project-config.el fil."
  :type '(repeat string)
  :group 'dir-config)

(defcustom dir-config-allowed-directories '()
  "List of directory names where dir-config files are allowed.
Both the dir-config file (e.g., '.dir-config.el') and the buffer path must be
under these directories, not just the dir-config file."
  :type '(repeat directory)
  :group 'dir-config)

(defcustom dir-config-verbose nil
  "If non-nil, enable verbose logging for dir-config operations.
When enabled, detailed logs will be produced when a dir-config file (e.g.,
'.dir-config.el') is loaded or ignored. This is useful for tracking the flow of
dir-config loading."
  :type 'boolean
  :group 'dir-config)

(defvar dir-config-debug nil
  "Enable debug mode for dir-config operations if non-nil.
When this option is enabled, detailed debug information will be logged for
various dir-config activities, including:
- Loading of directory config files,
- Cases where directory config files are not found,
- Internal state and processing steps,
- No error handling.
This option is useful for diagnosing and troubleshooting complex issues.")

;; Internal variables
(defvar dir-config--loaded nil)
(defvar dir-config--allowed-p nil)
(defvar dir-config--file nil)

(defun dir-config--message (&rest args)
  "Display a message with '[dir-config]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[dir-config] " (car args)) (cdr args)))

(defun dir-config--warning (&rest args)
  "Display a warning message with '[dir-config] Warning: ' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[dir-config] Warning: " (car args)) (cdr args)))

(defun dir-config--directory-allowed-p (file-list allowed-directories)
  "Check if all files in FILE-LIST are within one of the ALLOWED-DIRECTORIES.
Returns t if all files are within an allowed directory, nil otherwise."
  (seq-some
   (lambda (allowed-dir)
     (let ((expanded-allowed-dir (expand-file-name allowed-dir)))
       (and (seq-every-p
             (lambda (file)
               (file-in-directory-p file expanded-allowed-dir))
             file-list))))
   allowed-directories))

(defun dir-config-get-dir ()
  "Return the directory of the loaded dir-config file, or nil if none."
  (when (bound-and-true-p dir-config--file)
    (file-name-directory dir-config--file)))

(defun dir-config-get-file ()
  "Return the path of the currently loaded dir-config file, or nil if none."
  (when (bound-and-true-p dir-config--file)
    dir-config--file))

(defun dir-config-status ()
  "Report whether the dir-config file has been loaded for the current buffer."
  (interactive)
  (if (and (bound-and-true-p dir-config--file)
           (bound-and-true-p dir-config--loaded))
      (dir-config--message "%s: Loaded: %s" (buffer-name) dir-config--file)
    (dir-config--message "%s: Not loaded" (buffer-name))))

(defun dir-config--buffer-cwd ()
  "Return the directory associated with the current buffer.
Returns:
- The directory path if the buffer is in `dired-mode', or
- The directory of the file if the buffer is visiting a file, or
- nil if neither condition is met."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond ((derived-mode-p 'dired-mode)
           (dired-current-directory))

          (file-name
           (file-name-directory file-name)))))

(defun dir-config--find-dominating-file (file-names start-dir)
  "Locate the first available file from FILE-NAMES in the directory hierarchy.
Searches upward from START-DIR and returns the path to the first found file,
or nil if none is found."
  (when file-names
    (let ((found-file nil))
      (dolist (file-name file-names)
        (let ((file-path
               (locate-dominating-file start-dir
                                       (file-name-nondirectory file-name))))
          (when file-path
            (setq found-file (expand-file-name file-name file-path)))))
      found-file)))

(defun dir-config-load ()
  "Load the dir-config file (e.g., '.dir-config.el') for the current buffer.
The dir-config file is loaded only if the directory is allowed and is sourced
from the closest parent directory of the buffer."
  (if (bound-and-true-p dir-config--loaded)
      ;; Skip it
      (when dir-config-debug
        (dir-config--message "[DEBUG] %s: Skipping load as already loaded: %s"
                             (buffer-name)
                             dir-config--file))
    ;; Load it
    (let ((current-dir (dir-config--buffer-cwd)))
      (if (not current-dir)
          ;; Buffer not supported
          (when dir-config-debug
            (dir-config--message (concat "[DEBUG] Ignored because the "
                                         "buffer '%s' is not supported"
                                         " (major-mode: %s)")
                                 (buffer-name)
                                 major-mode))

        ;; Load it
        (setq-local dir-config--loaded nil)
        (setq-local dir-config--allowed-p nil)
        (setq-local dir-config--file nil)

        (let* ((dir-config-file
                (dir-config--find-dominating-file dir-config-file-names
                                                  current-dir)))
          (if (not dir-config-file)
              (when dir-config-debug
                (dir-config--message (concat
                                      "[DEBUG] %s: None of the dir-config "
                                      "files %s were found for the '%s' "
                                      "buffer (major-mode: %s)")
                                     (buffer-name)
                                     dir-config-file-names
                                     (buffer-name)
                                     major-mode))
            (let ((success nil)
                  (buffer (current-buffer))
                  (allowed-p (dir-config--directory-allowed-p
                              (list current-dir dir-config-file)
                              dir-config-allowed-directories)))
              ;; Allowed?
              (if (not allowed-p)
                  (when dir-config-verbose
                    (dir-config--message "%s: Ignored (not allowed): %s"
                                         (buffer-name)
                                         dir-config-file))
                ;; Set variables
                (setq-local dir-config--allowed-p allowed-p)
                (setq-local dir-config--file dir-config-file)

                ;; Load
                (if dir-config-debug
                    (progn
                      ;; Do not handle errors when debug is activated
                      (load dir-config-file nil t t)
                      (setq success t))
                  ;; Handle errors
                  (condition-case err
                      (progn
                        (load dir-config-file nil t t)
                        (setq success t))
                    (error
                     (dir-config--message "%s: Error loading '%s': %s"
                                          (buffer-name)
                                          dir-config-file
                                          (error-message-string err)))))
                ;; Show message
                (when success
                  (with-current-buffer buffer
                    (setq-local dir-config--loaded t))
                  (when dir-config-verbose
                    (dir-config--message "%s: Load: %s"
                                         (buffer-name)
                                         dir-config-file))))))))))
  dir-config--loaded)

;;;###autoload
(define-minor-mode global-dir-config-mode
  "Toggle `global-dir-config-mode'.
When enabled, `global-dir-config-mode' loads directory-specific settings
automatically."
  :global t
  :lighter " DirCfg"
  :group 'dir-config
  (if global-dir-config-mode
      (add-hook 'find-file-hook #'dir-config-load)
    (remove-hook 'find-file-hook #'dir-config-load)))

(provide 'dir-config)
;;; dir-config.el ends here
