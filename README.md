# dir-config.el - Automatically find and load the dir config Elisp code
![Build Status](https://github.com/jamescherti/dir-config.el/actions/workflows/ci.yml/badge.svg)
![](https://raw.githubusercontent.com/jamescherti/dir-config.el/main/.images/made-for-gnu-emacs.svg)

The `dir-config` Emacs package automatically loads and evaluates Elisp code from a `.dir-config.el` file found in the buffer's current directory or its closest parent directory. This enables Emacs to adjust settings or execute functions specific to the directory structure of each buffer.

For instance, you can use the `dir-config` package to:
- **Configure project-specific settings**: Automatically set up environment variables, keybindings, or modes unique to each project.
- **Apply directory-specific customizations**: Set specific behaviors or preferences for files in different directories, such as enabling or disabling certain minor modes based on security considerations. For example, you might disable linters that execute code in directories where you handle untrusted code.
- **Manage multiple environments**: Switch between different coding environments or workflows by loading environment-specific configurations.

Features:
- Automatic Configuration Discovery: Searches for and loads `.dir-config.el` file from the directory of the current buffer or its parent directories.
- Selective Directory Loading: Restricts the loading of configuration files to directories listed in the variable `dir-config-allowed-directories`, ensuring control over where configuration files are sourced from.
- The `global-dir-config-mode` mode: Automatically loads the `.dir-config.el` file whenever a file or directory is opened, leveraging the `find-file-hook` to ensure that the dir configurations are applied.
- The `.dir-config.el` file name can be changed by modifying the `dir-config-file-names` defcustom.

## Installation

### Install using straight

To install the `dir-config` using `straight.el`:

1. If you haven't already done so, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.

2. Add the following code to your Emacs init file:
``` emacs-lisp
(use-package dir-config
  :ensure t
  :straight (dir-config
             :type git
             :host github
             :repo "jamescherti/dir-config.el")
  :custom
  (dir-config-file-names '(".dir-config.el"))
  (dir-config-allowed-directories '("~/src" "~/projects"))
  :config
  (global-dir-config-mode))
```

Note:
- The dir-config file names can be customized by modifying: ```(setq dir-config-file-names '(".project-config.el" ".dir-config.el"))```. With this configuration, Emacs will search for the `.project-config.el` file first, and if it is not found, it will then search for the `.dir-config.el` file'.
- You can set `(setq dir-config-verbose t)` and `(setq dir-config-debug t)` to increase the verbosity of messages each time a file is loaded while `global-dir-config-mode` is active.

## Usage

Assuming that the `dir-config-dir` package has been configured to allow loading configuration files from specific directories, such as `~/src`, by setting the `dir-config-allowed-directories` variable:
``` emacs-lisp
(setq dir-config-allowed-directories '("~/src" "~/projects"))
```

Adding the following code to the `~/src/my_python_project/.dir-config.el` file can modify the `PYTHONPATH` environment variable for Python buffers within its directory or one of its subdirectories (e.g., `~/src/my_python_project/my_python_project/file.py`). Modifying `PYTHONPATH` ensures that processes executed by tools like Flycheck or Flymake have access to the Python project's modules:
``` emacs-lisp
;;; .dir-config.el --- Directory config -*- no-byte-compile: t; lexical-binding: t; -*-
(let ((python_path_env (getenv "PYTHONPATH"))
      (dir (dir-config-get-dir)))
  ;; This ensures that the processes that are executed by Flycheck or
  ;; Flake8, can access the environment variables PYTHONPATH.
  (when dir
    (setq-local process-environment
                (cons (concat "PYTHONPATH="
                              (dir-config-get-dir)
                              (if python_path_env (concat ":" python_path_env) ""))
                      process-environment))))
```

It is recommended to always begin your `.dir-config.el` files with the following header:
```
;;; .dir-config.el --- Directory config -*- no-byte-compile: t; lexical-binding: t; -*-
```

The `dir-config-dir` package allows for automatic application of specific configurations based on the directory of the files being accessed, enhancing the flexibility and customization of the Emacs environment.

## License

Copyright (C) 2023-2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [dir-config.el @GitHub](https://github.com/jamescherti/dir-config.el)
