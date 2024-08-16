# local-config - Automatically find and load local Emacs RC files
![License](https://img.shields.io/github/license/jamescherti/local-config.el)

This `local-config` Emacs package facilitates the search and loading of local configuration files (`.local-config.el`) within the directory of the buffer or its parent directories.

Features:
- Automatic Configuration Discovery: Searches for and loads `.local-config.el` file from the
  directory of the current buffer and its parent directories up to the root.
- Selective Directory Loading: Restricts the loading of configuration files to directories listed in the variable `local-config-allowed-directories` and `local-config-denied-directories`, ensuring control over where configuration files are sourced from.
- The `local-config-mode` mode: Automatically loads the `.local-config.el` file whenever a file is opened, leveraging the `find-file-hook` to ensure that local configurations are applied.

## Installation

### Install using straight

To install the `local-config` using `straight.el`:

1. If you haven't already done so, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.

2. Add the following code to your Emacs init file:
``` emacs-lisp
(use-package local-config
  :ensure t
  :straight (local-config
             :type git
             :host github
             :repo "jamescherti/local-config.el")
  :custom
  (local-config-verbose t)
  (local-config-filename ".local-config.el")
  (local-config-allowed-directories '("~/src" "~/projects"))
  (local-config-denied-directories '("~/src/excluded_dir"))
  :config
  (local-config-mode))
```

## Usage

Assuming that the `local-config-dir` package has been configured to allow loading configuration files from specific directories, such as `~/src`, by setting the `local-config-allowed-directories` variable:
``` emacs-lisp
(setq local-config-allowed-directories '("~/src" "~/projects"))
```

Adding the following code to the `~/src/my_python_project/.local-config.el` file can modify the `PYTHONPATH` environment variable for Python buffers within its directory or one of its subdirectories (e.g., `~/src/my_python_project/my_python_project/file.py`). Modifying `PYTHONPATH` ensures that processes executed by tools like Flycheck or Flymake have access to the Python project's modules:
``` emacs-lisp
;;; .local-config.el --- Local Emacs RC -*- no-byte-compile: t; lexical-binding: t; -*-

(when (or (derived-mode-p 'python-ts-mode) (derived-mode-p 'python-mode))
  (let ((python-path (getenv "PYTHONPATH"))
        (local-config-dir (local-config-get-dir)))
    (when local-config-dir
      (setenv "PYTHONPATH"
              (concat local-config-dir (when python-path
                                           (concat ":" python-path)))))))
```

It is recommended to always begin your `.local-config.el` files with the following header:
```
;;; .local-config.el --- Local Emacs RC -*- no-byte-compile: t; lexical-binding: t; -*-
```

The `local-config-dir` package allows for automatic application of specific configurations based on the directory of the files being accessed, enhancing the flexibility and customization of the Emacs environment.

## License

Copyright (C) 2023-2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [local-config.el @GitHub](https://github.com/jamescherti/local-config.el)
