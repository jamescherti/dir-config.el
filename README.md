# local-emacs-rc - Automatically find and load local Emacs RC files
![License](https://img.shields.io/github/license/jamescherti/local-emacs-rc.el)

This `local-emacs-rc` Emacs package facilitates the search and loading of local configuration files (`.local-emacs-rc.el`) within the directory of the buffer or its parent directories.

Features:
- Automatic Configuration Discovery: Searches for and loads `.local-emacs-rc.el` file from the
  directory of the current buffer and its parent directories up to the root.
- Selective Directory Loading: Restricts the loading of configuration files to directories listed in the variable `local-emacs-rc-allowed-directories` and `local-emacs-rc-denied-directories`, ensuring control over where configuration files are sourced from.
- The `local-emacs-rc-mode` mode: Automatically loads the `.local-emacs-rc.el` file whenever a file is opened, leveraging the `find-file-hook` to ensure that local configurations are applied.

## Installation

### Install using straight

To install the `local-emacs-rc` using `straight.el`:

1. If you haven't already done so, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.

2. [ ] Add the following code to your Emacs init file:
```
(use-package local-emacs-rc
  :ensure t
  :straight (local-emacs-rc
             :type git
             :host github
             :repo "jamescherti/local-emacs-rc.el")
  :custom
  (local-emacs-rc-verbose t)
  (local-emacs-rc-filename ".local-emacs-rc.el")
  (local-emacs-rc-allowed-directories '("~/src" "~/projects"))
  (local-emacs-rc-denied-directories '("~/src/excluded_dir"))
  :config
  (local-emacs-rc-mode))
```

## Usage

Assuming that Emacs has been configured to allow loading configuration files from specific directories, such as `~/src`, by setting the `local-emacs-rc-allowed-directories` variable:
```
(setq local-emacs-rc-allowed-directories '("~/src" "~/projects"))
```

For instance, adding the following code to the `~/src/my_python_project/.local-emacs-rc.el` file can modify the `PYTHONPATH` environment variable for Python buffers within that directory and its subdirectories (e.g., `~/src/my_python_project/my_python_project/file.py`). Modifying `PYTHONPATH` ensures that processes executed by tools like Flycheck or Flymake have access to the project's modules:
``` emacs-lisp
(when (or (derived-mode-p 'python-ts-mode) (derived-mode-p 'python-mode))
  (let ((python-path (getenv "PYTHONPATH"))
        (local-emacs-rc-dir (local-emacs-rc-get-dir)))
    (when local-emacs-rc-dir
      (setenv "PYTHONPATH"
              (concat local-emacs-rc-dir (when python-path
                                           (concat ":" python-path)))))))
```

This method allows for automatic application of specific configurations based on the directory of the files being accessed, enhancing the flexibility and customization of the Emacs environment.

## License

Copyright (C) 2023-2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [local-emacs-rc.el @GitHub](https://github.com/jamescherti/local-emacs-rc.el)
