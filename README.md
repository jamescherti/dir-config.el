# dir-config.el - Automatically find and evaluate .dir-config.el Elisp files (Flexible dir-locals alternative)
[![MELPA](https://melpa.org/packages/dir-config-badge.svg)](https://melpa.org/#/dir-config)
![Build Status](https://github.com/jamescherti/dir-config.el/actions/workflows/ci.yml/badge.svg)
![](https://raw.githubusercontent.com/jamescherti/dir-config.el/main/.images/made-for-gnu-emacs.svg)

The `dir-config` Emacs package automatically loads and evaluates Elisp code from a `.dir-config.el` file found in the buffer's current directory or its closest parent directory. This facilitates adjusting settings or executing functions specific to the directory structure of each buffer.

For instance, you can use the `dir-config` package to:
- **Configure project-specific settings**: Automatically set up environment variables, keybindings, or modes unique to each project.
- **Apply directory-specific customizations**: Set specific behaviors or preferences for files in different directories, such as enabling or disabling certain minor modes based on security considerations. For example, you might disable linters that execute code in directories where you handle untrusted code.
- **Manage multiple environments**: Switch between different coding environments or workflows by loading environment-specific configurations.

Features:
- Automatic Configuration Discovery: Searches for and loads `.dir-config.el` file from the directory of the current buffer or its parent directories.
- Selective Directory Loading: Restricts the loading of configuration files to directories listed in the variable `dir-config-allowed-directories`, ensuring control over where configuration files are sourced from.
- The `dir-config-mode` mode: Automatically loads the `.dir-config.el` file whenever a file or directory is opened, leveraging the `find-file-hook` to ensure that the dir configurations are applied.
- The `.dir-config.el` file name can be changed by modifying the `dir-config-file-names` defcustom.

## Installation

### Install with straight

To install `dir-config` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install `dir-config` from MELPA:
``` emacs-lisp
(use-package dir-config
  :ensure t
  :custom
  (dir-config-file-names '(".dir-config.el"))
  (dir-config-allowed-directories '("~/src" "~/projects"))
  :config
  (dir-config-mode))
```

Note:
- The dir-config file names can be customized by modifying the dir-config-file-names variable. For instance: ```(setq dir-config-file-names '(".project-config.el" ".dir-config.el"))``` will make `dir-config` search for the `.project-config.el` file first, and if it is not found, it will then search for the `.dir-config.el` file'.
- You can set `(setq dir-config-verbose t)` and `(setq dir-config-debug t)` to increase the verbosity of messages each time a file is loaded while `dir-config-mode` is active.

## Example usage

Assuming that the `dir-config` package has been configured to allow loading configuration files from specific directories, such as `~/src`, by setting the `dir-config-allowed-directories` variable:
``` emacs-lisp
(setq dir-config-allowed-directories '("~/src" "~/projects"))
```

Adding the following code to the `~/src/my_python_project/.dir-config.el` file can modify the `PYTHONPATH` environment variable for Python buffers within its directory or one of its subdirectories (e.g., `~/src/my_python_project/my_python_project/file.py`). Modifying `PYTHONPATH` ensures that processes executed by tools like Flycheck or Flymake have access to the Python project's modules:
``` emacs-lisp
;;; .dir-config.el --- Directory config -*- no-byte-compile: t; lexical-binding: t; -*-
(when (or (derived-mode-p 'python-ts-mode) (derived-mode-p 'python-mode))
  (let ((python-path (getenv "PYTHONPATH"))
        (dir (dir-config-get-dir)))
    ;; Update the PYTHONPATH environment variable to ensure that Flycheck,
    ;; Flymake, and other subprocesses can locate the Python modules.
    (when (and dir (or (file-exists-p (expand-file-name "setup.py" dir))
                       (file-exists-p (expand-file-name "pyproject.toml" dir))))
      (setq-local process-environment (copy-sequence process-environment))
      (setenv "PYTHONPATH" (concat dir (when python-path (concat ":" python-path)))))))
```

It is recommended to always begin your `.dir-config.el` files with the following header:
```
;;; .dir-config.el --- Directory config -*- no-byte-compile: t; lexical-binding: t; -*-
```

The `dir-config` package allows for automatic application of specific configurations based on the directory of the files being accessed, enhancing the flexibility and customization of the Emacs environment.

## Frequently Asked Questions

### How does .dir-config.el files compare to .dir-locals.el?

Here is the difference between with `.dir-locals.el` and the `.dir-config.el` files:

- `.dir-locals.el` (built-in):
  - Primarily used for setting per-directory local variables (static).
  - The syntax of `.dir-locals.el` relies heavily on nested lists and alist structures which can quickly become difficult to read and maintain.
  - The configuration in `dir-locals.el` is inherently static unless dynamic behavior is explicitly added using `eval`. Here is an example of `.dir-locals.el`:
    ```
    ((nil . ((eval . (progn
                       (setq-local my-variable t)
                       (message "Hello world"))))))
    ```
  - Only a single `.dir-locals.el` file can be specified by modifying the `dir-locals-file` variable.

- `.dir-config.el` (this package):
  - Loads and evaluates Emacs Lisp code (dynamic by default).
  - `.dir-config.el` files are easier to maintain, as they use standard Elisp code instead of nested alists.
  - Allows specifying multiple `.dir-config.el` file names by adding them to the `dir-config-file-names` list.

### Wouldn't it be better to move .dir-config.el Elisp code into Emacs init?

Dir config files (`.dir-config.el`) offer advantages for managing complex directory-specific configurations that require dynamic logic. They allow projects or directories to define their needs, providing flexibility to customize configurations to specific requirements.

For example, one of the author's use cases is to use `dir-config.el` to enable Emacs features only in trusted directories while keeping these features disabled by default (for security reasons, some code linters need to be disabled by default because they evaluate code).

While this code could be included in the author's init file, he prefers to keep the init file as a general editor configuration without project-specific details. Since the author uses multiple machines for various purposes, he finds it more straightforward to let the filesystem hierarchy (e.g., a Git repository or a project directory) determine the specific settings for each directory or project using `.dir-config.el`.

## License

Copyright (C) 2023-2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [dir-config.el @GitHub](https://github.com/jamescherti/dir-config.el)
- [dir-config.el @MELPA](https://melpa.org/#/dir-config)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
