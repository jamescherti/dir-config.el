# local-emacs-rc - Automatically load local Emacs RC files
![License](https://img.shields.io/github/license/jamescherti/local-emacs-rc.el)

The `local-emacs-rc' package automates the loading of Emacs configuration files from '.local-emacs-rc.el' files, allowing users to efficiently manage settings for various projects or workspaces.

Features:
- Recursive Search: Finds and loads '.local-emacs-rc.el' files from the current directory or one of its parent directories.
- Interactive Functions: Provides tools to check and navigate to loaded settings.
- Selective Loading: Restricts loading to directories specified in `local-emacs-rc-allowed-directories'.

## Installation

### Install using straight

To install the `local-emacs-rc` using `straight.el`:

1. If you haven't already done so, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.

2. Add the following code to your Emacs init file:
```
(use-package local-emacs-rc
  :ensure t
  :straight (local-emacs-rc
             :type git
             :host github
             :repo "jamescherti/local-emacs-rc.el"))
```

## License

Copyright (C) 2023-2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [local-emacs-rc.el @GitHub](https://github.com/jamescherti/local-emacs-rc.el)
