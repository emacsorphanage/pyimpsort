<a href="https://github.com/emacsorphanage/pyimpsort"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## pyimpsort.el
*Sort Python imports*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![MELPA](http://melpa.org/packages/pyimpsort-badge.svg)](http://melpa.org/#/pyimpsort)
![PyPI - Version](https://img.shields.io/pypi/v/pyimpsort)
![Build Status](https://github.com/emacsorphanage/pyimpsort/actions/workflows/tests.yml/badge.svg)

`pyimpsort.el` sort the python imports of a file.
Currently uses [pyimpsort.py](pyimpsort.py) to process the way to sort python
imports.

### Setup


Add the following snippet to your `init.el`:

    (require 'pyimpsort)

To run `pyimpsort-buffer` manually, you can bind it to a key in `python-mode`:

    (eval-after-load 'python
      '(define-key python-mode-map (kbd "C-c C-u") #'pyimpsort-buffer))

To run `pyimpsort-buffer` automatically before saving a Python file:

    (add-hook 'python-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'pyimpsort-buffer nil t)))

### Configuration


By default, `pyimpsort.el` looks for the `pyimpsort.py` script in the same
directory as this file and constructs a shell command using the `python3`
executable found in the system or in the configured virtual environment
(see `python-shell-virtualenv-root`).

Alternatively, you can install `pyimpsort` via PyPI and configure the command
as explained above.

These variables can be customized:

- `pyimpsort-script`
  Absolute path to the `pyimpsort.py` script.  By default, it is resolved
  relative to the location of this file.  You can override it like so:

      (setq pyimpsort-script "/my/custom/path/to/pyimpsort.py")

- `pyimpsort-command`
  Full shell command used to call the script.  By default, it launches the
  `pyimpsort-script` using the Python interpreter configured in Emacs
  (`python-shell-interpreter`).  Useful when calling the script inside a
  container or custom environment.  Example:

      (setq pyimpsort-command
            "docker exec -i my-container python3 -m pyimpsort")

- `pyimpsort-group-module-import`
  If non-nil, group multiple imports from the same module into a single
  statement. This adds the `--group` option to the command line.

      (setq pyimpsort-group-module-import t)

- `pyimpsort-group-platform-site`
  If non-nil, group platform-level site-packages (e.g. system-wide
  installations) separately from other third-party imports. These are
  placed before third-party group. This adds the `--site` option to the
  command line.

      (setq pyimpsort-group-platform-site t)

- `pyimpsort-local-import`
  A list of module names to arbitrarily place in the "local" import group.
  If left nil, the local module will be inferred by walking up directories
  containing `__init__.py` and returning the top-level package name.

      (setq pyimpsort-local-import '("myproject"))

You can also configure this per project using `.dir-locals.el`:

   ((python-mode
     . ((pyimpsort-command . "docker exec -i my-container python3 -m pyimpsort")
        (pyimpsort-group-module-import . t)
        (pyimpsort-group-platform-site . t)
        (setq pyimpsort-local-import . ("myproject")))))

### Troubleshooting


+ **Doesn't sort correctly third party libraries**

  `pyimpsort.el` tries to identify the third party libraries if are installed
  in in the PYTHONPATH, if a package is not installed it is assumed that
  belongs to the application.
  `pyimpsort.el` also tries to identify if a python virtualenv
  is activated.

### Related projects


+ [isort][] ([emacs integration](https://github.com/paetzke/py-isort.el))

[isort]: https://github.com/timothycrosley/isort

### ChangeLog


Version 0.1.1 (2025-07-10)

- Minor fixes

Version 0.1.0 (2025-07-10)

- Added: `pyimpsort-local-import` — force specific modules to be treated as
  local
- Added: `pyimpsort-group-platform-site` — separate platform site-packages
  from other third-party imports
- Added: `pyimpsort-group-module-import` — group multiple imports from the
  same module into a single statement
- Fixed: improved detection of multi-line import blocks
- Fixed: preserve comments adjacent to import statements when sorting



### Customization Documentation

#### `pyimpsort-display-error-buffer`

Display error buffer on error.

#### `pyimpsort-error-buffer-name`

Buffer name of pyimpsort error.

#### `pyimpsort-script`

Path to the `pyimpsort.py` script.

This script is used by default when `pyimpsort-command` is not customized.
If relative, it is resolved with respect to the location of the Emacs Lisp file.

#### `pyimpsort-command`

Shell command used to launch `pyimpsort`.

If you override this value, `pyimpsort-script` will be ignored.
Use this if you run Python in a non-standard environment, such as a container:

    docker exec -i my-dev-container sudo -u my-dev-user python3 -m pyimpsort

#### `pyimpsort-group-module-import`

Group multiple imports from the same module into a single statement.

If non-nil, consecutive imports from the same module will be merged.
For example, instead of:
    from os import path
    from os import remove

You get:
    from os import path, remove

#### `pyimpsort-group-platform-site`

Group platform site-packages separately from other third-party imports.

When non-nil, modules found in the platform's site-packages directory
(e.g., system-level installations) are grouped separately from other
third-party modules such as those installed in the user base.

This group is placed before other third-party imports.

#### `pyimpsort-local-import`

List of module names to treat as local imports.

Modules listed here are considered local to the project and will be grouped
accordingly, rather than as third-party or system imports.

If set to nil, the local module name is inferred by traversing the directory
tree upwards from the current file, stopping at the highest-level directory
that still contains an '__init__.py' file.  The name of that top-level directory
is used as the local module.

### Function and Macro Documentation

#### `(pyimpsort-region BEGIN END)`

Sort python imports from region BEGIN to END points.

#### `(pyimpsort-buffer)`

Sort python imports from current buffer.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
