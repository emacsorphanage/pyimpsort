;;; pyimpsort.el --- Sort python imports. -*- lexical-binding: t -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/pyimpsort.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; The companion script `pyimpsort.py` is distributed under the MIT License.
;; See the header of that file for details.

;;; Commentary:
;;
;; `pyimpsort.el' sort the python imports of a file.
;; Currently uses [pyimpsort.py](pyimpsort.py) to process the way to sort python
;; imports.
;;
;;; Setup:
;;
;; Add the following snippet to your `init.el':
;;
;;     (require 'pyimpsort)
;;
;; To run `pyimpsort-buffer' manually, you can bind it to a key in `python-mode':
;;
;;     (eval-after-load 'python
;;       '(define-key python-mode-map (kbd "C-c C-u") #'pyimpsort-buffer))
;;
;; To run `pyimpsort-buffer' automatically before saving a Python file:
;;
;;     (add-hook 'python-mode-hook
;;               (lambda ()
;;                 (add-hook 'before-save-hook #'pyimpsort-buffer nil t)))
;;
;;; Configuration:
;;
;; By default, `pyimpsort.el' looks for the `pyimpsort.py` script in the same
;; directory as this file, and constructs a shell command to run it using the
;; Python interpreter configured in Emacs (`python-shell-interpreter').
;;
;; These variables can be customized:
;;
;; - `pyimpsort-script'
;;   Absolute path to the `pyimpsort.py` script.  By default, it is resolved
;;   relative to the location of this file.  You can override it like so:
;;
;;       (setq pyimpsort-script "/my/custom/path/to/pyimpsort.py")
;;
;; - `pyimpsort-command'
;;   Full shell command used to call the script.  By default, it launches the
;;   `pyimpsort-script' using the Python interpreter configured in Emacs
;;   (`python-shell-interpreter').  Useful when calling the script inside a
;;   container or custom environment.  Example:
;;
;;       (setq pyimpsort-command
;;             "docker exec -i my-container python3 -m pyimpsort")
;;
;; - `pyimpsort-group-module-import'
;;   If non-nil, group multiple imports from the same module into a single
;;   statement. This adds the `--group` option to the command line.
;;
;;       (setq pyimpsort-group-module-import t)
;;
;; - `pyimpsort-group-platform-site'
;;   If non-nil, group platform-level site-packages (e.g. system-wide
;;   installations) separately from other third-party imports. These are
;;   placed before third-party group. This adds the `--site` option to the
;;   command line.
;;
;;       (setq pyimpsort-group-platform-site t)
;;
;; You can also configure this per project using `.dir-locals.el`:
;;
;;    ((python-mode
;;      . ((pyimpsort-command . "docker exec -i my-container python3 -m pyimpsort")
;;         (pyimpsort-group-module-import . t)
;;         (pyimpsort-group-platform-site . t))))
;;
;;
;;; Troubleshooting:
;;
;; + **Doesn't sort correctly third party libraries**
;;
;;   `pyimpsort.el' tries to identify the third party libraries if are installed
;;   in in the PYTHONPATH, if a package is not installed it is assumed that
;;   belongs to the application.
;;   `pyimpsort.el' also tries to identify if a python virtualenv
;;   is activated.
;;
;;; Related projects:
;;
;; + [isort][] ([emacs integration](https://github.com/paetzke/py-isort.el))
;;
;; [isort]: https://github.com/timothycrosley/isort

;;; Code:

(require 'python)

(defgroup pyimpsort nil
  "Sort python imports."
  :prefix "pyimpsort-"
  :group 'applications)

(defcustom pyimpsort-display-error-buffer nil
  "Display error buffer on error."
  :type 'boolean
  :group 'pyimpsort)

(defcustom pyimpsort-error-buffer-name "*pyimpsort-error*"
  "Buffer name of pyimpsort error."
  :type 'string
  :group 'pyimpsort)

(defcustom pyimpsort-script
  (expand-file-name "pyimpsort.py"
                    (file-name-directory (if load-in-progress
                                             load-file-name
                                           (buffer-file-name))))
  "Absolute path to the Python script `pyimpsort.py`.

This script is used by default when `pyimpsort-command' is not customized."
  :type 'string
  :group 'pyimpsort)

(defcustom pyimpsort-command
  (let* ((exec-path (python-shell-calculate-exec-path)))
    (mapconcat #'shell-quote-argument
               (list (executable-find python-shell-interpreter) pyimpsort-script)
               " "))
  "Shell command used to launch `pyimpsort`.

If you override this value, `pyimpsort-script' will be ignored.
Use this if you run Python in a non-standard environment, such as a container:

    docker exec -i my-dev-container sudo -u my-dev-user python3 -m pyimpsort"
  :type 'string
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-command)

(defcustom pyimpsort-group-module-import nil
  "Group multiple imports from the same module into a single statement.

If non-nil, consecutive imports from the same module will be merged.
For example, instead of:
    from os import path
    from os import remove

You get:
    from os import path, remove"
  :type 'boolean
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-group-module-import)

(defcustom pyimpsort-group-platform-site nil
  "Group platform site-packages separately from other third-party imports.

When non-nil, modules found in the platform's site-packages directory
\(e.g., system-level installations) are grouped separately from other
third-party modules such as those installed in the user base.

This group is placed before other third-party imports."
  :type 'boolean
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-group-platform-site)

(defconst pyimpsort-import-regex
  "^\\(from .* \\)?import[[:blank:]]*\\(([^)]*)\\|[^()\\\\\n]*\\(\\\\\n[^\\\\\n]*\\)*\\)"
  "Regular expression matching a Python import statement.")

(defun pyimpsort--search-import-bounds ()
  "Return the bounds (BEGIN . END) of the top-level contiguous import block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward pyimpsort-import-regex)
      (let* ((start (match-beginning 0))
             (end (match-end 0)))
        (while (progn
                 (goto-char end)
                 (when (looking-at "[[:blank:]\n]*")
                   (goto-char (match-end 0)))
                 (when (looking-at pyimpsort-import-regex)
                   (setq end (match-end 0))
                   t)))
        (cons start (1+ end))))))

;;;###autoload
(defun pyimpsort-region (begin end)
  "Sort python imports from region BEGIN to END points."
  (interactive "r")
  (let ((command pyimpsort-command))
    (when pyimpsort-group-module-import
      (setq command (concat command " --group")))
    (when pyimpsort-group-platform-site
      (setq command (concat command " --site")))
    (atomic-change-group
      (or (zerop (shell-command-on-region begin end command nil 'replace
                                          pyimpsort-error-buffer-name pyimpsort-display-error-buffer))
          (error "Command exited abnormally.  See %s for details" pyimpsort-error-buffer-name)))))

;;;###autoload
(defun pyimpsort-buffer ()
  "Sort python imports from current buffer."
  (interactive)
  (let ((bounds (pyimpsort--search-import-bounds)))
    (when bounds
      (pyimpsort-region (car bounds) (cdr bounds)))))

(provide 'pyimpsort)

;;; pyimpsort.el ends here
