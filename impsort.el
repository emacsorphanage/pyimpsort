;;; impsort.el --- Sort python imports. -*- lexical-binding: t -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/impsort.el
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

;;; Commentary:
;; [![Travis build status](https://travis-ci.org/emacs-pe/impsort.el.png?branch=master)](https://travis-ci.org/emacs-pe/impsort.el)
;;
;; `impsort.el' sort the python imports of a file.
;; Currently uses [impsort.py](impsort.py) to process the way to sort python
;; imports.
;;
;;; Setup
;; Add the following snippet to your `init.el':
;;
;;     (require 'impsort)
;;     (eval-after-load 'python
;;       '(define-key python-mode-map "\C-c\C-u" #'impsort-buffer))
;;
;;; Troubleshooting:
;; + **Doesn't sort correcly third party libraries**
;;
;;   `impsort.el' tries to identify the third party libraries if are installed
;;   in in the PYTHONPATH, if a package is not installed it is assumed that
;;   belongs to the application.
;;   `impsort.el' also tries to identify if a python virtualenv
;;   is activated.
;;
;;; Related projects:
;; + [isort][] ([emacs integration](https://github.com/paetzke/py-isort.el))
;;
;; [isort]: https://github.com/timothycrosley/isort

;;; Code:

(require 'python)

(defgroup impsort nil
  "Sort python imports."
  :prefix "impsort-"
  :group 'applications)

(defconst impsort-script
  (expand-file-name "impsort.py"
                    (file-name-directory (if load-in-progress
                                             load-file-name
                                           (buffer-file-name))))
  "Absolute path of python impsort.py script.")

(defvar impsort-error-buffer-name "*impsort-error*")

(defconst impsort-imports-start-regexp
  (rx (group (and bol (or "import" "from")))))

(defconst impsort-imports-end-regexp
  (rx (or (and bol (or "import" "from")) (and bol (* space) eol))))

(defun impsort--search-beg-point (&optional end)
  "Search the first import line until reach the END point."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward impsort-imports-start-regexp end t)
         (match-beginning 1))))

(defun impsort--search-end-point (begin)
  "Search the last import line starting from BEGIN point."
  (let (end)
    (save-excursion
      (goto-char begin)
      (goto-char (point-at-bol))
      (catch 'eof
        (while (re-search-forward impsort-imports-end-regexp (point-at-eol) t)
          (when (eobp)
            (throw 'eof "End of file."))
          (setq end (point-at-eol))
          (forward-line 1))))
    end))

;;;###autoload
(defun impsort-region (begin end)
  "Sort python imports from region BEGIN to END points."
  (interactive "r")
  (atomic-change-group
    (let* ((exec-path (python-shell-calculate-exec-path))
           (python-executable (executable-find python-shell-interpreter))
           (command (format "%s %s" python-executable impsort-script)))
      (shell-command-on-region begin end command nil 'replace impsort-error-buffer-name t))))

;;;###autoload
(defun impsort-buffer ()
  "Sort python imports from current buffer."
  (interactive)
  (let* ((begin (impsort--search-beg-point))
         (end (and begin (impsort--search-end-point begin))))
    (when (and begin end)
      (impsort-region begin end))))

(provide 'impsort)

;;; impsort.el ends here
