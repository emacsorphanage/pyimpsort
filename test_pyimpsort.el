;; test_pyimpsort.el --- Tests for pyimpsort.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains automated tests for the `pyimpsort.el` package.
;; It uses ERT (Emacs Lisp Regression Testing) to verify import sorting
;; behavior on various Python files.

;;; Code:

(require 'ert)
(require 'pyimpsort)

(defmacro pyimpsort--define-test (name input expected &optional bindings)
  "Define an ERT for `pyimpsort-buffer'.

NAME is the symbol used as the test's name.
INPUT and EXPECTED are file paths.  This test loads the INPUT file into a
temporary buffer, sets the buffer to `python-mode', applies `pyimpsort-buffer',
and then compares the resulting buffer content against the content of the
EXPECTED file.
Optional BINDINGS is an alist of variable settings active during the test."
  `(ert-deftest ,name ()
     (let ,bindings
       (with-temp-buffer
         (insert-file-contents ,input)
         (python-mode)
         (pyimpsort-buffer)
         (let ((actual (buffer-substring-no-properties (point-min) (point-max)))
               (expected-content (with-temp-buffer
                                   (insert-file-contents ,expected)
                                   (buffer-substring-no-properties (point-min) (point-max)))))
           (should (equal actual expected-content)))))))

(pyimpsort--define-test
 pyimpsort-test-1
 "samples/el/1.in"
 "samples/el/1.out")

(pyimpsort--define-test
 pyimpsort-test-2
 "samples/el/2.in"
 "samples/el/2.out"
 ((pyimpsort-group-module-import t))
 )

(provide 'test_pyimpsort)

;;; test_pyimpsort.el ends here
