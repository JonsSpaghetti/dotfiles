;;; private.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Doe
;;
;; Author: Jon Tan <https://github.com/jonsspaghetti>
;; Maintainer: Jon Tan <jon08192@gmail.com>
;; Created: March 30, 2021
;; Modified: March 30, 2021
;; Version: 0.0.1
;; Keywords: private functions
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'private)

(defun search-thing-at-point-in-project ()
  "Search for specific symbol under pointer in project using ivy."
  (interactive)
  (+vertico/project-search nil (thing-at-point 'symbol 'no-properties)))


(defun wsl-copy (start end)
  "WSL copy to clipboard from terminal"
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

(defun jmt/run-python-test()
  "Run docker-compose python test using django-admin based on file name."
  (interactive)
  (async-shell-command
   (concat "docker compose -f $CODE_DIR/ll/devops2/compose.yaml exec www python manage.py test " (jmt/get-path-after-www) "." (jmt/django-test-node-for-manage-test))))

(defun jmt/django-test-node-for-manage-test ()
  "Get the test node so we can run docker compose exec manage.py test"
  (replace-regexp-in-string "::" "." (python-pytest--node-id-def-or-class-at-point)))

(defun jmt/get-path-after-www ()
  "Get the file path after 'www/' with slashes replaced by dots, without .py extension."
  (interactive)
  (let* ((file-path (buffer-file-name)))
    (unless file-path
      (error "Buffer is not visiting a file"))

    (if (string-match "www/\\(.*\\)\\.py$" file-path)
        (match-string 1 file-path)
      (error"Could not find 'www/' in file path or file is not a .py file"))))

(defun jmt/python-pytest-run-def-or-class-at-point-dwim (file func args)
  "Run pytest on FILE using FUNC at point as the node-id.

If `python-pytest--test-file-p' returns t for FILE (i.e. the file
is a test file), then this function results in the same behavior
as calling `python-pytest-run-def-at-point'. If
`python-pytest--test-file-p' returns nil for FILE (i.e. the
current file is not a test file), then this function will try to
find related test files and test defs (i.e. sensible match) for
the current file and the def at point.

Additional ARGS are passed along to pytest.
With a prefix argument, allow editing."
  (interactive
   (list
    (jmt/get-path-after-www)
    (python-pytest--node-id-def-or-class-at-point)
    (transient-args 'python-pytest-dispatch)))
  (unless (python-pytest--test-file-p file)
    (setq
     file (python-pytest--sensible-test-file file)
     func (python-pytest--make-test-name func))
    (unless python-pytest-strict-test-name-matching
      (let ((k-option (-first (-partial #'s-prefix-p "-k") args)))
        (when k-option
          ;; try to use the existing ‘-k’ option in a sensible way
          (setq args (-remove-item k-option args)
                k-option (-->
                          k-option
                          (s-chop-prefix "-k" it)
                          (s-trim it)
                          (if (s-contains-p " " it) (format "(%s)" it) it))))
        (setq args (-snoc
                    args
                    (python-pytest--shell-quote file)
                    (if k-option
                        (format "-k %s and %s" func k-option)
                      (format "-k %s" func)))
              file nil
              func nil))))
  (python-pytest--run
   :args args
   :file file
   :node-id func
   :edit current-prefix-arg))


(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (dired-replace-in-string (expand-file-name "Documents/code/" (getenv "HOME")) "" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun arrayify (separator surround)
  "Converts the current region lines to a single line, CSV value, separated by the provided separator string."
  (interactive "sEnter separator character: \nsEnter surround character: ")
  (setq current-region-string (buffer-substring-no-properties (region-beginning) (region-end)))
  (insert
   (mapconcat (lambda (str) (if (> (length str) 0) (concat surround str surround)))
              (split-string current-region-string "\n")
              separator)))

(defun pretty-arrayify (separator surround)
  "Converts the current region lines to a single line, CSV value, separated by the provided separator string."
  (interactive "sEnter separator character: \nsEnter surround character: ")
  (setq current-region-string (buffer-substring-no-properties (region-beginning) (region-end)))
  (insert
   (mapconcat (lambda (str) (if (> (length str) 0) (concat surround str surround "\n")))
              (split-string current-region-string "\n")
              separator)))

(defun unarrayify (separator)
  "Converts the current region line, as a csv string, to a set of independent lines, splitting the string based on the provided separator."
  (interactive "sEnter separator character: ")
  (setq current-region-string (buffer-substring-no-properties (region-beginning) (region-end)))
  (insert
   (mapconcat 'identity
              (split-string current-region-string separator)
              "\n")))

;; TODO finish this up
(defun python-import-path ()
  "Get import path of thing at point"
  (interactive)
  (insert (concat (substring (string-replace "/" "." buffer-file-name) 0 -3) (thing-at-point 'symbol 'properties))))

(defun today ()
  "Insert string for today's date formatted how you like it"
  (interactive)
  (insert (format-time-string " %F")))

(defun now ()
  "Insert string for today's datetime formatted how you like it"
  (interactive)
  (insert (format-time-string "%FT%H:%M:%S %Z")))

(defun black-on-current-file ()
  "Run black on current file"
  (interactive)
  (save-buffer)
  (setenv "PATH" (concat (getenv "PATH") ":" "/Users/jonathan/Documents/code/venvs/dapi/bin/black"))
  ;; (shell-command (concat "echo $PATH"))
  (shell-command (concat "/Users/jonathan/Documents/code/venvs/dapi/bin/black" " " buffer-file-name))
  (revert-buffer-quick))

(defun source-conf-test ()
  "Source conf/test.env"
  (interactive)
  (save-buffer)
  (shell-command (concat "source" " " (projectile-project-root) "/conf/test.env")))

(defun copy-file-name-to-clipboard ()
  "Copy file name to clipboard"
  (interactive)
  (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(defun jon-scratch-cmd ()
  "Do whatever I want"
  (interactive)
  (projectile-root-top-down buffer-file-name))

(defun keep-csv-column (column)
  "Remove all CSV columns except the specified COLUMN."
  (interactive "nKeep column: ")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
        (setq columns (split-string line ","))
        (setq new-line (nth column columns))
        (delete-region (point-at-bol) (point-at-eol))
        (insert new-line)
        (forward-line)))))



;;; private.el ends here
