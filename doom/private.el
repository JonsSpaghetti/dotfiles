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

(defun run-python-test()
  "Run docker-compose python test using django-admin based on file name."
  (interactive)
  (async-shell-command
   (concat "docker-compose exec -T hippo django-admin test -k "
           (concat
            (mapconcat 'identity
                       (butlast
                        (split-string
                         (s-replace "/" "."
                                    (python-pytest--relative-file-name buffer-file-name))
                         "\\."))
                       ".")
            (concat "."
                    (python-pytest--current-defun))))))
                         
                 
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


;;; private.el ends here
