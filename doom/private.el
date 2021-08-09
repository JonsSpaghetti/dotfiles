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
  (+ivy/project-search nil (thing-at-point 'symbol 'no-properties))
)

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
                                         (python-pytest--relative-file-name buffer-file-name)
                                         ) "\\.")
                             ) ".")
                 (concat "."
                         (python-pytest--current-defun)
                         )
                 )
                )
        )
       )


(defun copy-current-line-position-to-clipboard ()
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
    (interactive)
    (let ((path-with-line-number
           (concat (dired-replace-in-string (getenv "HOME") "~" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))
;;; private.el ends here
