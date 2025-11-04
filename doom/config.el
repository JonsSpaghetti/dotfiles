;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jon Tan"
      user-mail-address "jon08192@gmail.com")

(message "CONFIG.EL LOADING...")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config))
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Prefer our keys over Doom’s defaults (after general is around)
(after! general
  (general-auto-unbind-keys))

(setq user-full-name "Jon Tan"
      user-mail-address "jon08192@gmail.com")

(message "CONFIG.EL LOADING...")

;; Theme / treemacs integration
(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config))
(setq doom-theme 'doom-one)

;; Only enable tree-sitter if available in your setup
(after! tree-sitter
  (when (fboundp 'global-tree-sitter-mode)
    (global-tree-sitter-mode))
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq parinfer-rust-library "~/.emacs.d/parinfer-rust/libparinfer_rust.dylib")

(setq org-directory "~/Documents/code/org-lambda/")
(setq org-files
      '(("todo" . "todo.org")
        ("journal" . "journal.org")
        ("work-log" . "work-log.org")
        ("notes" . "notes.org")
        ("meetings" . "meetings.org")
        ("one-on-one" . "one-on-one.org")))

(defun jmt/org-refile-candidates ()
  (directory-files org-directory t ".*\\.org$"))

(defun org-file (name)
  (concat (file-name-as-directory org-directory) (cdr (assoc name org-files))))

(setq org-id-link-to-org-use-id t)

(defun jmt/org-refile-with-link (&optional arg)
  "Refile the current heading and leave behind a link to its new location."
  (interactive "P")
  (org-back-to-heading)
  (let* ((refile-marker (point-marker))
         (source-link (or (org-store-link nil)
                          (user-error "Could not create Org link for this heading"))))
    (org-insert-heading)
    (insert source-link)
    (goto-char refile-marker)
    (org-refile arg)))

(after! org
  (message "ORG STUFF LOADING...")
  ;; Use our dynamic list of files as targets
  (add-to-list 'org-refile-targets '(jmt/org-refile-candidates :maxlevel . 3))

  (setq org-agenda-files
        (mapcar (lambda (n) (org-file n))
                '("todo" "journal" "work-log" "notes" "meetings" "one-on-one")))

  (setq org-capture-templates
        `(("t" "Todo"     entry (file+headline ,(org-file "todo") "Inbox")
           "* TODO [#A] %?\n:Created: %U\n" :empty-lines 0)
          ("j" "Journal"  entry (file+datetree ,(org-file "journal"))
           "* %?" :empty-lines 1)
          ("n" "Note"     entry (file+headline ,(org-file "notes") "Random Notes")
           "** %?" :empty-lines 0)
          ("m" "Meeting"  entry (file+datetree ,(org-file "meetings"))
           "* %u %? :meeting:%^g \nCreated: %U\nSCHEDULED: %T\n** Attendees\n*** \n** Notes\n** Action Items\n"
           :tree-type week :clock-in t :clock-resume t :empty-lines 0)
          ("o" "One on One" entry (file ,(org-file "one-on-one"))
           "* %u 1:1 \nCreated: %U\n** Notes\n*** Working on\n%?"
           :empty-lines 0
           :refile-targets ((,(org-file "one-on-one") . (:level . 1))))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "SCHEDULED(c)" "PLANNING(p)"
           "DELEGATED(e)" "IN-PROGRESS(s@/!)" "BLOCKED(b@)"
           "|" "DONE(d!)" "WONT-DO(k@/!)")))

  ;; Note: "DONE" (not "Done")
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "GoldenRod" :weight bold))
          ("NEXT" . (:foreground "DarkOrange" :weight bold))
          ("PLANNING" . (:foreground "MediumPurple" :weight bold))
          ("DELEGATED" . (:foreground "DeepPink" :weight bold))
          ("SCHEDULED" . (:foreground "RoyalBlue1" :weight bold))
          ("IN-PROGRESS" . (:foreground "cyan" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
          ("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  (setq org-tag-faces
        '(("planning"  . (:foreground "MediumPurple1" :weight bold))
          ("backend"   . (:foreground "RoyalBlue1"    :weight bold))
          ("frontend"  . (:foreground "forest green"  :weight bold))
          ("meeting"   . (:foreground "yellow1"       :weight bold))
          ("CRITICAL"  . (:foreground "red1"          :weight bold))))

  (map! :localleader
        :prefix ("r" "refile")
        :map org-mode-map
        :desc "refile copy" "c" #'org-refile-copy
        :desc "refile"      "r" #'org-refile
        :desc "refile link" "l" #'jmt/org-refile-with-link))

(after! org-agenda
  (message "ORG AGENDA STUFF LOADING...")
  (setq org-super-agenda-groups
        '((:name "Today"        :scheduled today)
          (:name "WIP")
          (:name "Up Next"      :todo ("NEXT"))
          (:name "Past Deadline"
           :and (:deadline past :todo ("TODO" "NEXT" "PLANNING" "DELEGATED" "SCHEDULED" "IN-PROGRESS")))
          (:name "Important"    :priority> "B")
          (:name "Meetings"     :tag ("meeting"))
          (:name "Scheduled"    :scheduled t :deadline t)
          (:name "Workflow"     :tag ("emacs"))))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (setq org-agenda-start-day nil
        org-agenda-span 'day
        org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-super-agenda-mode 1))

;; This must be a function symbol or lambda:
(add-hook 'org-agenda-mode-hook #'display-line-numbers-mode)

(after! evil-org-agenda
  (message "EVIL ORG AGENDA STUFF LOADING...")
  (map! :map org-agenda-mode-map "<escape>" #'org-agenda-redo)
  (map! :localleader
        :prefix ("v""view")
        :map org-agenda-mode-map
        :desc "day view" "d" #'org-agenda-day-view))

;; Line numbers: Doom handles this per-mode; global is okay, but do it after evil.
(setq display-line-numbers-type 'visual)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

;; Load private bits only if present to avoid hard errors
(let ((p (expand-file-name "private.el" doom-user-dir)))
  (when (file-exists-p p)
    (load! p)))

(setq evil-snipe-override-evil-repeat-keys nil
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; Python venv on project name (fix: correct hook + lambda)
(add-hook 'python-mode-hook
          (defun jmt/py-auto-workon ()
            (when (and (fboundp 'projectile-project-name)
                       (fboundp 'pyvenv-workon))
              (pyvenv-workon (projectile-project-name)))))

(after! evil
  ;; keybindings that touch evil maps must run after evil loads
  (define-key evil-normal-state-map (kbd "s") #'evil-substitute)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t)
  (setq evil-escape-key-sequence "jk"))

(setq projectile-project-search-path '("~/code/" "~/Documents/code/")
      python-remove-cwd-from-path nil)

(global-so-long-mode 1)

(after! evil-snipe
  (evil-snipe-mode -1))

(map! :localleader :desc "under cursor" "f w" #'search-thing-at-point-in-project)
(map! :leader      :desc "under cursor" "f w" #'search-thing-at-point-in-project)

(map! :localleader :desc "find in project" "f \"" #'+vertico/project-search)
(map! :leader      :desc "find in project" "f \"" #'+vertico/project-search)

(map! :localleader :desc "copy line location" "c l n" #'copy-current-line-position-to-clipboard)

(map! :localleader      :desc "open definition in other window" "g d w" #'xref-find-definitions-other-window)

(map! :leader :prefix ("r" "rename")
      :desc "rename symbol" "n" #'lsp-rename)

(map! :leader :prefix ("1")
      :desc "Show file name" "g" (cmd! (message "%s" (or buffer-file-name (buffer-name)))))

(set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :slant 'italic :height 120)
(with-eval-after-load 'org-faces
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(setq orderless-matching-styles '(orderless-regexp orderless-flex))

;; Only call Dirvish if installed
(after! dirvish
  (dirvish-override-dired-mode))

(add-to-list 'completion-styles 'initials t)

(provide 'config)
