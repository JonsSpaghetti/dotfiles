;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Ensure that if we conflict, use our keys instead of doom's keys
(general-auto-unbind-keys)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jon Tan"
      user-mail-address "jon08192@gmail.com")

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
(doom-themes-treemacs-config)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(setq doom-theme 'doom-one)

;; This will probably need to change every single time I get a new pc. Needed to compile parinfer-rust and put the build file somewhere
(setq parinfer-rust-library "~/.emacs.d/parinfer-rust/libparinfer_rust.dylib")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/code/org-airbyte/")
;; alist of org files i care about
(defvar org-files
  '(("todo" . "todo.org")
    ("journal" . "journal.org")
    ("work-log" . "work-log.org")
    ("notes" . "notes.org")
    ("meetings" . "meetings.org")))

(defun org-file (name)
  "Get org-file by NAME."
  (concat org-directory (cdr (assoc name org-files))))

(after! org
  (setq org-agenda-files
        `(,(org-file "todo")
          ,(org-file "journal")
          ,(org-file "work-log")
          ,(org-file "notes")
          ,(org-file "meetings")))

  (setq org-capture-templates
        `(("t" "Todo"
           entry (file+headline ,(org-file "todo") "Inbox")
           "* TODO [#B] %?\n:Created: %T\n"
           :empty-lines 0)

          ("j" "Journal"
           entry (file+datetree ,(org-file "journal"))
           "* %?"
           :empty-lines 1)

          ("l" "Work Log Entry"
           entry (file+datetree ,(org-file "work-log"))
           "* %?"
           :empty-lines 0)
          ("n" "Note"
           entry (file+headline ,(org-file "notes") "Random Notes")
           "** %?"
           :empty-lines 0)
          ("m" "Meeting"
           entry (file+datetree ,(org-file "meetings"))
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
           :tree-type week
           :clock-in t
           :clock-resume t
           :empty-lines 0)))

  ;; TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "DELEGATED(e)" "IN-PROGRESS(s@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "WONT-DO(k@/!)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "GoldenRod" :weight bold))
          ("PLANNING" . (:foreground "MediumPurple" :weight bold))
          ("DELEGATED" . (:foreground "DeepPink" :weight bold))
          ("IN-PROGRESS" . (:foreground "CYAN" :weight bold))
          ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("Done" . (:foreground "LimeGreen" :weight bold))
          ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
          ("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  ;; Tag colors
  (setq org-tag-faces
        '(
          ("planning"  . (:foreground "mediumPurple1" :weight bold))
          ("backend"   . (:foreground "royalblue1"    :weight bold))
          ("frontend"  . (:foreground "forest green"  :weight bold))
          ("meeting"   . (:foreground "yellow1"       :weight bold))
          ("CRITICAL"  . (:foreground "red1"          :weight bold)))))




;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Load private functions etc.
(load! "./private.el")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; Allow python mode to automatically activate venvs
;; (add-hook 'python-mode '(pyvenv-activate (concat projectile-project-root ".venv")))
(add-hook 'python-mode '(pyvenv-workon projectile-project-name))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(setq display-line-numbers-type 'relative)
(setq projectile-project-search-path '("~/code/" "~/Documents/code/"))
(setq python-remove-cwd-from-path nil)

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)

(define-key evil-normal-state-map (kbd "s") 'evil-substitute)
(after! evil-snipe
  (evil-snipe-mode -1))

(map! :localleader
      :desc "under cursor"
      "f w" #'search-thing-at-point-in-project)

(map! :leader
      :desc "under cursor"
      "f w" #'search-thing-at-point-in-project)

(map! :localleader
      :desc "find in project"
      "f \"" #'+vertico/project-search)

(map! :leader
      :desc "find in project"
      "f \"" #'+vertico/project-search)

(map! :localleader
      :desc "copy line location"
      "c l n" #'copy-current-line-position-to-clipboard)

;; Map paredit for clojure mode
(map!
 :localleader
 :prefix ("p" . "paredit")
 :map (clojure-mode-map)
 :desc "paredit slurp forward" "s l" #'paredit-forward-slurp-sexp
 :desc "paredit slurp backward" "s h" #'paredit-backward-slurp-sexp
 :desc "paredit barf forward" "b l" #'paredit-forward-barf-sexp
 :desc "paredit barf backward" "b h" #'paredit-backward-barf-sexp)

;; THis gets messed up w/ paredit
;; (map!
;;  :localleader
;;  :prefix ("s" . "surround")
;;  :map (clojure-mode-map)
;;   :desc "paredit wrap round" "(" #'paredit-wrap-round
;;   :desc "paredit wrap square" "[" #'paredit-wrap-square
;;   :desc "paredit wrap curly" "{" #'paredit-wrap-curly)

(map!
 :leader
 :prefix ("r" . "rename")
 :desc "rename symbol" "n" #'lsp-rename)

;; (autoload 'enable-paredit-mode "paredit" "Turn on paredit" t)
;; (eval-after-load 'clojure-mode
;;   '((add-hook 'clojure-mode-hook #'enable-paredit-mode)
;;     (add-hook 'clojurescript-mode-hook #'enable-paredit)))

(map!
 :leader
 :prefix ("1")
 :desc "Show file name" "g" #'(lambda () (interactive) (message buffer-file-name)))

(set-face-attribute 'variable-pitch nil :family "DejaVu Serif")
(set-face-attribute 'variable-pitch nil :slant 'italic)
(set-face-attribute 'variable-pitch nil :height 120)

(with-eval-after-load 'org-faces
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(setq orderless-matching-styles '(orderless-regexp orderless-flex))

(dirvish-override-dired-mode)

;; multi-Vterm
;; (add-hook 'vterm-mode-hook
;; (lambda ()))
                                        ; (define-key vterm-mode-map [return]                      #'vterm-send-return)

;; (setq vterm-keymap-exceptions nil)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
;; (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
;; (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
;; (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
;; (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)

;; LSP java setup
(add-hook 'java-mode-hook #'lsp)
(setq lsp-java-vmargs '("-Xmx2G" "-Xms2G"))
(setq read-process-output-max (* 1024 1024))
;; This works because format on save enabled modes is negated
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode java-mode))  ; elisp's mechanisms are good enough
(setq lsp-java-format-settings-url "~/Downloads/intellij-java-google-style.xml")
(setq lsp-java-format-settings-profile "GoogleStyle")

(provide 'config)
;;; config.el ends here
