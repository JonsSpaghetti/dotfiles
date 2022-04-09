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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-capture-templates
   `(("t" "Todo" entry (file+headline ,(concat org-directory "SOLV/todo.org")
                                     "Inbox")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree ,(concat org-directory "SOLV/journal.org"))
      "* %?\nEntered on %U\n  %i\n  %a")))

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

;; Load private functions etc.
(load! "./private.el")

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
      :desc "ivy proj search"
      "f" #'+ivy/project-search)

(map! :localleader
      :desc "ivy search under cursor"
      "f w" #'search-thing-at-point-in-project)


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

(map!
 :localleader
 :prefix ("s" . "surround")
 :map (clojure-mode-map)
  :desc "paredit wrap round" "(" #'paredit-wrap-round
  :desc "paredit wrap square" "[" #'paredit-wrap-square
  :desc "paredit wrap curly" "{" #'paredit-wrap-curly)

(map!
 :leader
 :prefix ("r" . "rename")
 :desc "rename symbol" "n" #'lsp-rename)

(autoload 'enable-paredit-mode "paredit" "Turn on paredit" t)
(eval-after-load 'clojure-mode
  '((add-hook 'clojure-mode-hook #'enable-paredit-mode)
    (add-hook 'clojurescript-mode-hook #'enable-paredit)))
