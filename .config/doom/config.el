;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomás Bizet"
      user-mail-address "tbizetde@gmail.com")

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
(setq doom-font (font-spec :size 18))
;; doom-variable-pitch-font (font-spec :family "sans" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (require 'doom-themes)
;; (load-theme 'doom-dracula t)
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
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

;; remove quit prompt
(setq confirm-kill-emacs nil)

;; default org-mode on buffers
;; (setq-default major-mode 'org-mode)
(setq initial-major-mode 'org-mode)

;; latex export class
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(setq org-latex-listings 't)

;; org-mode +pretty
(setq org-superstar-headline-bullets-list '("◉" "○" "◆" "◇"))
(setq org-superstar-prettify-item-bullets nil)

;; set background for code in org html export
;; (use-package org
;;   :config
;;   (progn
;;     (defun imalison:org-inline-css-hook (exporter)
;;       "Insert custom inline css to automatically set the
;; background of code to whatever theme I'm using's background"
;;       (when (eq exporter 'html)
;;         (let* ((my-pre-bg (face-background 'default))
;;                (my-pre-fg (face-foreground 'default)))
;;           (setq
;;            org-html-head-extra
;;            (concat
;;             org-html-head-extra
;;             (format "<style type=\"text/css\">\n pre.src, pre.src:before {background-color: %s;}</style>\n"
;;                     my-pre-bg))))))

;;     (add-hook 'org-export-before-processing-hook 'imalison:org-inline-css-hook)))

;; dashboard banner
(setq +doom-dashboard-banner-file (expand-file-name "ardinha.png" doom-private-dir))

;; journal config
;; (setq org-journal-dir "~/logs/"
;;       org-journal-date-prefix "#+title: "
;;       org-journal-time-prefix "* "
;;       org-journal-date-format "%a, %Y-%m-%d"
;;       org-journal-file-format "%Y%m%d.org")

;; deft config
;; (setq deft-directory "~/org/"
;;       deft-extensions '("txt", "org"))

;; roam config
(setq org-roam-directory "~/Notes")

;; find ins
;; org
;; (defun find-in-org ()
;;   (interactive)
;;   (counsel-find-file "~/org"))
;; (map! :leader "f o" 'find-in-org)

;; prog
(defun find-in-prog ()
  (interactive)
  (counsel-find-file "~/Prog"))
(map! :leader "f i" 'find-in-prog)

;; .config
(defun find-in-config ()
  (interactive)
  (counsel-find-file "~/.config"))
(map! :leader "f j" 'find-in-config)

(map! "C-c ." 'call-last-kbd-macro)

;; line numbers in text-mode
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; 80 col indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; valign in orgmode
;; (add-hook 'text-mode-hook #'valign-mode)

;; org-mode src block syntax highlight
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; hide markup chars in org-mode
(setq org-hide-emphasis-markers t)

;; close scheduled when done
(setq org-log-done 'time)

;; turn off syntax checking backend on lsp
;; (setq lsp-diagnostics-provider :none)

;; rls workaround
(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-diagnostics-enable nil)
(setq lsp-rust-analyzer-cargo-watch-enable nil)

;; ;; org agenda
;; (setq org-agenda-files (file-expand-wildcards "~/roam/daily/*.org"))

;; org agenda today's deadlines
;; (defun org-agenda-skip-deadline-if-not-today ()
;; "If this function returns nil, the current match should not be skipped.
;; Otherwise, the function must return a position from where the search
;; should be continued."
;;   (ignore-errors
;;     (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;;           (deadline-day
;;             (time-to-days
;;               (org-time-string-to-time
;;                 (org-entry-get nil "DEADLINE"))))
;;           (now (time-to-days (current-time))))
;;        (and deadline-day
;;             (not (= deadline-day now))
;;             subtree-end))))

;; (add-to-list 'org-agenda-custom-commands
;;              '("b" agenda "Today's Deadlines"
;;                ((org-agenda-span 'day)
;;                 (org-agenda-skip-function '(org-agenda-skip-deadline-if-not-today))
;;                 (org-agenda-entry-types '(:deadline))
;;                 (org-agenda-overriding-header "Today's Deadlines "))))

;; (setq treemacs-position 'left
;;       treemacs-width 27)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (require 'centered-window-mode)
(centered-window-mode)
(setq cwm-centered-window-width 95)


;; no first indent inside <script>
(setq! web-mode-script-padding nil)

;; set initial window size
;; (setq initial-frame-alist '((top . 1) (left . 1) (width . 143) (height . 55)))

;; vue volar
(use-package! lsp-volar)

;; org-mode custom highlights
(defun my/org-mode-keywords ()
  "Add custom font-lock keywords for org-mode."
  (font-lock-add-keywords nil
                          `(( "!!!" 0 '(:foreground ,(doom-color 'error) :background ,(doom-color 'base3) :weight bold) t)))
  (font-lock-add-keywords nil
                          `(( "\\(\\?\\?\\?\\)" 0 '(:foreground ,(doom-color 'orange) :background ,(doom-color 'base3) :weight bold) t)))
  )

(add-hook 'org-mode-hook #'my/org-mode-keywords)

;; evil inside mini buffer
(setq evil-want-minibuffer t)
