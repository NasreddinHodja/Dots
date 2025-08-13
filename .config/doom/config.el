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
(setq doom-font (font-spec :family "mono" :size 16))
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

;; dashboard banner
(setq +doom-dashboard-banner-file (expand-file-name "banner.png" doom-private-dir))

;; roam config
(setq org-roam-directory "~/Notes")

;; find ins
(defun find-in-prog ()
  (interactive)
  (counsel-find-file "~/Prog"))

(defun find-in-dots ()
  (interactive)
  (counsel-find-file "~/Dots"))

(map! :leader
      (:prefix ("f" . "file")
       (:prefix ("i" . "find in")
        :desc "Find in Prog directory" "p" #'find-in-prog
        :desc "Find in Dots directory" "d" #'find-in-dots)))

;; remap call last macro
(map! "C-c ." 'call-last-kbd-macro)

;; line numbers in text-mode
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; 80 col indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; org-mode src block syntax highlight
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; hide markup chars in org-mode
(setq org-hide-emphasis-markers t)

;; close scheduled when done
(setq org-log-done 'time)

;; rls workaround
(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-diagnostics-enable nil)
(setq lsp-rust-analyzer-cargo-watch-enable nil)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; org-mode custom highlights
(defun my/org-mode-keywords ()
  "Add custom font-lock keywords for org-mode."
  (font-lock-add-keywords nil ;; !!!
                          `(( "!!!" 0 '(:foreground ,(doom-color 'error) :background ,(doom-color 'base3) :weight bold) t)))
  (font-lock-add-keywords nil ;; ???
                          `(( "\\(\\?\\?\\?\\)" 0 '(:foreground ,(doom-color 'orange) :background ,(doom-color 'base3) :weight bold) t)))
  )

(add-hook 'org-mode-hook #'my/org-mode-keywords)

;; remap code format to "SPC c RET"
(map! :leader
      :prefix "c"
      "f" nil)
(map! :leader
      :prefix "c"
      :desc "Format buffer or region"
      "<return>" #'+format/buffer)

;; evil inside mini buffer
(setq evil-want-minibuffer t)

;; org download image dir as "./images"
(setq org-download-image-dir "./images")

;; configure org latex preview
(setq org-preview-latex-process-alist
      '((imagemagick
         :programs ("pdflatex" "convert")
         :description "pdf > png"
         :message "You need to install: pdflatex and imagemagick."
         :image-input-type "pdf"
         :image-output-type "png"
         :image-size-adjust (1.0 . 1.0)
         :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
         :image-converter ("convert -density 300 -trim -antialias %f -quality 100 %O"))))

(setq org-preview-latex-default-process 'imagemagick)

;; use trash
(setq delete-by-moving-to-trash t)
