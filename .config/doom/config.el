;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Tomás Bizet"
      user-mail-address "tbizetde@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 16)
      doom-variable-pitch-font (font-spec :family "sans" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; evil inside mini buffer
(setq evil-want-minibuffer t)

;; initial window size
(defconst my/frame-parameters
  '((width . 100)
    (height . 45))
  "Default frame parameters.")

;; Set for all frames (both initial and client)
(dolist (param my/frame-parameters)
  (add-to-list 'default-frame-alist param))

;; remove quit prompt
(setq confirm-kill-emacs nil)

;; default org-mode on buffers
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

;; dashboard banner
(setq +doom-dashboard-banner-file (expand-file-name "banner.png" doom-user-dir))

;; roam config
(setq org-roam-directory "~/Notes")


;; find-ins
(defun find-in-dir (dir)
  "Generic function to find files in DIR."

  (interactive)
  (find-file (read-file-name (format "Find file in %s: " dir) dir)))

(defun build-keybinding (key path)
  "Create keybinding specification for KEY and PATH."

  (let ((desc (format "Find in %s" path)))
    (list :desc desc key `(lambda () (interactive) (find-in-dir ,path)))))

(defmacro def-find-dirs (&rest key-path-pairs)
  "Create keybindings from (KEY PATH) pairs."

  (let ((keybindings (mapcar (lambda (pair)
                               (build-keybinding (nth 0 pair) (nth 1 pair)))
                             key-path-pairs)))
    `(progn (map! :leader
                  (:prefix ("f" . "file")
                           (:prefix ("i" . "find in")
                                    ,@keybindings))))))

(map! :leader
      (:prefix ("f" . "file")
               "i" nil))

(def-find-dirs
 ("d" "~/Dots/")
 ("b" "~/.local/bin/")
 ("p" "~/Prog/")
 ("f" "~/Facu/")
 ("t" "~/Trab/")
 ("h" "~/"))


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

;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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

;; workspaces in modeline
(setq doom-modeline-persp-name t)

;; default create roam node template
(defun my-org-roam-slug (title)
  "Convert TITLE to a slug with hyphens instead of underscores."
  (replace-regexp-in-string "_" "-" (org-roam-node-slug (org-roam-node-create :title title))))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%(my-org-roam-slug \"${title}\").org" "#+title: ${title}\n") :unnarrowed t)))

;; projects folder
(setq projectile-project-search-path '("~/Prog/"))
