;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; - `use-package!' for configuring packages
;; - `after!' for reconfiguring packages
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; - `load!' for loading external *.el files relative to this one


(setq user-full-name "Tomás Bizet"
      user-mail-address "tbizetde@gmail.com")

(setq doom-font (font-spec :family "Source Code Pro" :size 16)
      doom-variable-pitch-font (font-spec :family "sans" :size 16))

(setq doom-theme 'doom-dracula)

(setq +doom-dashboard-banner-file (expand-file-name "banner.png" doom-user-dir))

(setq display-line-numbers-type 'relative)


;;; general
;; initial window parameters
(defconst nas/frame-parameters
  '((width . 100)
    (height . 45))
  "Default frame parameters.")

(dolist (param nas/frame-parameters)
  (add-to-list 'default-frame-alist param))

;; use trash
(setq delete-by-moving-to-trash t)

;; remove quit prompt
(setq confirm-kill-emacs nil)

;;; projects
(setq projectile-project-search-path '("~/Prog/"))

;; evil inside mini buffer
(setq evil-want-minibuffer t)

;; popups
(set-popup-rules!
  '(("*helpful*" :ignore t)
    ("*compilation" :ignore t)
    ("*Help*" :ignore t)))

;; line numbers in text-mode
(add-hook! 'text-mode-hook #'display-line-numbers-mode)

;; 80 col indicator
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

;; workspaces
(setq doom-modeline-persp-name t)


;;; org-mode
;; dirs
(setq org-directory "~/Notes/"
      org-roam-directory org-directory)

;; default org-mode on buffers
(setq initial-major-mode 'org-mode)

;; org-mode src block syntax highlight
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; hide markup chars in org-mode
(setq org-hide-emphasis-markers t)

;; close scheduled when done
(setq org-log-done 'time)

;; org-mode custom highlights
(defun nas/org-mode-keywords ()
  "Add custom font-lock keywords for org-mode."
  (font-lock-add-keywords nil ;; !!!
                          `(( "!!!" 0 '(:foreground ,(doom-color 'error) :background ,(doom-color 'base3) :weight bold) t)))
  (font-lock-add-keywords nil ;; ???
                          `(( "\\(\\?\\?\\?\\)" 0 '(:foreground ,(doom-color 'orange) :background ,(doom-color 'base3) :weight bold) t)))
  )

(add-hook! 'org-mode-hook #'nas/org-mode-keywords)

;; org latex export class
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

;; default create roam node template
(defun nas/org-roam-slug (title)
  "Convert TITLE to a slug with hyphens instead of underscores."
  (replace-regexp-in-string "_" "-" (org-roam-node-slug (org-roam-node-create :title title))))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%(nas/org-roam-slug \"${title}\").org" "#+title: ${title}\n") :unnarrowed t)))


;;; maps
;; find-ins
(defun nas/find-in-dir (dir)
  "Generic function to find files in DIR."

  (interactive)
  (find-file (read-file-name (format "Find file in %s: " dir) dir)))

(defun nas/build-keybinding (key path)
  "Create keybinding specification for KEY and PATH."

  (let ((desc (format "Find in %s" path)))
    (list :desc desc key `(lambda () (interactive) (nas/find-in-dir ,path)))))

(defmacro nas/def-find-dirs (&rest key-path-pairs)
  "Create keybindings from (KEY PATH) pairs."

  (let ((keybindings (mapcar (lambda (pair)
                               (nas/build-keybinding (nth 0 pair) (nth 1 pair)))
                             key-path-pairs)))
    `(progn (map! :leader
                  (:prefix ("f" . "file")
                           (:prefix ("i" . "find in")
                                    ,@keybindings))))))

(map! :leader
      (:prefix ("f" . "file")
               "i" nil))

(nas/def-find-dirs
 ("d" "~/Dots/")
 ("b" "~/.local/bin/")
 ("p" "~/Prog/")
 ("f" "~/Facu/")
 ("t" "~/Trab/")
 ("h" "~/"))

;; remap code format to "SPC c RET"
(map! :leader
      :prefix "c"
      "f" nil)
(map! :leader
      :prefix "c"
      :desc "Format buffer or region"
      "<return>" #'+format/buffer)


;;; eshell
;; aliasrc to eshell aliases
(defconst nas/aliasrc-file "~/.config/aliasrc")
(defconst nas/eshell-aliases-file "~/.config/doom/eshell/aliases")

(defun nas/eshell-convert-aliasrc ()
  "Convert multi-line Bash-style aliasrc to cleaned Eshell aliases."

  (when (file-exists-p nas/aliasrc-file)
    (let ((aliases '()))
      (with-temp-buffer
        (insert-file-contents nas/aliasrc-file)

        ;; Remove comments
        (goto-char (point-min))
        (while (re-search-forward "#.*$" nil t)
          (replace-match ""))

        ;; Delete empty lines
        (goto-char (point-min))
        (flush-lines "^[ \t]*$")

        ;; Delete lines that are just 'alias \'
        (goto-char (point-min))
        (flush-lines "^alias[ \t]*\\\\[ \t]*$")

        ;; Remove indentation
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]+" nil t)
          (replace-match ""))

        ;; Collect alias pairs
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (string-match "^\\([^=]+\\)=\"\\([^\"]+\\)\"[ \\\]*$" line)
              (push (format "alias %s %s"
                            (match-string 1 line)
                            (match-string 2 line))
                    aliases)))
          (forward-line 1)))

      (unless (file-exists-p (file-name-directory nas/eshell-aliases-file))
        (make-directory (file-name-directory nas/eshell-aliases-file) t))

      ;; Write aliases to file
      (with-temp-file nas/eshell-aliases-file
        (insert (mapconcat 'identity (nreverse aliases) "\n"))))))

(add-hook! eshell-first-time-mode-hook #'nas/eshell-convert-aliasrc)
