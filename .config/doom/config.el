;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; - `use-package!' for configuring packages
;; - `after!' for reconfiguring packages
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; - `load!' for loading external *.el files relative to this one


;;
;;; * GENERAL ==================================================================
;;

(setq! user-full-name "tomás bizet"
       user-mail-address "tbizetde@gmail.com")

(setq! doom-font (font-spec :family "Source Code Pro" :size 16)
       doom-variable-pitch-font (font-spec :family "sans" :size 16))

(setq! doom-theme 'doom-dracula)

(setq! +doom-dashboard-banner-file (expand-file-name "banner.png" doom-user-dir))

(setq! display-line-numbers-type 'relative)

(defconst nas/frame-parameters
  '((width . 100)
    (height . 45))
  "Default frame parameters.")

(dolist (param nas/frame-parameters)
  (add-to-list 'default-frame-alist param))

;; use trash
(setq! delete-by-moving-to-trash t)

;; remove quit prompt
(setq! confirm-kill-emacs nil)

;; load path for local packages
(add-to-list 'load-path (expand-file-name "local" doom-user-dir))

(setq! projectile-project-search-path '("~/prog/"))

;; evil
(setq! evil-want-minibuffer t
       evil-default-state 'normal)

;; line numbers in text-mode
(add-hook! text-mode #'display-line-numbers-mode)

;; 80 col indicator
(add-hook! prog-mode #'display-fill-column-indicator-mode)

;; TODO hide modeline ignores
(setq! hide-mode-line-excluded-modes '(elemental-mode vterm-mode eshell-mode))

;; workspaces
(defconst nas/workspaces-dir "~/.config/doom/workspaces/")
(defconst nas/workspaces-file "workspaces")

(setq! doom-modeline-persp-name t
       +workspaces-data-file (concat nas/workspaces-dir nas/workspaces-file))

;; TODO figure why this is being reset
(setq! persp-auto-save-opt 0
       persp-save-dir nas/workspaces-dir)

;; lsp hints
(setq lsp-inlay-hint-enable t)

;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(91 . 91))
;; (add-to-list 'default-frame-alist '(alpha . (91 . 91)))
;; (set-frame-parameter nil 'alpha-background 80)
;; (add-to-list 'default-frame-alist '(alpha-background . 80))

;; proced
(map! :leader
      :desc "Proced"
      "o h" #'proced)

;; centered buffer
(perfect-margin-mode 1)

;;
;;; * EMACS BINDINGS ===========================================================
;;

(map! "C-=" #'er/expand-region)


;;
;;; * ORG-MODE =================================================================
;;

;; src blocks results with ansi colors
(require 'ansi-color)
(defun nas/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'org-babel-after-execute-hook 'nas/ansi-colorize-buffer)

;; dirs
(setq! org-directory "~/Notes/"
       org-roam-directory org-directory)

;; default to org-mode on buffers
(setq! initial-major-mode 'org-mode)

;; org-mode src block syntax highlight
(setq! org-src-fontify-natively t
       org-src-tab-acts-natively t
       org-confirm-babel-evaluate nil
       org-edit-src-content-indentation 0)

;; org-mode custom highlights
(defun nas/org-mode-keywords ()
  "Add custom font-lock keywords for org-mode."
  (font-lock-add-keywords  ;; !!!
   nil
   `(("!!!" 0 '(:foreground ,(doom-color 'error) :background ,(doom-color 'base3) :weight bold) t)))
  (font-lock-add-keywords  ;; ???
   nil
   `(("\\(\\?\\?\\?\\)" 0 '(:foreground ,(doom-color 'orange) :background ,(doom-color 'base3) :weight bold) t)))
  )

(add-hook! org-mode #'nas/org-mode-keywords)

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

(setq! org-latex-listings 't)

;; configure org latex preview
(setq! org-preview-latex-process-alist
       '((imagemagick
          :programs ("pdflatex" "convert")
          :description "pdf > png"
          :message "You need to install: pdflatex and imagemagick."
          :image-input-type "pdf"
          :image-output-type "png"
          :image-size-adjust (1.0 . 1.0)
          :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
          :image-converter ("convert -density 300 -trim -antialias %f -quality 100 %O"))))

(setq! org-preview-latex-default-process 'imagemagick)

;; roam capture templates
(defun nas/org-roam-slug (title)
  (replace-regexp-in-string
   "[^[:alnum:]-]+" "-"
   (replace-regexp-in-string "[ \t]+" "-" title)))

(setq! org-roam-capture-templates
       '(("d" "default" plain
          "%?"
          :if-new
          (file+head "%(nas/org-roam-slug \"${title}\").org" "#+title: ${title}\n")
          :unnarrowed t)))


(after! org
  (setq! org-todo-keywords
         '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT (W)" "WAIT TEST(w)" "HOLD(h)" "IDEA(i)"
            "|" "DONE(d)" "KILL(k) CANCELLED(c)")
           (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
           (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))))


;;
;;; * MAPS =====================================================================
;;

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
 ("w" "~/Work/")
 ("h" "~/"))

;; Remap code format to "SPC c RET"
(map! :leader
      :prefix "c"
      "f" nil)
(map! :leader
      :prefix "c"
      :desc "Format buffer or region"
      "<return>" #'+format/buffer)

;; popups
(map! :prefix ("M-p" . "popup")
      :desc "Raise"  "r" #'+popup/raise
      :desc "Close"  "c" #'+popup/close
      :desc "Toggle" "t" #'+popup/toggle
      :desc "Cycle" "n" #'+popup/other
      :desc "Restore" "u" #'+popup/restore
      :desc "Close all" "u" #'+popup/cloase-all
      :desc "Buffer to popup" "RET" #'+popup/buffer)

;; restart & restart daemon
(defun nas/restart ()
  "Restart daemon and reopen main frame by running `remcs'"

  (interactive)

  (call-process "systemd-run" nil 0 nil "--user" "--scope"
                "bash" "-c" "sleep 1 && remcs"))

(map! :leader
      :prefix "q"
      :desc "Restart daemon & main frame" "r" #'nas/restart
      :desc "Restart & restore" "R" #'doom/restart-and-restore)

;;
;;; * ESHELL ===================================================================
;;

;; aliasrc to eshell aliases
(defconst nas/aliasrc-file "~/.config/aliasrc")
(defconst nas/eshell-aliases-file "~/.config/doom/eshell/aliases")

(defun nas/eshell-convert-aliasrc ()
  "Convert multi-line Bash-style aliasrc to cleaned Eshell aliases."

  (when (file-exists-p nas/aliasrc-file)
    (let ((aliases '()))
      (with-temp-buffer
        (insert-file-contents nas/aliasrc-file)

        (goto-char (point-min))
        (while (re-search-forward "#.*$" nil t)
          (replace-match ""))

        (goto-char (point-min))
        (flush-lines "^[ \t]*$")

        (goto-char (point-min))
        (flush-lines "^alias[ \t]*\\\\[ \t]*$")

        (goto-char (point-min))
        (while (re-search-forward "^[ \t]+" nil t)
          (replace-match ""))

        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (string-match "^\\([^=]+\\)=\"\\([^\"]+\\)\"[ \\\]*$" line)
              (push (format "alias %s %s $@*"
                            (match-string 1 line)
                            (match-string 2 line))
                    aliases)))
          (forward-line 1)))

      (unless (file-exists-p (file-name-directory nas/eshell-aliases-file))
        (make-directory (file-name-directory nas/eshell-aliases-file) t))

      (with-temp-file nas/eshell-aliases-file
        (insert (mapconcat 'identity (nreverse aliases) "\n"))))))

(add-hook! eshell-mode #'nas/eshell-convert-aliasrc)

(setq! eshell-directory-name "~/.config/doom/eshell/")


;;
;;; * VTERM ====================================================================
;;

(setq vterm-shell "/bin/bash")
(setenv "SHELL" "/bin/bash")

;; (add-hook 'vterm-mode-hook #'evil-emacs-state)

;; (after! vterm
;;   (defun nas/vterm-send-c ()
;;     "Send c command to zsh vi mode."
;;     (interactive)
;;     (execute-kbd-macro (kbd "i"))
;;     (vterm-send-key (kbd "ESC"))
;;     (vterm-send-key (kbd "c")))

;;   (defun nas/vterm-send-d ()
;;     "Send c command to zsh vi mode."
;;     (interactive)
;;     (execute-kbd-macro (kbd "i"))
;;     (vterm-send-key (kbd "ESC"))
;;     (vterm-send-key (kbd "d")))

;;   (evil-define-key 'normal vterm-mode-map
;;     "c" #'nas/vterm-send-c
;;     "d" #'nas/vterm-send-d)
;;   )


;;
;;; * CLAUDIN ==================================================================
;;

(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))

(setq claudemacs-prefer-projectile-root t)

(global-auto-revert-mode t)

(map! :leader :desc "Claude Code" "M-c" #'claudemacs-transient-menu)


;;
;;; * KANATA =====================================================================
;;

(use-package! kanata-kbd-mode
  :mode ("\\.kbd\\'" . kanata-kbd-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (string-match "/\\.config/kanata/kanata\\.kbd$" buffer-file-name)
              (copy-file buffer-file-name "/sudo::/etc/kanata/kanata-config.kbd" t)
              (message "Kanata config updated!"))))
