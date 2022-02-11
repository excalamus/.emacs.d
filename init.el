;;; init.el -- Principium de Excalamus

;; Author: Matt Trzcinski <matt@excalamus.com>
;; URL: https://github.com/excalamus/.emacs.d.git
;; Requires: ((emacs "27.1"))

;; Maintain package consistency across multiple devices using
;; straight.el with use-package.el.  Fork packages and point
;; straight.el to personal repos.  The forks help with submitting pull
;; requests and provides another point of consistency.

  ; <-- insert linebreak with 'C-q C-l' (quoted-insert)
    ;     navigate with 'C-x ]' (forward-page) and 'C-x [' (backward-page)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xc/debug nil
  "Toggle debug mode.")

(if xc/debug (toggle-debug-on-error))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make use-package invoke straight.  Must be set before straight is
;; bootstrapped because it affects how straight.el is loaded.
;; Use-package is configured below, after bootstrapping.  See
;; https://github.com/raxod502/straight.el#getting-started
(setq straight-use-package-by-default 't)

;; bootstrap straight.el.  See https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; This configures straight with use-package.  It also ensures that
;; use-package is installed.
;; https://github.com/raxod502/straight.el#integration-with-use-package
(straight-use-package 'use-package)

;; extend use-package with key-chord
(use-package use-package-chords
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))

;; Ensure recipe inheritance. Allows for simple :fork override to
;; clone/pull from personal fork.  M-x straight-fetch-package to
;; update from fork, C-u M-x straight-fetch-package to fetch from
;; upstream
(setq straight-allow-recipe-inheritance t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xc/device
  (cond ((file-directory-p "C:\\") 'windows)
        ((file-directory-p "/home/") 'gnu/linux)
        ((file-directory-p "/data/data/com.termux/") 'termux))
  "Current device.

Either 'windows, 'gnu/linux, or 'termux.

`system-type' doesn't differentiate X from terminal.
`window-system' gets assigned after init loads.")

(defvar xc/on-demand-window nil
  "Target on-demand window.

An on-demand window is one which you wish to return to within the
current Emacs session but whose importance doesn't warrant a
permanent binding.")

(defvar xc/atlassian ""
  "Atlassian url.")

(defvar xc/pyside-modules
  '("QtCore" "Qt3DAnimation" "QtGui" "QtHelp" "QtNetwork" "QtOpenGL" "QtPrintSupport" "QtQml"
    "QtCharts" "QtQuick" "QtDataVisualization" "QtQuickWidgets" "QtTextToSpeech" "QtSql"
    "QtMultimedia" "QtMultimediaWidgets" "QtMacExtras" "QtSvg" "QtUiTools" "QtTest" "QtConcurrent"
    "QtAxContainer" "QtWebEngineCore" "QtWebEngineWidgets" "QtWebChannel" "QtWebSockets" "QtWidgets"
    "QtWinExtras" "QtX11Extras" "QtXml" "QtXmlPatterns" "Qt3DCore" "Qt3DExtras" "Qt3DInput" "Qt3DLogic"
    "Qt3DRender" "QtPositioning" "QtLocation" "QtSensors" "QtScxml")
  "List of Qt modules for use in `xc/pyside-lookup'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prevent custom from writing to init; have it write to a dump file
;; that never gets loaded or read
(setq custom-file "~/.emacs.d/custom-set.el")

;; todo, make interactive
(defun xc/load-directory (dir &optional ext)
  "Load all files in DIR with extension EXT.

Default EXT is \".el\".

See URL `https://www.emacswiki.org/emacs/LoadingLispFiles'"
  (let* ((load-it (lambda (f)
                    (load-file (concat (file-name-as-directory dir) f))))
         (ext (or ext ".el"))
         (ext-reg (concat "\\" ext "$")))
    (mapc load-it (directory-files dir nil ext-reg))))

(if (file-exists-p "~/.emacs.d/lisp/")
    (xc/load-directory "~/.emacs.d/lisp/"))

;; load customizations which aren't versioned here
(cond ((eq xc/device 'windows-nt)
       (add-hook 'after-init-hook (lambda () (load "~/secret-lisp.el"))))
      ((eq xc/device 'gnu/linux)
       (progn
         (add-hook 'after-init-hook (lambda () (load "/home/ahab/.emacs.d/mine/my-lisp.el")))
         (with-eval-after-load "yasnippet"
           (add-to-list 'yas-snippet-dirs "/home/ahab/.emacs.d/mine/" t)))))

(cond ((eq xc/device 'gnu/linux)
       (add-to-list 'Info-directory-list "/home/ahab/Documents/info_manuals-main")))

;; (setq yas-snippet-dirs (list yas-snippet-dirs))

;; InnoSetup .iss files are basically ini files
(add-to-list 'auto-mode-alist '("\\.iss\\'" . conf-mode))

;; configure autosave directory
;; https://stackoverflow.com/a/18330742/5065796
(defvar xc/-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p xc/-backup-directory))
    (make-directory xc/-backup-directory t))
(setq backup-directory-alist `(("." . ,xc/-backup-directory))) ; put backups in current dir and in xc/-backup-directory
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "utf-8")

;; tab insertion. See `xc/before-save-hook'.
(setq-default indent-tabs-mode nil)  ; don't ever insert tabs
;; (dtrt-indent-global-mode)  ; insert tabs based on file

(defun xc/before-save-hook ()
  "Conditionally run whitespace-cleanup before save.

Run whitespace-cleanup on save unless
`xc/disable-whitespace-cleanup' is non-nil.  Set
`xc/disable-whitespace-cleanup' using a directory local variable:

  ;; .dir-locals-2.el
  ((nil . ((xc/disable-whitespace-cleanup . t))))"
  (unless (and (boundp 'xc/disable-whitespace-cleanup)
               xc/disable-whitespace-cleanup)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'xc/before-save-hook)

;; (if (eq xc/device 'gnu/linux)
;;     (setq whitespace-style '(face tabs)))

(setq-default abbrev-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
(put 'narrow-to-region 'disabled nil)  ; enabling disables confirmation prompt
(setq initial-scratch-message nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq initial-major-mode 'emacs-lisp-mode)
(setq help-window-select t)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq show-help-function nil)
(set-default 'truncate-lines t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; don't prompt when calling dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; Change yes-no prompts to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Make occur window open in side-window
(setq
 display-buffer-alist
 '(("\\*Occur\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 0)
    (window-width . fit-window-to-buffer))
   ))

;; split ediff vertically
(setq ediff-split-window-function 'split-window-right)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; restore window configuration on ediff close
;; https://emacs.stackexchange.com/a/17089
(defvar xc/ediff-last-windows nil)

(defun xc/store-pre-ediff-winconfig ()
  (setq xc/ediff-last-windows (current-window-configuration)))

(defun xc/restore-pre-ediff-winconfig ()
  (set-window-configuration xc/ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'xc/store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'xc/restore-pre-ediff-winconfig)

(if (eq xc/device 'termux) (setq browse-url-browser-function 'eww-browse-url))

;; Make *Occur* window size to the contents
(add-hook 'occur-hook
       (lambda ()
         (let ((fit-window-to-buffer-horizontally t))
           (save-selected-window
             (pop-to-buffer "*Occur*")
             (fit-window-to-buffer)))))

;; Make *Occur* window always open on the right side
(setq
 display-buffer-alist
 `(("\\*Occur\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 0)
    (window-width . fit-window-to-buffer))))

;; Automatically switch to *Occur* buffer
(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))

(defun xc/-append-newline-after-comma (x)
  (replace-regexp-in-string "," ",\n" x))

(defun xc/toggle-long-line-filter ()
  "Prevent long lines from bogging down the shell."
  (interactive)
  (if (member 'xc/-append-newline-after-comma comint-preoutput-filter-functions)
      (progn
        ;; (remove-hook 'comint-preoutput-filter-functions 'xc/-append-newline-after-comma t)
        (remove-hook 'comint-preoutput-filter-functions 'xc/-append-newline-after-comma)
        (message "Removed local long line filter"))
    (progn
      ;; (add-hook 'comint-preoutput-filter-functions 'xc/-append-newline-after-comma 90 t)
      (add-hook 'comint-preoutput-filter-functions 'xc/-append-newline-after-comma 90)
      (message "Added local long line filter"))))

(remove-hook 'comint-preoutput-filter-functions 'xc/-append-newline-after-comma)

(setq c-default-style "gnu")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove insanely annoying space underlines in rst and markdown modes
;; See https://emacs.stackexchange.com/a/10546/
(set-face-attribute 'nobreak-space nil :underline 'unspecified :inherit 'unspecified)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (display-time)

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode 1)

;; https://dejavu-fonts.github.io/
;; https://stackoverflow.com/a/296316
;; Values in 1/10pt, so 100 will give you 10pt, etc.
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 100
                        :weight 'normal
                        :width 'normal))

;;; Mode line
(column-number-mode t)
(setq mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c "))) (-3 "%p")))
(which-function-mode)

;; ;; Just a hack, needs proper attention
(setq-default mode-line-format
              '("%e"
                evil-mode-line-tag
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                " "
                mode-line-position
                mode-line-misc-info
                (vc-mode vc-mode)
                " "
                mode-line-end-spaces))

;; Automatically reload files that have changed on disk
(global-auto-revert-mode)

;; Remove Git prefix from vc since only using git
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git" " " vc-mode))))

;; make titlebar the filename
;; https://emacs.stackexchange.com/a/16836
(setq-default frame-title-format '("%f"))

;; Theme advice approach modified from
;; https://www.greghendershott.com/2017/02/emacs-themes.html
(use-package base16-theme
  :after (:all org)
  :straight (:fork "excalamus/base16-emacs"))
(use-package zenburn-theme
  :after (:all org)
  :straight (:fork "excalamus/zenburn-emacs"))

(defun xc/disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar xc/theme-hooks nil
  "((theme-id . function) ...)")

(defun xc/add-theme-hook (theme-id hook-func)
  (add-to-list 'xc/theme-hooks (cons theme-id hook-func)))

(defun xc/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `xc/add-theme-hook'."
  (unless no-enable
    (xc/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id xc/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'xc/load-theme-advice)

(defvar xc/theme-dark nil
  "My dark theme.")

(defvar xc/theme-light nil
  "My light theme.")

(if (eq xc/device 'windows)
    (setq xc/theme-dark 'zenburn)
  (setq xc/theme-dark 'base16-eighties))

(setq xc/theme-light 'base16-tomorrow)

;; Add to hook to reload these automatically
(defun xc/dark-theme-hook ()
  "Run after loading dark theme."
  (cond
   ((eq xc/theme-dark 'zenburn)
         (progn
           (if (eq xc/device 'termux)
               (set-face-attribute 'mode-line-inactive nil :background "color-236"))
           (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
           (setq evil-insert-state-cursor '("gray" bar))
           (set-face-attribute 'hl-line nil :background "gray29" :foreground 'unspecified)
           (set-face-attribute 'mode-line nil :background "gray40")
           (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified)
           (set-face-attribute 'xc/hi-comint nil :background "dim gray")))

   ((eq xc/theme-dark 'base16-eighties)
        (progn
          (set-face-attribute 'hl-line nil :background "gray20" :foreground 'unspecified)

          (set-face-attribute 'mode-line-inactive nil
                              :box '(:line-width 1 :color "gray1" :style released-button)
                              ;; :background "#393939"
                              :background "#2d2d2d")

          (set-face-attribute 'mode-line nil
                              :foreground 'unspecified
                              :background "gray9"
                              :box '(:line-width 1 :color "gray1" :style released-button))

          (with-eval-after-load "iedit-lib"
            (set-face-attribute 'iedit-occurrence nil :background "gray30"))

          ;; odd should be darker
          (with-eval-after-load "ediff-init"
            (set-face-attribute 'ediff-even-diff-A nil :background "gray12" :foreground 'unspecified :inverse-video 'unspecified)
            (set-face-attribute 'ediff-even-diff-B nil :background "gray12" :foreground 'unspecified :inverse-video 'unspecified)
            (set-face-attribute 'ediff-even-diff-C nil :background "gray12" :foreground 'unspecified :inverse-video 'unspecified)

            (set-face-attribute 'ediff-odd-diff-A nil :background "gray10" :foreground 'unspecified :inverse-video 'unspecified)
            (set-face-attribute 'ediff-odd-diff-B nil :background "gray10" :foreground 'unspecified :inverse-video 'unspecified)
            (set-face-attribute 'ediff-odd-diff-C nil :background "gray10" :foreground 'unspecified :inverse-video 'unspecified)
            )

          (with-eval-after-load "bm"
            (set-face-attribute 'bm-face nil :background "RosyBrown2"))

          ))))

(defun xc/light-theme-hook ()
  "Run after loading light theme."
  ;; base16-tomorrow
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (set-face-attribute 'hl-line nil :background "gray96" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "light gray")
  (set-face-attribute 'mode-line-inactive nil :background "white smoke")
  (set-face-attribute 'org-mode-line-clock nil :background "white" :inherit nil)
  (set-face-attribute 'bm-face nil :background "light cyan" :overline 'unspecified :foreground 'unspecified)
  (set-face-attribute 'xc/hi-comint nil :background "light gray"))

;; ;; If using another theme, such as with a different Emacs instance
;; ;; (`xc/emacs-standalone' with tango-dark, set custom with:

;; (set-face-attribute 'hl-line nil :background "gray36" :foreground 'unspecified)
;; (set-face-attribute 'highlight nil :background "orange red" :foreground 'unspecified)

(xc/add-theme-hook xc/theme-dark #'xc/dark-theme-hook)
(xc/add-theme-hook xc/theme-light #'xc/light-theme-hook)

(defvar xc/theme-type nil
  "Type of current theme.")

(setq xc/theme-type 'dark)

(defun xc/theme-toggle (&optional type)
  "Toggle theme to TYPE."
  (interactive)
  (unless type (setq type xc/theme-type))
  (cond ((eq type 'dark)
         (disable-theme xc/theme-light)
         (load-theme xc/theme-dark t nil)
         (setq xc/theme-type 'dark))
        ((eq type 'light)
         (disable-theme xc/theme-dark)
         (load-theme xc/theme-light t nil)
         (setq xc/theme-type 'light))))

(defun xc/theme-switch ()
  "Switch from dark theme to light or vice versa."
  (interactive)
  (cond ((eq xc/theme-type 'light)
         (xc/theme-toggle 'dark))
        ((eq xc/theme-type 'dark)
         (xc/theme-toggle 'light))))

;; theme config depends on ace-window and bm
(with-eval-after-load "ace-window"
  (with-eval-after-load "bm"
    (with-eval-after-load "hi-lock"
      (xc/theme-toggle))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Way more packages use markdown-mode than you might expect.  When
;; put in alphabetical order, one of these other packages builds it
;; first, throwing an error about two build recipes.  See URL
;; `https://github.com/raxod502/straight.el/issues/518'
(use-package markdown-mode
  :after (:all org)
  :straight (:fork "excalamus/markdown-mode")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config

  ;; ;; allow spaces in minibuffer (e.g. completing-read)
  ;; (define-key minibuffer-local-completion-map " " nil)
  ;; (define-key minibuffer-local-must-match-map " " nil)

  (if xc/debug (message "markdown-mode")))


;; ...and same with yasnippet
(use-package yasnippet
  :after (:all org)
  :straight (:fork "excalamus/yasnippet")
  :config
  (yas-global-mode)

  (if xc/debug (message "yasnippet")))


;; ...and something similar with lsp-mode
(use-package lsp-mode
;; requires pip install python-language-server
  :after (:all org pyvenv) ; posframe)
  :straight (:fork "excalamus/lsp-mode")
  :commands lsp
  :config
  (pyvenv-mode 1)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  ;; (remove-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  ;; Open docs in frame instead of minibuffer
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2749
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-render-documentation t) ;
  (setq lsp-signature-doc-lines 5)
  (setq lsp-restart 'auto-restart)
  (setq lsp-completion-enable nil)
  ;; (setq lsp-signature-function 'lsp-signature-posframe))
  (setq lsp-signature-function 'lsp-lv-message))


(use-package ace-window
  :after (:all org)
  :straight (:fork "excalamus/ace-window")
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)

  (if xc/debug (message "ace-window")))


(use-package bm
  :after (:all org)
  :straight (:fork "excalamus/bm")
  :init
  :config
  (setq bm-cycle-all-buffers t)

  (if xc/debug (message "bm")))


(use-package csv-mode
  :after (:all org)
  :config

  (add-hook 'csv-mode-hook
            (lambda ()
              (hl-line-mode)
              (face-remap-add-relative 'hl-line :box '(:color "gray" :line-width 1))))

  (if xc/debug (message "csv-mode")))


(use-package comment-dwim-2
  :after (:all org)
  :straight (:fork "excalamus/comment-dwim-2")
  :config

  (if xc/debug (message "comment-dwim-2")))


(use-package csound-mode
  :after (:all org)
  :straight (:fork "excalamus/csound-mode")
  :config

  (if xc/debug (message "csound-mode")))


(use-package define-word
  :after (:all org)
  :straight (:fork "excalamus/define-word")
  :config

  ;; https://github.com/abo-abo/define-word/issues/31
  (setq url-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0")

  (if xc/debug (message "define-word")))


(use-package dtrt-indent
  :after (:all org)
  :straight (:fork "excalamus/dtrt-indent")
  :config

  ;; (dtrt-indent-global-mode)

  (if xc/debug (message "dtrt-indent")))


(use-package dumb-jump
  :after (:all org)
  :straight (:fork "excalamus/dumb-jump")
  :config

  (if xc/debug (message "dumb-jump")))


(use-package gemini-mode
  :after (:all org)
  :straight (:repo "http://git.carcosa.net/jmcbray/gemini.el.git"))


(use-package general
  :after (:all org)
  :straight (:fork "excalamus/general.el")
  :config
  (require 'ffap)

  ;; https://web.archive.org/web/20210725155836/https://github.com/noctuid/general.el/issues/460

  (defvar xc/plover-enabled nil
    "State of whether Plover is active.")

  (defvar xc/steno-hook nil)

  (defun general--split-on-positional-args (rest)
    "Remove all positional arguments from the list REST.
Return a list of containing the list of positional arguments and the and a list
of the other arguments. The order of arguments will be preserved."
    (let (positional-args)
      (while (general--positional-arg-p (car rest))
        (push (pop rest) positional-args))
      (list positional-args rest)))

  (cl-defmacro general-unbind (&rest args)
    "A wrapper for `general-def' to unbind multiple keys simultaneously.
Insert after all keys in ARGS before passing ARGS to `general-def.' \":with
#'func\" can be optionally specified to use a custom function instead (e.g.
`ignore'). :full t can be used to specify that ARGS contains key/command pairs
instead of just keys. This can be useful to create a command that either binds
or unbinds commands."
    (declare (indent defun))
    ;; Note: :with can be at an odd position, so must handle internally and not
    ;; with &key
    (let* (with
           full
           (split-args (general--remove-keyword-args args))
           (kargs (cl-loop for (key val) on (cadr split-args) by 'cddr
                           if (memq key '(:with :full))
                           do (if (eq key :with)
                                  (setq with val)
                                (setq full val))
                           else
                           collect key
                           and collect val))
           (split-maps (general--split-on-positional-args (car split-args)))
           (maps
            (if full
                (cl-loop for (key _) on (cadr split-maps) by 'cddr
                         collect key
                         and collect (if (eq with t)
                                         nil
                                       with))
              (cl-loop for key in (cadr split-maps)
                       collect key
                       and collect (if (eq with t)
                                       nil
                                     with))))
           (args (append (car split-maps) maps kargs)))
      `(general-def ,@args)))

  (defmacro xc/steno-define (&rest args)
    `(progn
       (general-def ,@args :prefix "→")
       (general-add-hook 'xc/steno-hook
                         (lambda ()
                           (if xc/plover-enabled
                               (progn
                                 (general-unbind ,@args :prefix "SPC" :full t)
                                 (message "Disabled keyboard leader"))
                             (progn
                               (general-def ,@args :prefix "SPC")
                               (message "Enabled keyboard leader")))))))

  (defun xc/toggle-general-keyboard-leader ()
    "Toggle global keyboard leader."
    (interactive)
    (setq xc/plover-enabled (not xc/plover-enabled))
    (run-hooks 'xc/steno-hook))

  (general-after-init

    (general-unbind
      "C-h h"
      "<f3>"
      "<f4>"
      "M-v"
      ;; Disable stupid minimize hotkeys
      "C-z"
      "C-x C-z")

    (general-unbind :keymaps 'shell-mode-map
      "C-c C-w"
      )

    ;; (general-define-key :keymaps 'key-translation-map
    ;;  ;; "M-x" "M-q"  ; dvp
    ;;  ;; "M-q" "M-x"  ; dvp
    ;;  ;; "SPC" "<tab>"
    ;;  ;; "<prior>" "<escape>"
    ;;  )

    (if (eq xc/device 'windows)
        (general-def :keymaps 'override
          :prefix "C-x i"
          "b" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/brag.org"))
          "g" '(lambda () (interactive) (find-file "C:/glossary/glossary.org"))
          "n" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/notes.org"))
          "m" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/monorepo.org"))
          "t" '(lambda () (interactive) (find-file "c:/Users/mtrzcinski/Documents/notes/timecard.org"))
          )
      (general-def :keymaps 'override
        :prefix "C-x i"
        "n" '(lambda () (interactive) (find-file "~/Documents/notes.org"))
        )
      )

    (general-def :keymaps 'override
      :prefix "C-x i"
      "a" '(lambda () (interactive) (find-file "~/.emacs.d/archive/andr-init.el"))
      "i" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "c" '(lambda () (interactive) (find-file "~/.emacs.d/archive/classic-init.el"))
      "e" '(lambda () (interactive) (find-file "~/.emacs.d/experimental.el"))
      "s" '(lambda () (interactive) (find-file "~/secret-lisp.el"))  ; %APPDATA% on Windows
      )

    (general-def :keymaps 'override
      "<vertical-line> <mouse-3>" 'balance-windows  ; right-click on vertical to balance-windows
      "C-x s" 'save-buffer
      "<f5>" '(lambda () (interactive) (progn (funcall 'xc/send-line-or-region)))
      "<f8>" 'xc/switch-to-last-window
      "C-<f8>" '(lambda () (interactive) (peut-gerer-switch-to 'main t 0))
      "S-<f8>" '(lambda () (interactive) (peut-gerer-switch-to 'shell t 0))
      "M-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-select-project))
      "C-S-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-create-shell))
      "C-M-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-activate-project))
      "M-c" 'xc/copy-symbol-at-point
      ;; "M-j" 'xc/helm-imenu ; navigate the file's structure (functions or otherwise)
      "M-j" 'helm-semantic-or-imenu ; navigate the file's structure (functions or otherwise)
      ;; "M-j" 'xc/python-occur-definitions
      "M-y" 'xc/yank-pop-forwards  ; todo but p is not yank... (use C-p for evil-paste-pop)
      "C-M-y" 'helm-show-kill-ring
      "C-M-j" 'helm-swoop  ; swoop (S)pecific thing (at point)
      "C-j" 'helm-swoop-without-pre-input ; enter navigate-state
      "<f2>" 'bm-common-next
      "S-<f2>" 'bm-common-previous
      "C-<f2>" 'bm-toggle

      "<f7>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (if xc/kill-python-p
                     (xc/kill-python))  ; kills aws cli commands
                 ;; (quit-process (get-buffer-process peut-gerer-shell))
                 (peut-gerer-send-command peut-gerer-command))
      "C-<f7>" '(lambda () (interactive) (call-interactively 'peut-gerer-send-command))
      ;; can use to create new *shell* after load
      "<f10>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (if xc/kill-python-p
                     (xc/kill-python))  ; kills aws cli commands
                 ;; (quit-process (get-buffer-process peut-gerer-shell))
                 (peut-gerer-send-command peut-gerer-command))
      "C-<f10>" '(lambda () (interactive) (call-interactively 'peut-gerer-send-command))
      "C-c +" 'evil-numbers/inc-at-pt
      "C-c -" 'evil-numbers/dec-at-pt
      "C-h j" 'describe-face  ; introspect colors
      "C-h R" 'elisp-index-search ; C-h r is 'info-emacs-manual by default
      "C-h C-f" 'find-function
      "C-h C-w" 'define-word-at-point ; masks define-no-warranty
      "C-x a d" 'xc/define-abbrev
      "C-x b" 'helm-buffers-list
      "C-x g" 'magit-status
      "C-x R" 'magit-list-repositories ; C-x r clashes with rectangular edit
      "C-x o" 'ace-window
      "C-c C-w" 'other-window
      "C-c C-p" 'xc/switch-to-last-window
      "M-o" 'other-frame
      "M-v" 'other-window
      "M-V" '(lambda () (interactive) (other-window -1))
      "C-S-v" 'xc/switch-to-last-window
      "C-x n D" 'xc/narrow-to-defun-indirect
      "<f1>" '(lambda ()
                (interactive)
                (if xc/on-demand-window
                    (call-interactively 'xc/on-demand-window-goto)
                  (call-interactively 'ace-window)))
      "S-<f1>" 'xc/on-demand-window-set
      "C-<f1>" 'ace-window
      "C-=" 'iedit-mode
      "<pause>" 'xc/punch-timecard
      "M-l" 'xc/recenter-window-top-bottom
      )

    (general-def :keymaps 'override
      :states '(normal insert emacs)
      (general-chord "jk") 'xc/newline-without-break-of-line
      (general-chord "hh") 'evil-emacs-state
      (general-chord "HH") 'evil-insert-state
      "C-;" 'comment-dwim-2
      "<f9>" 'save-buffer
      "C-<f9>" 'write-file
      "S-<f9>" 'xc/backup-region-or-buffer
      "\M-Q" 'xc/unfill-paragraph
      )

    (general-def :keymaps 'global-map
      "C-a" 'xc/smart-beginning-of-line
      "<home>" 'xc/smart-beginning-of-line
      "C-M-S-t" 'transpose-regions
      )

    (general-def :states '(normal)
      ;; ;; "C-o" 'dumb-jump-back  ; obsoleted
      ;; ;; "C-o" 'xref-pop-marker-stack
      ;; "C-o" 'evil-jump-backward
      ;; "C-i" 'evil-jump-forward

      "gd" 'xref-find-definitions
      "gD" 'xref-find-definitions-other-window
      "gF" 'xref-find-references
      )

    (general-def :states '(insert)
      "C-n" 'dabbrev-expand
      )

    (general-def :states '(normal insert emacs)

      ;; ;; qwerty bindings
      ;; "C-]" 'xref-find-definitions
      ;; "C-}" 'xref-find-definitions-other-window
      ;; "M-]" 'dumb-jump-go
      ;; "M-}" 'dumb-jump-go-other-window

      ;; dvp bindings
      "C-]" 'xref-find-definitions
      "C-6" 'xref-find-definitions-other-window
      "M-]" 'dumb-jump-go
      "M-6" 'dumb-jump-go-other-window

      ;; Ugh, C-i is also TAB
      ;; "M-o" 'evil-jump-backward
      ;; "M-i" 'evil-jump-forward
      "C-o" 'xref-pop-marker-stack
      )

    ;; (general-def :states '(normal visual)
      ;; :prefix "SPC"
   (xc/steno-define '(normal visual)
       ";" 'comment-dwim-2
       "=" 'er/expand-region
       "+" 'er/contract-region
       "b" 'helm-buffers-list
       "f" 'find-file
       "F" 'ffap-other-window
       "g" 'xc/open-file-browser
       "h" 'info
       "i" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
       "k" 'kill-buffer
       "o" 'ace-window
       "r" '(lambda() (interactive)
             (save-some-buffers t nil)
             (if xc/kill-python-p
                 (xc/kill-python))  ; kills aws cli commands
             (peut-gerer-send-command peut-gerer-command))
       "s" 'save-buffer
       "t" 'xc/open-terminal
       "x" 'eval-expression
       "p" 'xc/pop-buffer-into-frame
       )

    (general-define-key :keymaps 'anaconda-mode-map
     :states '(insert emacs)
     "-" #'(lambda () (interactive) (insert "_"))
     "_" #'(lambda () (interactive) (insert "-"))
     )
    (general-def :keymaps 'anaconda-mode-map
      "<apps>" 'xc/kill-python
      "<f6>" 'xc/insert-breakpoint
      "<C-S-f10>" 'peut-gerer-set-command-to-current-file
      "<S-f10>" 'peut-gerer-buffer-file-to-shell
      "<C-S-f7>" 'peut-gerer-set-command-to-current-file
      "<S-f7>" 'peut-gerer-buffer-file-to-shell
      )

    ;;(general-define-key :keymaps 'comint-mode-map
     ;;:states '(insert emacs)
     ;;"-" #'(lambda () (interactive) (insert "_"))
     ;;"_" #'(lambda () (interactive) (insert "-"))
    ;;)


    (general-def :keymaps 'comint-mode-map
      "C-l" 'comint-clear-buffer
      "C-x C-l" 'recenter-top-bottom
      "C-r" 'comint-history-isearch-backward
      "<apps>" 'xc/kill-python
      )

    (general-def :keymaps 'dired-mode-map
      "RET" 'dired-find-alternate-file  ; kill dired buffer on select
      ;; "<down-mouse-1>" 'dired-find-alternate-file
      ;; "<mouse-1>" 'dired-find-alternate-file
      "<mouse-2>" 'dired-mouse-find-file
      )

    ;; (general-define-key :keymaps 'elpy-mode-map
    ;;  :states '(insert emacs)
    ;;  "-" #'(lambda () (interactive) (insert "_"))
    ;;  "_" #'(lambda () (interactive) (insert "-"))
    ;;  )

    ;; (general-def
    ;;   :keymaps 'elpy-mode-map
    ;;   "<C-return>" 'nil
    ;;   "<C-S-return>" 'nil
    ;;   )
    (general-def :keymaps 'elpy-mode-map
      "<C-S-return>"'elpy-shell-send-statement
      "<C-return>" 'elpy-shell-send-statement-and-step
      "<M-return>" 'elpy-shell-send-group
      "<M-S-return>" 'elpy-shell-send-group-and-step
      "<pause>" 'elpy-test-pytest-runner
      ;; "<insert>" 'elpy-test-pytest-runner
      "C-c o" 'elpy-occur-definitions
      )

    ;; (general-def :keymaps 'emacs-lisp-mode-map
      ;; :states 'normal
      ;; :prefix "SPC"
    (xc/steno-define :keymaps 'emacs-lisp-mode-map
                     :states 'normal
      "e" 'eval-last-sexp
      )

    (general-def :keymaps 'emacs-lisp-mode-map
      "C-<next>" 'forward-page  ; C-PgUp goto previous linebreak
      "C-<prior>" 'backward-page ; C-PgDown goto next linebreak
      )

    ;; won't work in terminal bc of how terminals work
    (general-def :keymaps 'evil-emacs-state-map
      "<escape>" 'evil-normal-state
      )
    (general-def :keymaps 'evil-normal-state-map
      "C-b" 'backward-char
      "C-f" 'forward-char
      )

    (general-def :keymaps 'helm-map
      "<escape>"  'helm-keyboard-quit
      ;; "<down>" 'helm-follow-action-forward
      ;; "<up>" 'helm-follow-action-backward
      )

    ;; How to generate Info docs for Python (and others)!
    ;; https://stackoverflow.com/a/65100142/5065796
    (general-def :keymaps 'Info-mode-map
      "a" 'info-apropos
      "G" '(lambda () (interactive) (xc/Info-current-node-to-url 4))
      "P" '(lambda () (interactive) (Info-goto-node "(python)"))
      "U" 'xc/Info-current-node-to-url
      "h" 'nil  ; hitting 'h' by accident kills all window arrangement
      )

    ;;(general-def :keymaps 'ledger-mode-map
      ;;:states 'normal
      ;;:prefix "SPC"

    (xc/steno-define :keymaps 'ledger-mode-map
                     :states 'normal
      ;; "n" 'ledger-display-balance-at-point
      "d" '(lambda () (interactive) (scroll-other-window-down 1))
      "u" '(lambda () (interactive) (scroll-other-window 1))
      "n" 'xc/balance-at-point
      "r" 'ledger-report
      "a" 'ledger-post-align-dwim
      "t" 'ledger-add-transaction
      "c" 'ledger-fully-complete-xact
      ";" 'xc/toggle-comment-contiguous-lines
      "k" 'xc/ledger-kill-current-transaction
      "y" 'xc/ledger-kill-ring-save-current-transaction
      "[" 'evil-numbers/dec-at-pt
      "]" 'evil-numbers/inc-at-pt
      "g" 'bm-common-next
      )

    (general-def :keymaps 'lsp-mode-map
      "<mouse-3>" 'right-click-context-menu
      "<down-mouse-3>" 'right-click-context-menu
      "C-c C-d" 'lsp-describe-thing-at-point
      )

    (general-def :keymaps 'magit-status-mode-map
      "S-<tab>" 'magit-section-cycle-global
      "<tab>" 'magit-section-toggle
      )

    ;; Disable mouse click on minibuffer from opening messages
    (general-def :keymaps 'minibuffer-inactive-mode-map [mouse-1] nil)

    ;; ;; remap completion for minibuffer so that Plover can do Spaces After
    ;; (general-def :keymaps 'minibuffer-local-completion-map
    ;;   "SPC"  'self-insert-command       ;; now inserts a space character
    ;;   "S-<tab>" 'minibuffer-complete-word ;; previously was space
    ;;   "<tab>" 'minibuffer-complete    ;; previously was tab
    ;;   )

    ;; (general-def :keymaps 'occur-mode-map
    ;;   ;; "<escape>" 'quit-window
    ;;   "<tab>" '(lambda () (interactive) (occur-mode-mouse-goto) (xc/switch-to-last-window))
    ;;   "RET" '(lambda () (interactive) (occur-mode-goto-occurrence) (with-current-buffer "*Occur*" (quit-window)))
    ;;   )

    ;; (general-define-key :keymaps 'org-mode-map
    ;;  :states '(insert emacs)
    ;;  "-" #'(lambda () (interactive) (insert "_"))
    ;;  "_" #'(lambda () (interactive) (insert "-"))
    ;;  )

    ;;(general-def :keymaps 'org-mode-map
      ;;:states 'normal
      ;;:prefix "SPC"

    (xc/steno-define :keymaps 'org-mode-map
                     :states 'normal

      "SPC" 'org-ctrl-c-ctrl-c
      "→" 'org-ctrl-c-ctrl-c
      "t" 'org-insert-structure-template
      )
    (general-def :keymaps 'org-mode-map
      "C-c h o" 'org-clock-out
      "C-c h i" 'org-clock-in
      "C-c h d" 'org-clock-display
      "C-c C--" 'org-ctrl-c-minus
      )

    ;; (general-def :keymaps 'python-mode-map
    ;;   :states 'normal
    ;;   :prefix "SPC"
    ;;   "d" 'xc/python-occur-definitions
    ;;   "c" 'xc/string-inflection-style-cycle
    ;;   "u" 'xc/pyside-lookup
    ;;   )

    (general-def :keymaps 'python-mode-map
      "<apps>" 'xc/kill-python
      "<f6>" 'xc/insert-breakpoint
      "S-<f6>" '(lambda () (interactive) (xc/insert-breakpoint "breakpoint()")) ; pdb
      "<C-S-f10>" 'peut-gerer-set-command-to-current-file
      "<S-f10>" '(lambda () (interactive) (quit-process (get-buffer-process peut-gerer-shell)) (peut-gerer-buffer-file-to-shell))
      "<C-S-f7>" 'peut-gerer-set-command-to-current-file
      "<S-f7>" '(lambda () (interactive) (quit-process (get-buffer-process peut-gerer-shell)) (peut-gerer-buffer-file-to-shell))
      "<f3>" '(lambda () (interactive) (quit-process (get-buffer-process peut-gerer-shell)) (peut-gerer-buffer-file-to-shell))
      "<S-wheel-down>" 'python-nav-forward-block
      "<S-wheel-up>" 'python-nav-backward-block
      )

    ;; (general-define-key :keymaps 'python-mode-map
    ;;  :states '(insert emacs)
    ;;  "-" #'(lambda () (interactive) (insert "_"))
    ;;  "_" #'(lambda () (interactive) (insert "-"))
    ;;  )

    ;; (general-define-key :keymaps 'python-mode-map
    ;;  :states '(insert emacs)
    ;;  "-" #'self-insert-command
    ;;  "_" #'self-insert-command
    ;;  )

    (general-def :keymaps 'smerge-mode-map
      :states 'normal
      "RET" 'smerge-keep-current
      "E" 'smerge-ediff
      "b" 'smerge-keep-lower
      "n" 'smerge-next
      "p" 'smerge-prev
      "t" 'smerge-keep-upper

      ;; "a" 'smerge-keep-all
      ;; "b" 'smerge-keep-base
      ;; "l" 'smerge-keep-lower
      ;; "m" 'smerge-keep-upper
      ;; "n" 'smerge-next
      ;; "o" 'smerge-keep-lower
      ;; "p" 'smerge-prev
      ;; "u" 'smerge-keep-upper
      )
    )

  (if xc/debug (message "general.el")))


(use-package elpher
  :after (:all org)
  :straight (:repo "git://thelambdalab.xyz/elpher.git"))


(use-package elpy
  ;; :disabled
  :after (:all pyvenv helm key-chord use-package-chords org)
  :straight (:fork "excalamus/elpy")
  :init
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'elpy-mode-hook #'hs-minor-mode)
  (add-hook 'elpy-mode-hook (lambda () (company-mode -1)))
  :config
  (pyvenv-mode 1)
  (elpy-enable)
  (setq elpy-rpc-timeout 2)

  ;; (remove-hook 'xref-backend-functions #'elpy--xref-backend t)
  (advice-add 'elpy--xref-backend :override #'dumb-jump-xref-activate)

  ;; color docstings differently than strings
  ;; https://stackoverflow.com/questions/27317396/how-to-distinguish-python-strings-and-docstrings-in-an-emacs-buffer
  (setq py-use-font-lock-doc-face-p t)

  (setq elpy-rpc-virtualenv-path 'current)

  (when (eq system-type 'gnu/linux)
    (setq-default indent-tabs-mode nil)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt")
    (setq elpy-rpc-python-command "python3"))

  (when (eq system-type 'windows-nt)
    (setq-default indent-tabs-mode nil)
    ;; https://emacs.stackexchange.com/questions/24750/emacs-freezes-with-ipython-5-0-0
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt")
    (setq elpy-rpc-python-command "python"))

  (if xc/debug (message "elpy")))


(use-package erc
  :config
  (setq erc-nick "excalamus")
  (setq erc-user-full-name "excalamus")
  (setq erc-port 6697)
  (setq erc-default-server "irc.libera.chat"))


(use-package ess
  :after (:all org)
  :straight (:fork "excalamus/ess")
  :init (require 'ess-site)
  :config

  (if xc/debug (message "ess")))


(use-package evil
  :after (:all dumb-jump key-chord org)
  :straight (:fork "excalamus/evil")

  :init
  ;; C-i is TAB, so setting this makes TAB evil-jump-forward
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'elpher-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  ;; (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'magit-repolist-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  ;; https://github.com/emacs-evil/evil/issues/1074
  (setq evil-undo-system 'undo-redo)

  ;; Coordinate states with cursor color
  (if (not (eq xc/device 'termux))
      (progn
        (setq evil-emacs-state-cursor '("SkyBlue2" bar))
        (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
        (setq evil-insert-state-cursor '("light gray" bar))
        (setq evil-visual-state-cursor '("gray" box))
        (setq evil-motion-state-cursor '("plum3" box)))
    (progn
      (setq evil-emacs-state-tag (propertize " <E> " 'face '((:background "color-117"))))
      (setq evil-normal-state-tag (propertize " <N> " 'face '((:background "color-172"))))
      (setq evil-insert-state-tag (propertize " <I> " 'face '((:background "color-244"))))
      (setq evil-visual-state-tag (propertize " <V> " 'face '((:background "color-246"))))
      (setq evil-motion-state-tag (propertize " <M> " 'face '((:background "color-177"))))))

  (add-hook 'python-mode-hook #'hs-minor-mode)

  (if xc/debug (message "evil")))


(use-package evil-lion
  :after (:all evil org)
  :straight (:fork "excalamus/evil-lion")
  :config
  (evil-lion-mode 1)

  (if xc/debug (message "evil-lion")))


(use-package evil-numbers
  :after (:all evil org)
  :straight (:fork "excalamus/evil-numbers")
  :config

  (if xc/debug (message "evil-numbers")))


(use-package evil-surround
  :after (:all evil org)
  :straight (:fork "excalamus/evil-surround")
  :config
  (global-evil-surround-mode 1)

  (if xc/debug (message "evil-surround")))


(use-package expand-region
  :after (:all org)
  :straight (:fork "excalamus/expand-region.el")
  :config

  (if xc/debug (message "expand-region.el")))


(use-package flycheck
  :after (:all org)
  :straight (:fork "excalamus/flycheck")
  :config

  (if xc/debug (message "flycheck")))


(straight-use-package 'flymake)


(use-package free-keys
  :after (:all org)
  :straight (:fork "excalamus/free-keys")
  :config

  (if xc/debug (message "free-keys")))


(use-package fold-this
  :after (:all org)
  :straight (:fork "excalamus/fold-this.el")
  :config

  (if xc/debug (message "fold-this.el")))


(use-package git-timemachine
  :after (:all org)
  :straight (:host github :repo "excalamus/git-timemachine")
  :config

  (if xc/debug (message "git-timemachine")))


(use-package helm
  :after (:all org)
  :straight (:fork "excalamus/helm")
  :config

  ;; https://teddit.net/r/emacs/comments/3mtus3/how_to_display_a_list_of_classes_functions_etc/cvj6p9z?utm_source=share&utm_medium=web2x&context=3
  (require 'helm-imenu)

  ;; (defun xc--helm-imenu-transformer (cands)
  ;;   (with-helm-current-buffer
  ;;     (save-excursion
  ;;    (cl-loop for (func-name . mrkr) in cands
  ;;             collect
  ;;             (cons (format "Line %4d: %s"
  ;;                           (line-number-at-pos mrkr)
  ;;                           (progn (goto-char mrkr)
  ;;                                  (buffer-substring mrkr (line-end-position))))
  ;;                   (cons func-name mrkr))))))

  (defun xc--helm-imenu-transformer (cands)
    (with-helm-current-buffer
      (save-excursion
        (cl-loop for (func-name . mrkr) in cands
                 collect
                 (cons (format "%s %s"
                               (progn (goto-char mrkr)
                                      (make-string (- (marker-position mrkr) (line-beginning-position)) ?\s))
                               (progn (goto-char mrkr)
                                      (buffer-substring mrkr (line-end-position))))
                       (cons func-name mrkr))))))

  (defvar xc/helm-imenu-source  (helm-make-source "Imenu" 'helm-imenu-source
                                  :candidate-transformer
                                  'xc--helm-imenu-transformer))
  (defun xc/helm-imenu ()
    (interactive)
    (let ((imenu-auto-rescan t)
          (imenu-sort-function #'imenu--sort-by-position)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'xc/helm-imenu-source
            :preselect str
            :buffer "*helm imenu*")))

  (if xc/debug (message "helm")))


(use-package helm-swoop
  :after (:all helm org)
  :straight (:fork "excalamus/helm-swoop")
  :config

  (defun xc/-reset-linum-hack ()
    "Hack to reset line numbers by switching to next buffer and switching back."
    (progn
      (switch-to-buffer (other-buffer (current-buffer) 1))
      (switch-to-buffer (other-buffer (current-buffer) 1))))

  (add-hook 'helm-after-action-hook 'xc/-reset-linum-hack)
  (add-hook 'helm-quit-hook 'xc/-reset-linum-hack)

  ;; toggle syntax coloring in suggestions
  (setq helm-swoop-speed-or-color t)

  (set-face-attribute 'helm-swoop-target-word-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit    'lazy-highlight)
  (set-face-attribute 'helm-swoop-target-line-face nil
                      :foreground         'unspecified
                      :distant-foreground 'unspecified
                      :background         'unspecified
                      :inherit            'secondary-selection)
  (set-face-attribute 'helm-selection nil
                      :distant-foreground 'unspecified
                      :background         'unspecified
                      :inherit            'secondary-selection)


  (if xc/debug (message "helm-swoop")))


;; bundled with emacs
(use-package hi-lock
  :after (:all org)
  :init
  (defun xc/toggle-global-hl-line-sticky-flag ()
    "Toggle whether highlighted line persists when switching windows.

    This function does not currently behave as expected.  Resetting
    `global-hl-line-sticky-flag' does not take effect until the next
    time `global-hl-line-mode' is turned on.

    For more information, see `global-hl-line-sticky-flag'."
    (interactive)
    (if global-hl-line-sticky-flag
        (setq global-hl-line-sticky-flag nil)
      (setq global-hl-line-sticky-flag t))

    ;; once toggled, the mode needs to be restarted
    (global-hl-line-mode -1)
    (global-hl-line-mode 1))

  (defface xc/hi-comint
    '((t (:background "dim gray")))
    "Face for comint mode."
    :group 'hi-lock-faces)

  :config
  ;; Toggle on: M-s h r
  ;; Toggle off: M-s h u

  (set-face-attribute 'hi-yellow nil :background "yellow3" :foreground "gray30" :distant-foreground "yellow3 "    :box "dim gray")
  (set-face-attribute 'hi-pink   nil                       :foreground "gray30" :distant-foreground "pink"        :box "dim gray")
  (set-face-attribute 'hi-green  nil                       :foreground "gray30" :distant-foreground "light green" :box "dim gray")
  (set-face-attribute 'hi-blue   nil                       :foreground "gray30" :distant-foreground "light blue " :box "dim gray")

  (if xc/debug (message "hi-lock")))


(use-package hl-todo
  :after (:all org)
  :straight (:fork "excalamus/hl-todo")
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"    . "#f84547")
          ("FIXME"   . "#f84547")
          ("WARNING" . "#FFFF00")
          ("DEBUG"   . "#8485ce")
          ("NOTE"    . "#95c76f")))
  (global-hl-todo-mode)

  (if xc/debug (message "hl-todo")))


(use-package htmlize
  :after (:all org)
  :straight (:fork "excalamus/emacs-htmlize")
  :config

  (if xc/debug (message "emacs-htmlize")))


(use-package iedit
  :after (:all org)
  :straight (:fork "excalamus/iedit")
  :config

  (if xc/debug (message "iedit")))

;; on windows, you need to install Hunspell
;; https://sourceforge.net/projects/ezwinports/files/
;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/EZWinPorts.html
(use-package ispell
  :after (:all org)
  :if (eq system-type 'windows-nt)
  :config
  (setq ispell-program-name "C:/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))


(use-package keycast
  :after (:all org)
  :straight (:fork "excalamus/keycast")
  :config

  (setq keycast-separator-width 30)
  (set-face-attribute 'keycast-key nil :background "gray29" :foreground "yellow3" :height 1.2)
  (set-face-attribute 'keycast-command nil :background "gray29" :foreground "yellow3" :height 1.2 :weight 'bold :box '(:line-width -3 :style released-button))
  (if xc/debug (message "keycast")))


(use-package language-detection
  :after (:all org)
  :straight (:fork "excalamus/language-detection.el")
  :config
  ;; (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode) (awk awk-mode) (c c-mode) (cpp c++-mode) (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode) (css css-mode) (dart dart-mode) (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode) (erlang erlang-mode) (fortran fortran-mode) (fsharp fsharp-mode)
                  (go go-mode) (groovy groovy-mode) (haskell haskell-mode) (html html-mode) (java java-mode)
                  (javascript javascript-mode) (json json-mode javascript-mode) (latex latex-mode) (lisp lisp-mode)
                  (lua lua-mode) (matlab matlab-mode octave-mode) (objc objc-mode c-mode) (perl perl-mode)
                  (php php-mode) (prolog prolog-mode) (python python-mode) (r r-mode) (ruby ruby-mode)
                  (rust rust-mode) (scala scala-mode) (shell shell-script-mode) (smalltalk smalltalk-mode)
                  (sql sql-mode) (swift swift-mode) (visualbasic visual-basic-mode) (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (setq shr-external-rendering-functions
        '((pre . eww-tag-pre)))

  (if xc/debug (message "language-detection.el")))


(use-package ledger-mode
  :after (:all org)
  :straight (:fork "excalamus/ledger-mode")
  :defer t
  :config
  (setq ledger-post-amount-alignment-column 60)
  (setq ledger-report-auto-refresh-sticky-cursor t)
  (setq ledger-highlight-xact-under-point nil)
  (setq ledger-report-resize-window nil)

  (defvar xc/ledger-highlight-regexp "dummy"
    "Regexp for matching lines in Ledger Report buffer.")

  (defun xc/set-ledger-highlight-regexp (reg)
    "Set `xc/ledger-highlight-regexp' to REG."
    (interactive
     (list (read-string "Regexp: ")))
    (let ((quoted (regexp-quote reg)))
      (setq xc/ledger-highlight-regexp quoted)
      (message "Set `xc/ledger-highlight-regexp' to %s"
               xc/ledger-highlight-regexp)))

  (add-hook 'ledger-report-after-report-hook
            (lambda () (highlight-lines-matching-regexp
                        xc/ledger-highlight-regexp
                        'hi-yellow)))

  (if xc/debug (message "ledger-mode")))


(use-package lsp-jedi
;; requires pip install jedi-language-server
;; https://github.com/pappasam/jedi-language-server
  :disabled
  :after (:all org lsp-mode)
  :straight (:fork "excalamus/lsp-jedi")
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))


(use-package magit
  :after (:all evil org)
  :straight (:fork "excalamus/magit")
  :init
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide)))
  :config

  ;; For privacy's sake, define `magit-repository-directories' in
  ;; secret-lisp.el:
  ;;
  ;; (setq magit-repository-directories
  ;;       '(("C:/path/to/project1/repo/" . 0)
  ;;         ("C:/path/to/project2/repo/" . 0)
  ;;         ))

  (add-hook 'git-commit-mode-hook 'evil-emacs-state)

  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident                  ())
          ("D"        1 magit-repolist-column-dirty                  ())
          ("B<P"      3 magit-repolist-column-unpulled-from-pushremote
           ((:right-align t)
            (:help-echo "Pushremote changes not in branch")))
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U"      3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Version" 25 magit-repolist-column-version                ())
          ("Path"    99 magit-repolist-column-path                   ())
          ))

  (if xc/debug (message "magit")))


(use-package markdown-toc
  :after (:all markdown-mode org)
  :straight (:fork "excalamus/markdown-toc")
  :config

  (if xc/debug (message "markdown-toc")))


(use-package nameless
  :after (:all org)
  :straight (:fork "excalamus/nameless")
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config

  (if xc/debug (message "nameless")))


(use-package nov
  :after (:all org)
  :init
  :config

  (if xc/debug (message "nov")))


(defun xc/-org-mode-config ()
  ;; (require 'ox-texinfo)
  ;; (require 'ox-md)

  (if (eq xc/device 'gnu/linux)
      (setq org-babel-python-command "python3"))
  (setq org-adapt-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select 'always)
  (setq org-indent-indentation-per-level 0)
  (setq org-todo-keywords
        '((sequence
           ;; open items
           "TODO"                 ; todo, not active
           "CURRENT"              ; todo, active item
           "PENDING"              ; requires more information (timely)
           "|"  ; entries after pipe are considered completed in [%] and [/]
           ;; closed items
           "DONE"        ; completed successfully
           "ON-HOLD"     ; requires more information (indefinite time)
           "CANCELED"    ; no longer relevant, not completed
           )))

  (setq org-todo-keyword-faces
        '(
          ("TODO" . "light pink")
          ("CURRENT" . "yellow")
          ("DONE" . "light green")
          ("PENDING" . "light blue")
          ("ON-HOLD" . "plum")
          ("CANCELED" . "gray")
          ))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (makefile . t)
     (python . t)
     (emacs-lisp . t)
     (ledger . t)
     (latex . t)
     (shell . t)
     (scheme . t)
     ))

  (defun xc/new-clock-task ()
    "Switch to new task by clocking in after clocking out."
    (interactive)
    (org-clock-out)
    (org-clock-in))

  ;; org-mode doesn't automatically save archive files for some
  ;; reason.  This is a ruthless hack which saves /all/ org buffers in
  ;; archive.  https://emacs.stackexchange.com/a/51112/15177
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers))

;; https://github.com/raxod502/straight.el/issues/624
(if (eq xc/device 'gnu/linux)

    ;; use latest org
    (use-package org
      :straight org
      :config
      (xc/-org-mode-config)

      (if xc/debug (message "org")))

  ;; use built-in
  (use-package org
    :straight (:type built-in)
    :config
    (xc/-org-mode-config)

    (if xc/debug (message "org"))))


(use-package peut-publier
  :after (:all org)
  ;; :straight (:repo "git@github.com:excalamus/peut-publier.git")
  :straight (:repo "https://github.com/excalamus/peut-publier.git")
  :config

  (if xc/debug (message "peut-publier")))

;; 
(use-package peut-gerer
  :after (:all right-click-context org)
  :straight (:repo "https://github.com/excalamus/peut-gerer.git" :branch "main")
  :config

  ;; For privacy's sake, define `peut-gerer-project-alist' in secret-lisp.el:
  ;;
  ;;     (setq peut-gerer-project-alist
  ;;           '(("project-x"
  ;;              :root "/data/data/com.termux/files/home/projects/project-x/"
  ;;              :main "main.py"
  ;;              :venv  "/data/data/com.termux/files/home/projects/project-x/venv/"
  ;;              :activate "/data/data/com.termux/files/home/projects/project-x/venv/bin/activate"
  ;;              :commands ("pyinstaller build.spec")
  ;;              )
  ;;             ("project-a"
  ;;              :root "C:\\projects\\project-umbrella\\apps\\project_a\\"
  ;;              :main "project_a.py"
  ;;              :venv "C:\\Users\\excalamus\\Anaconda3\\envs\\project_a\\"
  ;;              :activate "C:\\Users\\excalamus\\Anaconda3\\condabin\\conda.bat activate"
  ;;              )))

  (if (eq xc/device 'gnu/linux)
      (setq peut-gerer-command-prefix "python3"))

  (setq peut-gerer-after-activate-functions '(pyvenv-activate))

  (setq peut-gerer-after-select-functions
        '((lambda (x) (funcall 'pyvenv-deactivate))
          pyvenv-activate))

  ;; ;; disable sending to shell while in shell because it would only be
  ;; ;; useful for concatenating duplicates of a region; if you have
  ;; ;; region selected and you send that region to the current buffer,
  ;; ;; the region appears immediately after the region.
  ;; (add-to-list 'right-click-context-global-menu-tree
  ;;              '("Send to region to shell"
  ;;                :call (peut-gerer-send-region)
  ;;                :if
  ;;                (and (use-region-p)
  ;;                     ;; only disables to peut-gerer registered shells
  ;;                     (not
  ;;                      (member (string-trim (buffer-name) "*" "*")
  ;;                              peut-gerer--active-projects-alist)))))

  ;; quick hack; create an xc/on-demand-window (C-<f1>), then you can
  ;; select a region anywhere and send that region to the odw.  For
  ;; use in exploratory debugging.  Try stuff in the repl, then send
  ;; that to the script.
  (add-to-list 'right-click-context-global-menu-tree
               '("Send region to on-demand-window"
                 :call (xc/send-line-or-region)))

  (add-to-list 'right-click-context-global-menu-tree
               '("Send to shell"
                 ;; :call (xc/send-line-or-region nil nil nil peut-gerer-shell)))
                 :call (xc/send-line-or-region nil peut-gerer-shell nil nil)))

  (add-to-list 'right-click-context-global-menu-tree
               '("Search..."
                 ;; ("pyside" :call (xc/search-Qt))
                 ("QGIS" :call (xc/search-qgis))
                 ("sdl-wiki" :call (xc/search-sdl-wiki))
                 ("Open Jira ticket" :call (xc/search-jira))
                 ("ddg" :call (xc/search-ddg))))

  (if (eq xc/device 'gnu/linux)
      (add-to-list 'right-click-context-global-menu-tree
                   '("Search for in SDL" :call (xc/search-sdl)))
    (add-to-list 'right-click-context-global-menu-tree
                 '("Search for in PySide" :call (xc/search-Qt))))

  ;; (pop right-click-context-global-menu-tree)

  (if xc/debug (message "peut-gerer")))


(use-package qml-mode
  :after (:all org)
  :straight (:fork "excalamus/qml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

  (if xc/debug (message "qml-mode")))


(use-package right-click-context
  :after (:all org)
  :straight (:fork "excalamus/right-click-context")
  :config
  (right-click-context-mode 1)

  (if xc/debug (message "right-click-context")))


(use-package rg
  :after (:all org)
  :straight (:fork "excalamus/rg.el")
  :config

  (if xc/debug (message "rg.el")))


;; skeeto fork
(use-package simple-httpd
  :after (:all org)
  :straight (:fork "excalamus/emacs-web-server")
  :config

  (if xc/debug (message "emacs-web-server")))


(use-package smartparens
  :after (:all org)
  :straight (:fork "excalamus/smartparens")
  :config
  (require 'smartparens-config)
  ;; this behvior doesn't take when configured after startup for some
  ;; reason.  See `https://github.com/Fuco1/smartparens/issues/965'
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "\"" "\"" :unless '(sp-point-before-word-p sp-point-after-word-p))
  (smartparens-global-mode 1)

  (if xc/debug (message "smartparens")))


(use-package string-inflection
  :after (:all org)
  :straight (:fork "excalamus/string-inflection")
  :config
  (defun xc/-string-inflection-style-cycle-function (str)
    "foo-bar => foo_bar => FOO_BAR => fooBar => FooBar => foo-bar"
    (cond
     ;; foo-bar => foo_bar
     ((string-inflection-kebab-case-p str)
      (string-inflection-underscore-function str))
     ;; foo_bar => FOO_BAR
     ((string-inflection-underscore-p str)
      (string-inflection-upcase-function str))
     ;; FOO_BAR => fooBar
     ((string-inflection-upcase-p str)
      (string-inflection-pascal-case-function str))
     ;; fooBar => FooBar
     ((string-inflection-pascal-case-p str)
      (string-inflection-camelcase-function str))
     ;; FooBar => foo-bar
     ((string-inflection-camelcase-p str)
      (string-inflection-kebab-case-function str))))

  (defun xc/string-inflection-style-cycle ()
    "foo-bar => foo_bar => FOO_BAR => fooBar => FooBar => foo-bar"
    (interactive)
    (string-inflection-insert
     (xc/-string-inflection-style-cycle-function
      (string-inflection-get-current-word))))

  (if xc/debug (message "string-inflection")))


(use-package sql
  :after (:all org)
  :config
  ;; ;; load project profiles, kept here versus lisp/ for security sake
  ;; (if (eq system-type 'windows-nt)
  ;;     (load "~/sql-connections.el"))
  (setq sql-postgres-login-params nil)

  (if xc/debug (message "sql")))


(use-package sql-indent
  :after (:all org)
  :straight (:fork "excalamus/emacs-sql-indent")
  :config

  (if xc/debug (message "emacs-sql-indent")))


(use-package web-mode
  :after (:all org)
  :straight (:fork "excalamus/web-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (if xc/debug (message "web-mode")))


(use-package xref
  :config
  ;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  (setq xref-prompt-for-identifier
        '(not xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references)))


(use-package yaml-mode
  :after (:all org)
  :straight (:fork "excalamus/yaml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (if xc/debug (message "yaml-mode")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xc/backup-region-or-buffer (&optional buffer-or-name file beg end)
  "Write copy of BUFFER-OR-NAME between BEG and END to FILE.

BUFFER-OR-NAME is either a buffer object or name. Uses current
buffer when none is passed.  Uses entire buffer for region when
BEG and END are nil.  Prompts for filename when called
interactively.  Will always ask before overwriting. Returns the
name of the file written to.

See URL `https://stackoverflow.com/a/18780453/5065796'."
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffo (or (get-buffer buffer-or-name) (error "Buffer does not exist")))  ; buffer object
         (buffn (or (buffer-file-name buffo) (buffer-name buffo)))                 ; buffer name
         (beg (or beg (if (use-region-p) (region-beginning) beg)))
         (end (or end (if (use-region-p) (region-end) end)))
         (prompt (if (and beg end) "region" "buffer"))
         (new (if (called-interactively-p 'interactive)
                  (read-file-name
                   (concat "Write " prompt " to file: ")
                   nil nil nil
                   (and buffn (file-name-nondirectory buffn)))
                (or file (error "Filename cannot be nil"))))
         ;; See `write-region' for meaning of 'excl
         (mustbenew (if (and buffn (file-equal-p new buffn)) 'excl t)))
    (with-current-buffer buffo
      (if (and beg end)
          (write-region beg end new nil nil nil mustbenew)
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) new nil nil nil mustbenew))))
    new))


(defun xc/define-abbrev (name expansion &optional fixed table interp)
  "Define abbrev with NAME and EXPANSION for last word(s) before point in TABLE.

FIXED sets case-fixed; default is nil.

TABLE defaults to `global-abbrev-table'.

Behaves similarly to `add-global-abbrev'.  The prefix argument
specifies the number of words before point that form the
expansion; or zero means the region is the expansion.  A negative
argument means to undefine the specified abbrev.  This command
uses the minibuffer to read the abbreviation.

Abbrevs are overwritten without prompt when called from Lisp.

\(fn NAME EXPANSION &optional FIXED TABLE)"
  (interactive
   (let* ((arg (prefix-numeric-value current-prefix-arg))
          (exp (and (>= arg 0)
                    (buffer-substring-no-properties
                     (point)
                     (if (= arg 0) (mark)
                       (save-excursion (forward-word (- arg)) (point))))))
          (name (read-string (format (if exp "Abbev name: "
                                       "Undefine abbrev: "))))
          (expansion (and exp (read-string "Expansion: " exp)))
          (table (symbol-value (intern-soft (completing-read
                                             "Abbrev table (global-abbrev-table): "
                                             abbrev-table-name-list nil t nil nil "global-abbrev-table"))))
          (fixed (and exp (y-or-n-p (format "Fix case? ")))))
     (list name expansion fixed table t)))
  (let ((table (or table global-abbrev-table))
        (fixed (or fixed nil)))
    (set-text-properties 0 (length name) nil name)
    (set-text-properties 0 (length expansion) nil expansion)
    (if (or (null expansion)                     ; there is expansion to set,
            (not (abbrev-expansion name table))  ; the expansion is not already defined
            (not interp)                         ; and we're not calling from code (calling interactively)
            (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                              name (abbrev-expansion name table))))
        (define-abbrev table name expansion nil :case-fixed fixed))))


(defun xc/comint-exec-hook ()
  (interactive)
  (highlight-lines-matching-regexp "-->" 'xc/hi-comint)
  (setq comint-scroll-to-bottom-on-output t)
  (setq truncate-lines t)
  (set-window-scroll-bars (get-buffer-window (current-buffer)) nil nil 10 'bottom))

(add-hook 'comint-exec-hook #'xc/comint-exec-hook)


(defun xc/copy-symbol-at-point ()
  "Place symbol at point in `kill-ring'."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds))
         (sym (thing-at-point 'symbol)))
    (kill-ring-save beg end)
    (message "\"%s\"" sym)))


;; todo, when universal, prompt for mode
;; https://stackoverflow.com/a/21058075/5065796
(defun xc/create-scratch-buffer ()
  "Create a new numbered scratch buffer."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    (if (= n 1) initial-major-mode)))


;; https://stackoverflow.com/a/1110487
(eval-after-load "dired"
  '(progn
     (defun xc/dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the
point, or when prefix arg, the next N files"
       (interactive "P")
       (mapc 'find-file (dired-get-marked-files nil arg)))
     (define-key dired-mode-map "F" 'xc/dired-find-file)))

;; Auto-refresh dired on file change
;; https://superuser.com/a/566401/606203
(add-hook 'dired-mode-hook 'auto-revert-mode)


(defun xc/duplicate-buffer (&optional dup)
  "Copy current buffer to new buffer named DUP.

Default DUP name is `#<buffer-name>#'."
  (interactive)
  (let* ((orig (buffer-name))
         (dup (or dup (concat "%" orig "%" ))))
    (if (not (bufferp dup))
        (progn
          (get-buffer-create dup)
          (switch-to-buffer dup)
          (insert-buffer-substring orig)
          (message "Duplicate buffer `%s' created" dup))
      (error "Duplicate buffer already exists"))))


(defun xc/kill-all-buffers-in-frame ()
  "Kill all buffers visible in selected frame."
  (interactive)
  (let ((buffer-save-without-query t))
    (walk-windows '(lambda (win) (kill-buffer (window-buffer win)))
                  nil (selected-frame))
    (delete-other-windows)))

(defun xc/kill-frame-and-buffers ()
  "Kill current frame along with its visible buffers."
  (interactive)
  (xc/kill-all-buffers-in-frame)
  (delete-frame nil t))


;; magit doesn't seem to revert buffers when creating a local copy of
;; a branch that exists on the remote. This happens when the branch is
;; made in BitBucket/GitHub or that someone has pushed up. The default
;; behavior of magit is supposed to auto revert all tracked files. You
;; would think that creating a local version of a remote branch would
;; do that, but it seems not. Maybe I'm getting myself mixed up?
(defun xc/revert-all-buffers (&optional arg)
  "Revert all file buffers.

Buffers visiting files that no longer exist are ignored.  Files
not readable (including do not exist) are ignored.  Other errors
are reported only as messages. Don't ask for confirmation when
called with a prefix.

See `https://emacs.stackexchange.com/a/24464/'"
  (interactive "p")
  (let ((noconfirm (if (= arg 1) nil t))
        (file))
    (dolist (buf (buffer-list))
      (setq file (buffer-file-name buf))
      (when (and file (file-readable-p file))
        (with-current-buffer buf
          (with-demoted-errors "Error: %S"
             (if (revert-buffer t noconfirm)
                 (message "Reverted buffer from file: %s" file)
               (message "User canceled revert for buffer: %s" buf))))))))


(defun xc/emacs-standalone (&optional arg)
  "Start standalone instance of Emacs.

Load Emacs without init file when called interactively.

\(fn\)"
  (interactive "p")
  (cond ((eql arg 1)
         (cond ((eq xc/device 'windows)
                (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe")))
               ((eq xc/device 'gnu/linux)
                (setq proc (start-process "emacs" nil "/usr/bin/env" "emacs")))
               ((eq xc/device 'termux)
                (setq (start-process "emacs" nil "/data/data/com.termux/files/usr/bin/emacs")))))
        ((eql arg 4)
         (cond ((eq xc/device 'windows)
                (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe" "-q")))
               ((eq xc/device 'gnu/linux)
                (setq proc (start-process "emacs" nil "/usr/bin/env" "emacs" "--no-init-file")))
               ((eq xc/device 'termux)
                (setq (start-process "emacs" nil "/data/data/com.termux/files/usr/bin/emacs" "--no-init-file")))))
         (t (error "Invalid arg")))
        (set-process-query-on-exit-flag proc nil))


(defun xc/reselect-last-region ()
  "Reselect the last region.

Taken from URL
`https://web.archive.org/web/20170118020642/http://grapevine.net.au/~striggs/elisp/emacs-homebrew.el'"
  (interactive)
  (let ((start (mark t))
        (end (point)))
    (goto-char start)
    (call-interactively 'set-mark-command)
    (goto-char end)))


(defun xc/get-file-name ()
  "Put filename of current buffer on kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (if filename
        (progn
          (kill-new filename)
          (message "%s" filename))
      (message "Buffer not associated with a file"))))


(defun xc/highlight-current-line ()
  (interactive)
  (let ((regexp
         (regexp-quote
          (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (face (hi-lock-read-face-name)))
    (highlight-lines-matching-regexp regexp face)))


(defun xc/Info-current-node-to-url (&optional arg)
  "Put the url of the current Info node into the kill ring.

The Info file name and current node are converted to a
url (hopefully) corresponding to the GNU online documentation.
With prefix arg, visit url with default web browser and do not
put url into the kill ring."
  (interactive "P")
  (unless Info-current-node
    (user-error "No current Info node"))
  (let* ((info-file (if (stringp Info-current-file)
                        (file-name-sans-extension
                         (file-name-nondirectory Info-current-file))))
         (node  Info-current-node)
         (url (concat
               "https://www.gnu.org/software/emacs/manual/html_node/"
               info-file "/"
               (replace-regexp-in-string " " "-" node t)
               ".html")))
    (if arg
        (browse-url-default-browser url)
      (kill-new url))
    (message "%s" url)))


(defun xc/search (&optional prefix engine beg end)
  "Search the web for something.

If a region is selected, lookup using region defined by BEG and
END.  When no region or issue given, try using the thing at
point.  If there is nothing at point, ask for the search query."
  (interactive)
  (let* ((engine-list `(("ddg" . "https://duckduckgo.com/?q=%s")
                        ("Qt" . "https://doc-snapshots.qt.io/qtforpython-5.15/search.html?check_keywords=yes&area=default&q=%s")
                        ("qgis" . "https://qgis.org/pyqgis/master/search.html?check_keywords=yes&area=default&q=%s")
                        ("sdl-wiki" . "https://wiki.libsdl.org/wiki/search/?q=%s")
                        ("sdl" . "https://wiki.libsdl.org/%s")
                        ("jira" . ,(concat xc/atlassian "%s"))))
         (beg (or beg (if (use-region-p) (region-beginning)) nil))
         (end (or end (if (use-region-p) (region-end)) nil))
         (thing (thing-at-point 'symbol t))
         (lookup-term (cond ((and beg end) (buffer-substring-no-properties beg end))
                            (thing thing)
                            (t (read-string "Search for: "))))
         (engine (or engine (completing-read "Select search engine: " engine-list nil t (caar engine-list))))
         (query (if prefix (format "%s %s" prefix lookup-term) lookup-term))
         (search-string (url-encode-url (format (cdr (assoc engine engine-list)) query))))
    (browse-url-default-browser search-string)))

(defun xc/search-ddg (&optional beg end)
  (interactive)
  (xc/search nil "ddg" beg end))

(defun xc/search-jira (&optional beg end)
  (interactive)
  (xc/search nil "jira" beg end))

(defun xc/search-qgis (&optional beg end)
  (interactive)
  (xc/search nil "qgis" beg end))

(defun xc/search-Qt (&optional beg end)
  (interactive)
  (xc/search nil "Qt" beg end))

(defun xc/search-sdl (&optional beg end)
  (interactive)
  (xc/search nil "sdl" beg end))

(defun xc/search-sdl-wiki (&optional beg end)
  (interactive)
  (xc/search nil "sdl-wiki" beg end))


(defun minibuffer-inactive-mode-hook-setup ()
  "Allow autocomplete in minibuffer.

Make `try-expand-dabbrev' from `hippie-expand' work in
mini-buffer @see `he-dabbrev-beg', so we need re-define syntax
for '/'.  This allows \\[dabbrev-expand] to be used for
expansion.

Taken from URL
`https://blog.binchen.org/posts/auto-complete-word-in-emacs-mini-buffer-when-using-evil.html'"
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))

(add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)


(defun xc/narrow-to-defun-indirect (&optional arg)
  "Narrow to function or class with preceeding comments.

Open in other window with prefix.  Enables
`which-function-mode'."
  (interactive "P")
  (deactivate-mark)
  (which-function-mode t)
  (let* ((name (concat (buffer-name) "<" (which-function) ">"))
         (clone-indirect-fun (if arg 'clone-indirect-buffer-other-window 'clone-indirect-buffer))
         (switch-fun (if arg 'switch-to-buffer-other-window 'switch-to-buffer))
         ;; quasi-quote b/c name gets passed as symbol otherwise
         (buf (apply clone-indirect-fun `(,name nil))))
    (with-current-buffer buf
      (narrow-to-defun arg))
    ;; both switch-fun and buf are symbols
    (funcall switch-fun buf)))


(defun xc/newline-without-break-of-line ()
  "Create a new line without breaking the current line and move
the cursor down."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))


(defun xc/punch-timecard ()
  "Clock in or clock out.

Assumes a 'timecard.org' file exists with format:

    #+TITLE: Timecard
    #+AUTHOR: Excalamus

    DISPLAY CLOCK

    * Report
    #+BEGIN: clocktable :scope file :maxlevel 2 :block thisweek
    #+CAPTION: Clock summary at [2021-08-18 Wed 12:51], for week 2021-W33.
    | Headline       | Time    |      |
    |----------------+---------+------|
    | *Total time*   | *19:27* |      |
    |----------------+---------+------|
    | Timecard       | 19:27   |      |
    | \_  2021/08/18 |         | 3:59 |
    | \_  2021/08/17 |         | 8:27 |
    | \_  2021/08/16 |         | 7:01 |
    #+END:

    * Timecard

    * Local Variables
    # Local Variables:
    # eval: (defun xc/-button-pressed (&optional button) (interactive) (org-clock-display))
    # eval: (define-button-type 'display-clock-button 'follow-link t 'action #'org-clock-display)
    # eval: (make-button 45 58 :type 'display-clock-button)
    # eval: (setq org-duration-format 'h:mm)
    # End:
"

  (interactive)
  (if (not (featurep 'org-clock))
      (require 'org-clock))
  (if (not (get-buffer "timecard.org"))
      (progn
        (find-file "c:/Users/mtrzcinski/Documents/notes/timecard.org")
        (previous-buffer)))
  (with-current-buffer "timecard.org"
    (let* ((buffer-save-without-query t)
           (time-string "%Y/%m/%d")
           (system-time-locale "en_US")
           (todays-date (format-time-string time-string))
           (header-regexp (format "\\*\\* %s" todays-date)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward header-regexp nil t)
            ;; found
            (progn
              ;; clock in or out accordingly
              (if (org-clocking-p)
                  (progn
                    (org-clock-out)
                    (setq result (format "Clocked out at %s [%s]"
                                         (format-time-string "%-I:%M %p" (current-time))
                                         (org-duration-from-minutes (org-clock-get-clocked-time)))))
                (progn
                  (org-clock-in)
                    (setq result (format "Clocked in at %s [%s]"
                                         (format-time-string "%-I:%M %p" (current-time))
                                         (org-duration-from-minutes (org-clock-get-clocked-time))))))
              (save-buffer)
              (message "%s" result))
          ;; not found
          (progn
            ;; insert subtree with today's date and try punching in
            (goto-char (point-min))
            (re-search-forward "* Timecard")
            (insert (format "\n** %s" todays-date))
            (xc/punch-timecard)))))))


(defun xc/on-demand-window-set ()
  "Set the value of the on-demand window to current window."
  (interactive)
  (setq xc/on-demand-window (selected-window))
  ;; (setq xc/on-demand-buffer (current-buffer))
  (message "Set on-demand window to: %s" xc/on-demand-window))


(defun xc/on-demand-window-goto ()
  "Goto `xc/on-demand-window' with `xc/on-demand-buffer'."
  (interactive)
  (let ((win xc/on-demand-window))
    (unless win (error "No on-demand window set! See `xc/on-demand-window-set'."))
    (if (eq (selected-window) xc/on-demand-window)
        (error "Already in `xc/on-demand-window'"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))


(defun xc/open-file-browser (&optional file)
  "Open file explorer to directory containing FILE.

FILE may also be a directory."
  (interactive)
  (let* ((file (or (buffer-file-name (current-buffer)) default-directory))
         (dir (expand-file-name (file-name-directory file))))
    (if dir
        (progn
          (if (eq xc/device 'windows)
              (browse-url-of-file dir)
            (start-process "thunar" nil "/run/current-system/profile/bin/thunar" file)))
      (error "No directory to open"))))


(defun xc/open-terminal (&optional file)
  "Open external terminal in directory containing FILE.

FILE may also be a directory.

See URL `https://stackoverflow.com/a/13509208/5065796'"
  (interactive)
  (let* ((file (or (buffer-file-name (current-buffer)) default-directory))
         (dir (expand-file-name (file-name-directory file))))
    (cond ((eq xc/device 'windows)
           (let (;; create a cmd to create a cmd in desired directory
                 ;; /C Carries out the command specified by string and then stops.
                 ;; /K Carries out the command specified by string and continues.
                 ;; See URL `https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cmd'
                 (proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/K" "cd" dir)))
             (set-process-query-on-exit-flag proc nil)))
          (t (start-process "terminal" nil "/run/current-system/profile/bin/xfce4-terminal" (format "--working-directory=%s" dir))))))


(defun xc/org-babel-goto-tangle-file ()
  "Open tangle file associated with source block at point.

Taken from URL `https://www.reddit.com/r/emacs/comments/jof1p3/visit_tangled_file_with_orgopenatpoint/'
"
  (interactive)
  (if-let* ((args (nth 2 (org-babel-get-src-block-info t)))
            (tangle (alist-get :tangle args)))
      (when (not (equal "no" tangle))
        (ffap-other-window tangle)
        t)))


(defun xc/pop-buffer-into-frame (&optional arg)
  "Pop current buffer into its own frame.

With ARG (\\[universal-argument]) maximize frame."
  (interactive "P")
  (let ((win (display-buffer-pop-up-frame (current-buffer) nil)))
    (if (and arg win)
        (progn
          (select-frame (car (frame-list)))
          (toggle-frame-maximized) ))))


(defun xc/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.

See URL `http://steve.yegge.googlepages.com/my-dot-emacs-file'"
  (interactive "GNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun xc/delete-file-visiting ()
  "Trash the file currently being visited.

Prompt to kill active buffer."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (delete-file filename t)
        (cond ((not (file-exists-p filename))
               (message "Deleted %s" filename)
               (call-interactively 'kill-buffer))
              (message "Error deleting %s" filename))))))


(defun xc/send-line-or-region (&optional advance buff beg end)
  "Send region or line to BUFF.

If buffer has a process, insert and send line to the process. If
no process, then simply insert text at point.  Create a new line
when ADVANCE is non-nil.  Use current region if BEG and END not
provided.  If no region provided, send entire line.  Default BUFF
is the buffer associated with `xc/on-demand-window'."
  (interactive (if (use-region-p)
                   (list nil nil (region-beginning) (region-end))
                 (list nil nil nil nil)))
  (let* ((beg (or beg (if (use-region-p) (region-beginning)) nil))
         (end (or end (if (use-region-p) (region-end)) nil))
         (substr (string-trim
                  (or (and beg end (buffer-substring-no-properties beg end))
                      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
         (buff (or buff (window-buffer xc/on-demand-window)))
         (proc (get-buffer-process buff)))
    (if substr
        (with-selected-window (get-buffer-window buff t)
          (let ((window-point-insertion-type t))  ; advance marker on insert
            (cond (proc
                   (goto-char (process-mark proc))
                   (insert substr)
                   (comint-send-input nil t))
                  (t
                   (insert substr)
                   (if advance
                       (progn
                         (end-of-line)
                         (newline-and-indent)))))))
      (error "Invalid selection"))))


(defvar xc--send-string-history nil
  "History of strings sent via `xc/send-string'")

(defun xc/send-string (string &optional advance buff)
  "Send STRING to BUFF'.

Default BUFF is the buffer associated with
`xc/on-demand-window' (or current window if not set).  If BUFF
has an associated process, send region as input, otherwise just
insert the region.  Create a new line when ADVANCE is non-nil."
  (interactive
   (let* ((prompt (format "Send string to % s: " (window-buffer xc/on-demand-window)))
          (cmd (read-string prompt "" 'xc--on-demand-send-string-history)))
   (list cmd nil nil)))

  (let* ((buff (or buff (window-buffer xc/on-demand-window)))
         (proc (get-buffer-process buff)))
    (with-selected-window (get-buffer-window buff t)
      (let ((window-point-insertion-type t))  ; advance marker on insert
        (cond (proc
               (goto-char (process-mark proc))
               (insert string)
               (comint-send-input nil t))
              (t
               (insert string)
               (if advance
                   (progn
                     (end-of-line)
                     (newline-and-indent)))))))))


(defun xc/smart-beginning-of-line ()
  "Move point to first non-whitespace character or to the beginning of the line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line.

See URL `https://stackoverflow.com/a/145359'"
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


(defun xc/switch-to-last-window ()
  "Switch to most recently used window.

See URL `https://emacs.stackexchange.com/a/7411/15177'"
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))


(defun xc/suicide ()
  "Kill all Emacs processes."
  (interactive)
  (let ((cmd (if (eq system-type 'gnu/linux)
                 "killall -9 -r emacs" ; probably won't kill server administered by systemd
               "taskkill /f /fi \"IMAGENAME eq emacs.exe\" /fi \"MEMUSAGE gt 15000\"")))
    (shell-command cmd)))


(defun xc/toggle-plover ()
  "Toggle whether Plover is active."
  (interactive)
  (if xc/plover-enabled
      (progn
        (setq xc/plover-enabled nil)
        (message "Plover disabled"))
    (progn
      (setq xc/plover-enabled t)
      (message "Plover enabled"))))


(defun xc/toggle-comment-contiguous-lines ()
  "(Un)comment contiguous lines around point."
  (interactive)
  (let ((pos (point)))
    (mark-paragraph)
    (forward-line)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (goto-char pos)))


(defun xc/unfill-paragraph (&optional region)
  "Make multi-line paragraph into a single line of text.

REGION unfills the region.  See URL
`https://www.emacswiki.org/emacs/UnfillParagraph'"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(defun xc/yank-pop-forwards (arg)
  "Pop ARGth item off the kill ring.

See URL `https://web.archive.org/web/20151230143154/http://www.emacswiki.org/emacs/KillingAndYanking'"
  (interactive "p")
  (yank-pop (- arg)))


(defun xc/minimize-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (window-resize
   window
   (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
   nil nil window-resize-pixelwise))

(defun xc/1/4-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
   (- (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4))
    nil nil window-resize-pixelwise))

(defun xc/center-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 2)
    nil nil window-resize-pixelwise))

(defun xc/3/4-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4)
    nil nil window-resize-pixelwise))

(defun xc/maximize-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise))

(setq xc/last-window-op 'center)

(defun xc/recenter-window-top-bottom (&optional arg)
  (interactive "P")

  ;; ;; center-max-3/4-1/4-min
  ;; (cond ((eq xc/last-window-op 'center)
  ;;        (xc/maximize-window)
  ;;        (setq xc/last-window-op 'max))
  ;;       ((eq xc/last-window-op 'max)
  ;;        (xc/3/4-window)
  ;;        (setq xc/last-window-op 'three-quarter))
  ;;       ((eq xc/last-window-op 'three-quarter)
  ;;        (xc/1/4-window)
  ;;        (setq xc/last-window-op 'one-quarter))
  ;;       ((eq xc/last-window-op 'one-quarter)
  ;;        (xc/minimize-window)
  ;;        (setq xc/last-window-op 'min))
  ;;       ((eq xc/last-window-op 'min)
  ;;        (xc/center-window)
  ;;        (setq xc/last-window-op 'center))))

  ;; min-1/4-center-3/4-max
  (cond ((eq xc/last-window-op 'min)
          (xc/1/4-window)
          (setq xc/last-window-op 'one-quarter))
         ((eq xc/last-window-op 'one-quarter)
          (xc/center-window)
          (setq xc/last-window-op 'center))
         ((eq xc/last-window-op 'center)
          (xc/3/4-window)
          (setq xc/last-window-op 'three-quarter))
         ((eq xc/last-window-op 'three-quarter)
          (xc/maximize-window)
          (setq xc/last-window-op 'max))
         ((eq xc/last-window-op 'max)
          (xc/minimize-window)
          (setq xc/last-window-op 'min))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension-ledger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xc/ledger-kill-current-transaction (pos)
  "Kill transaction surrounding POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (kill-region (car bounds) (cadr bounds))
    (message "Killed current transaction")))


(defun xc/ledger-kill-ring-save-current-transaction (pos)
  "Save transaction surrounding POS to kill ring without
killing."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (kill-ring-save (car bounds) (cadr bounds))
    (message "Placed on kill ring")))

(defun xc/balance-at-point ()
  "Get balance of account at point"
  (interactive)
  (let* ((account (ledger-context-field-value (ledger-context-at-point) 'account))
         (buffer (find-file-noselect (ledger-master-file)))
         (balance (with-temp-buffer
                    (apply 'ledger-exec-ledger buffer (current-buffer) "cleared" account nil)
                    (if (> (buffer-size) 0)
                        (buffer-substring-no-properties (point-min) (1- (point-max)))
                      (concat account " is empty.")))))
    (when balance
      (message balance))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension-python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xc/convert-slashes (&optional beg end)
  "Convert backslashes to forward slashes.

Only convert within region defined by BEG and END.  Use current
line if no region is provided."
  (interactive)
  (let* ((beg (or beg (if (use-region-p) (region-beginning)) (line-beginning-position)))
         (end (or end (if (use-region-p) (region-end)) (line-end-position))))
    (subst-char-in-region beg end ?\\ ?/)
    (replace-string "//" "/" nil beg end)))


(defvar xc/kill-python-p t
  "Will Python be killed?")

(if (eq xc/device 'gnu/linux)
    (setq xc/kill-python-p nil))


(defun xc/toggle-kill-python ()
  (interactive)
  (if xc/kill-python-p
      (progn
        (setq xc/kill-python-p nil)
        (message "Python will be spared"))
    (progn
      (setq xc/kill-python-p t)
      (message "Python will be killed henceforth"))))


(defun xc/conda-activate ()
  "Activate conda venv."
  (interactive)
  (insert "C:\\Users\\mtrzcinski\\Anaconda3\\condabin\\conda.bat activate "))


(defun xc/mamba-activate ()
  "Activate mamba venv."
  (interactive)
  (insert "C:\\python\\miniconda38\\condabin\\mamba.bat activate "))


;; (setq xc/python-break-string "import ipdb; ipdb.set_trace(context=10)")
;; (setq xc/python-break-string "import mydebugger; mydebugger.breakpoint()")
;; (setq xc/python-break-string "import my_other_debugger; my_other_debugger.breakpoint()")
(setq xc/python-break-string "import pydevd_pycharm; pydevd_pycharm.settrace('localhost', port=53100, stdoutToServer=True, stderrToServer=True)")

(defun xc/insert-breakpoint (&optional string)
  (interactive)
  (let ((breakpoint (or string string xc/python-break-string)))
    (xc/newline-without-break-of-line)
    (insert breakpoint)
    (bm-toggle)
    (save-buffer)))


(defun xc/kill-proc-child (&optional buffer-name)
  "Kill any child process associated with BUFFER-NAME."
  (interactive)
  (let* ((proc-buffer (or proc-buffer "*shell*"))
         (proc (get-buffer-process proc-buffer))
         (shell-pid (if proc (process-id proc)))
         (child-pid (if shell-pid (car (split-string
                                        (shell-command-to-string (format "pgrep --parent %d" shell-pid))))))
         rv)
    (if child-pid
        (setq rv (shell-command (format "kill -9 %s" child-pid)))
      ;; (message "No child process to kill!")
      )
    (if rv
        (if (> 0 rv) (message "Process could not be killed: %s" rv)
          ;; (message "Process killed")
          ))))

;; 16000
(defun xc/kill-python ()
  "Kill Python.

Note: This kills indiscriminantly on Windows systems.  It will
kill any system process, like the AWS CLI, that runs on the
Python interpetor."
  (interactive)
  (if (eq xc/device 'windows)
      (shell-command "taskkill /f /fi \"IMAGENAME eq python.exe\" /fi \"MEMUSAGE gt 15000\"")
    (xc/kill-proc-child peut-gerer-shell)))


(defun xc/pyside-lookup (&optional arg)
  "Lookup symbol at point in PySide2 online documentation.

Tries to lookup symbol in QWidget documentation.

When called with universal prefix, prompt for module.  This
requires list of modules (provided in `pyside-modules.el').  When
called with negative prefix, search within the online PySide
documentation.

\(fn)"
  (interactive "p")
  (let* ((sym (thing-at-point 'symbol))
         (direct-url (concat
                      "https://doc-snapshots.qt.io/qtforpython-5.15/PySide2/QtWidgets/"
                      sym
                      ".html"
                      ))
         (search-url (concat
                      "https://doc-snapshots.qt.io/qtforpython-5.15/search.html?check_keywords=yes&area=default&q="
                      sym
                      )))
    (cond ((eql arg 1) ; no prefix
           (let ((buff (get-buffer-window "*eww*")))
             (if buff
                 (with-selected-window buff
                   (eww direct-url))
               (eww direct-url))))
          ((eql arg 4)  ; "C-u", expand search to be "universal"
           (let* ((buff (get-buffer-window "*eww*"))
                  (completion-ignore-case t)
                  (module (completing-read "Select module: " xc/pyside-modules nil 'confirm "Qt"))
                  (direct-url (concat
                               "https://doc-snapshots.qt.io/qtforpython-5.15/PySide2/"
                               module "/"
                               (thing-at-point 'symbol)
                               ".html")))
             (if buff
                 (with-selected-window buff
                   (eww direct-url))
               (eww direct-url))))
          ((eql arg -1)  ; "C--", it's 'negative' to have to leave Emacs
           (browse-url-default-browser search-url))
          (t (error "Invalid prefix")))))


(defun xc/python-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.
Also, switch to that buffer.

See URL `https://github.com/jorgenschaefer/elpy/blob/c31cd91325595573c489b92ad58e492a839d2dec/elpy.el#L2556'
"
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))


(defun xc/jump-to-file-from-python-error ()
  "Jump to line in file specified by a Python traceback."
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         file
         number)

    (string-match "^ *File \\(\"?\\)\\([^,\" \n    <>]+\\)\\1, lines? \\([0-9]+\\)-?\\([0-9]+\\)?" line)

    (setq file (match-string 2 line))
    (setq number (match-string 3 line)) ; match is string, not numeric

    (cond ((and file number)
           (find-file-other-window file)
           (with-current-buffer (get-buffer (file-name-nondirectory file))
             (goto-char (point-min))
             (forward-line (1- (string-to-number number))))))))


(defun xc/statement-to-function (&optional statement func beg end)
  "Convert STATEMENT to FUNC.

For use with statements in Python such as 'print'.  Converts
statements like,

  print \"Hello, world!\"

to a function like,

  print(\"Hello, world\")

Also works generally so that the STATEMENT can be changed to any
FUNC.  For instance, a 'print' statement,

  print \"Hello, world!\"

could be changed to a function,

  banana(\"Hello, world!\")

Default STATEMENT is 'print'.  Default FUNC is
STATEMENT (e.g. 'print').  Prompt for STATEMENT and FUNC when
called with universal prefix, `C-u'.

If region is selected interactively, only perform conversion
within the region.  The same can be achieved programmatically
using BEG and END of the desired region."
  (interactive "p")
  (let* ((arg statement)  ; statement argument overwritten, so preserve value
         ;; only prompt on universal prefix; 1 means no universal, 4 means universal
         (statement (cond ((eql arg 1) "print")  ; no prefix
                          ((eql arg 4) (read-string "Statement (print): " "" nil "print")))) ; C-u
         (func (cond ((eql arg 1) statement)  ; no prefix
                     ((eql arg 4) (read-string (concat "Function " "(" statement "): ") "" nil statement)))) ; C-u
         ;; [[:space:]]*  -- allow 0 or more spaces
         ;; \\(\"\\|\'\\) -- match single or double quotes
         ;; \\(\\)        -- define capture group for statement expression; recalled with \\2
         (regexp (concat statement "[[:space:]]*\\(\"\\|\'\\)\\(.*?\\)\\(\"\\|'\\)"))
         ;; replace statement with function and place statement expression within parentheses \(\)
         (replace (concat func "\(\"\\2\"\)"))
         ;; set start of replacement region to what the user passed in
         ;; (if used programmatically), beginning of region (if used
         ;; interactively with a region selected), or the start of the
         ;; buffer (if no region specified)
         (beg (or beg (if (use-region-p) (region-beginning)) (point-min)))
         ;; end defines the `bound' of the regexp search. According to
         ;; documentation, "the match found must not end after this."
         ;; Extend the end of the region slightly to compensate.
         ;; While this may technically introduce an unwanted match, it
         ;; is unlikely for any but the shortest statements (probably
         ;; less than 2 or 3 characters).  Doing this at the end of a
         ;; buffer sometimes results in a "while: Invalid search bound
         ;; (wrong side of point)" error.  It may be an Emacs
         ;; bug. Regardless, it does not appear to affect the results
         ;; which are otherwise as expected. A value of nil means
         ;; search to the end of the accessible portion of the buffer.
         (end (or end (if (use-region-p) (+ (region-end) 3) nil))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (replace-match replace nil t)))))


(defun xc/spam-filter (string)
  "Filter stupid comint spam."
  (with-current-buffer (current-buffer)
    (mark-whole-buffer)
    (flush-lines "has no notify signal and is not constant")))


(defun xc/toggle-spam-filter ()
  "Toggle spam filter"
  (interactive)
  (if (member 'xc/spam-filter comint-output-filter-functions)
      (progn
        (setq comint-output-filter-functions
              (delete 'xc/spam-filter comint-output-filter-functions))
        (message "Spam filter off"))
    (progn
      (add-hook 'comint-output-filter-functions 'xc/spam-filter)
      (message "Spam filter on"))))


(defun xc/venv-activate ()
  "Activate venv."
  (interactive)
  (insert "venv\\Scripts\\activate"))


(defun xc/venv-create ()
  "Create Python venv.

I don't keep Python on my path.  Unfortunately, the autocomplete
in shell.el pulls completions from other buffers, creating a
chicken and egg problem."
  (interactive)
  (insert "\"C:\\python\\python37\\python.exe\" -m venv venv"))

(defun xc/qt-live-code ()
  "Call ipython interactively with live-code toggle."
  (interactive)
  (insert "ipython -i -- qt_live_code.py live"))

(defun xc/run-python-with-qt-live-code ()
  (interactive)
  (let ((current-prefix-arg '(4))
       (python-shell-interpreter-args "-i -- qt_live_code.py live"))
    (call-interactively 'run-python )))

(defun xc/toggle-build-debug ()
  (interactive)
  (insert "set BUILD_DEBUG="))

(defun xc/kill-qgis ()
  (interactive)
  (shell-command "taskkill /f /fi \"IMAGENAME eq qgis-ltr-bin.exe\""))

(defun xc/run-qgis ()
  (interactive)
  (save-some-buffers t nil)
  (xc/kill-qgis)
  (shell-command "taskkill /f /t /fi \"WINDOWTITLE eq \\qgis\\ \"")
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "\"qgis\"" "cmd.exe" "/K" "C:\\Program Files\\QGIS 3.10\\bin\\qgis-ltr.bat")))
    (set-process-query-on-exit-flag proc nil))
  ;; assume qgis loads in X seconds
  (run-at-time "3 sec" nil #'(lambda () (progn (shell-command "taskkill /f /fi \"WINDOWTITLE eq \\qgis\\ \"")))))
