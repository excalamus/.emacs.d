;;; init.el -- Principium de Excalamus

;; Author: Matt Trzcinski <excalamus@tutanota.com>
;; URL: https://github.com/excalamus/.emacs.d.git
;; Requires: ((emacs "26.1"))

;; Maintain package consistency across multiple devices using
;; straight.el with use-package.el.  Fork packages and point
;; straight.el to personal repos.  The forks help with submitting pull
;; requests and provides another point of consistency.
;;
;; SSH (e.g. ssh-agent) is hard to get working with Emacs.  To get
;; authorization during first run, call one of:
;;
;;     git config --global credential.helper cache
;;     git config --global credential.helper wincred
;;
;; Use 'cache' for GNU/Linux, 'wincred' for Windows.  This will
;; temporarily cache credentials for https connections.  See URL:
;; `https://docs.github.com/en/free-pro-team@latest/github/using-git/caching-your-github-credentials-in-git'

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
  :config (key-chord-mode 1))

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
        ((file-directory-p "/data/data/com.termux/") 'terminal))
  "Current device.

Either 'windows, 'gnu/linux, or 'terminal.

`system-type' doesn't differentiate X from terminal.
`window-system' gets assigned after init loads.")

(defvar xc/on-demand-window nil
  "Target on-demand window.

An on-demand window is one which you wish to return to within the
current Emacs session but whose importance doesn't warrant a
permanent binding.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prevent custom from writing to init; have it write to a dump file
;; that never gets loaded or read
(setq custom-file "~/.emacs.d/custom-set.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(setq-default abbrev-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
(put 'narrow-to-region 'disabled nil)  ; enabling disables confirmation prompt
(setq initial-scratch-message nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq initial-major-mode 'emacs-lisp-mode)
;; (setq help-window-select t)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq show-help-function nil)
(set-default 'truncate-lines t)

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

(add-hook 'before-save-hook 'whitespace-cleanup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-time)

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

;; setting true causes rev in vc-git.el:362 to be nil, causing error in substring
;; (setq debug-on-error t)
;; (setq auto-revert-check-vc-info t)

;; Automatically reload files that have changed on disk
(global-auto-revert-mode 1)

;; Remove Git prefix from vc since only using git
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git" " " vc-mode))))

;; make titlebar the filename
;; https://emacs.stackexchange.com/a/16836
(setq-default frame-title-format '("%f"))

;; Theme advice approach modified from
;; https://www.greghendershott.com/2017/02/emacs-themes.html
(use-package base16-theme
  :straight (:fork "excalamus/base16-emacs"))
(use-package zenburn-theme
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

(setq xc/theme-dark 'zenburn)
(setq xc/theme-light 'base16-tomorrow)

;; Add to hook to reload these automatically
(defun xc/dark-theme-hook ()
  "Run after loading dark theme."
  ;; zenburn
  (if (eq xc/device 'terminal)
      (set-face-attribute 'mode-line-inactive nil :background "color-236"))
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (setq evil-insert-state-cursor '("gray" bar))
  (set-face-attribute 'hl-line nil :background "gray29" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "gray40")
  (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified)
  (set-face-attribute 'xc/hi-comint nil :background "dim gray"))

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


(use-package yasnippet
  :straight (:fork "excalamus/yasnippet")
  :config
  (yas-global-mode)

  (if xc/debug (message "yasnippet")))


;; Way more packages use markdown-mode than you might expect.  When
;; put in alphabetical order, one of these other packages builds it
;; first, throwing an error about two build recipes.  See URL
;; `https://github.com/raxod502/straight.el/issues/518'
(use-package markdown-mode
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


(use-package ace-window
  :straight (:fork "excalamus/ace-window")
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)

  (if xc/debug (message "ace-window")))


(use-package ag
  :straight (:fork "excalamus/ag.el")
  :config

  (if xc/debug (message "ag.el")))


(use-package bm
  :straight (:fork "excalamus/bm")
  :init
  :config
  (setq bm-cycle-all-buffers t)

  (if xc/debug (message "bm")))


(use-package comment-dwim-2
  :after (:all org)
  :straight (:fork "excalamus/comment-dwim-2")
  :config

  (if xc/debug (message "comment-dwim-2")))


(use-package define-word
  :straight (:fork "excalamus/define-word")
  :config

  (if xc/debug (message "define-word")))


(use-package dap-mode
  :straight (:fork "excalamus/dap-mode")
  :after (:all markdown-mode)
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (if xc/debug (message "dap-mode")))


(use-package dumb-jump
  :straight (:fork "excalamus/dumb-jump")
  :config

  (if xc/debug (message "dumb-jump")))


(use-package general
  :straight (:fork "excalamus/general.el")
  :config
  (require 'ffap)

  (general-after-init

    ;; Disable stupid minimize hotkeys
    (general-unbind
      "C-z"
      "C-x C-z")

    (general-define-key
     :keymaps 'key-translation-map
     ;; "M-x" "M-q"  ; dvp
     ;; "M-q" "M-x"  ; dvp
     "<next>" "<tab>"
     "<prior>" "<escape>"
     )

    (if (eq xc/device 'windows)
        (general-def
          :keymaps 'override
          :prefix "C-x i"
          "b" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/brag.org"))
          "g" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/glossary.org"))
          "n" '(lambda () (interactive) (find-file "C:/Users/mtrzcinski/Documents/notes/notes.org"))
          )
      (general-def
        :keymaps 'override
        :prefix "C-x i"
        "n" '(lambda () (interactive) (find-file "~/Documents/notes.org"))
        )
      )

    (general-def
      :keymaps 'override
      :prefix "C-x i"
      "a" '(lambda () (interactive) (find-file "~/.emacs.d/archive/andr-init.el"))
      "i" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "c" '(lambda () (interactive) (find-file "~/.emacs.d/archive/classic-init.el"))
      "e" '(lambda () (interactive) (find-file "~/.emacs.d/experimental.el"))
      "p" '(lambda () (interactive) (find-file "~/peut-gerer-projects.el"))  ; %APPDATA% on Windows
      )

    (general-def
      :keymaps 'override
      "<vertical-line> <mouse-3>" 'balance-windows  ; right-click on vertical to balance-windows
      "C-x s" 'save-buffer
      "<f5>" '(lambda () (interactive) (progn (funcall 'xc/send-line-or-region)))
      "<f8>" 'xc/switch-to-last-window
      "C-<f8>" '(lambda () (interactive) (peut-gerer-switch-to :main t 0))
      "S-<f8>" '(lambda () (interactive) (peut-gerer-switch-to :shell t 0))
      "M-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-select-project))
      "C-S-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-create-shell))
      "C-M-<f8>" '(lambda () (interactive) (call-interactively 'peut-gerer-activate-project))
      "M-c" 'xc/copy-symbol-at-point
      "M-j" 'helm-semantic-or-imenu ; navigate the file's structure (functions or otherwise)
      "M-y" 'xc/yank-pop-forwards  ; todo but p is not yank... (use C-p for evil-paste-pop)
      "C-M-y" 'helm-show-kill-ring
      "C-M-j" 'helm-swoop  ; swoop (S)pecific thing (at point)
      "C-j" 'helm-swoop-without-pre-input ; enter navigate-state
      "<f2>" 'bm-common-next
      "S-<f2>" 'bm-common-previous
      "C-<f2>" 'bm-toggle
      ;; can use to create new *shell* after load
      "<f10>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (xc/kill-python)  ; kills aws cli commands
                 (peut-gerer-send-command peut-gerer-command))
      "C-<f10>" '(lambda () (interactive) (call-interactively 'peut-gerer-send-command))
      "C-h j" 'describe-face  ; introspect colors
      "C-h C-w" 'define-word-at-point
      "C-x a d" 'xc/define-abbrev
      "C-x b" 'helm-buffers-list
      "C-x g" 'magit-status
      "C-x o" 'ace-window
      "<f1>" '(lambda ()
                (interactive)
                (if xc/on-demand-window
                    (call-interactively 'xc/on-demand-window-goto)
                  (call-interactively 'ace-window)))
      "S-<f1>" 'xc/on-demand-window-set
      "C-<f1>" 'ace-window
      )

    (general-def
      :keymaps 'override
      :states '(normal insert emacs)
      (general-chord "jk") 'xc/newline-without-break-of-line
      (general-chord "hh") 'evil-emacs-state
      (general-chord "jj") 'evil-normal-state
      "C-;" 'comment-dwim-2
      "<f9>" 'save-buffer
      "\M-Q" 'xc/unfill-paragraph
      )

    (general-def
      :states '(normal)
      ;; "C-o" 'dumb-jump-back  ; obsoleted
      ;; "C-o" 'xref-pop-marker-stack
      "C-o" 'evil-jump-backward
      "C-i" 'evil-jump-forward
      )

    (general-def
      :states '(normal insert emacs)

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
      )

    (general-def
      :states '(normal visual)
      :prefix "SPC"
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
      "q" 'sx-search
      "s" 'save-buffer
      "t" 'xc/open-terminal
      "x" 'eval-expression
      )

    (general-define-key
     :keymaps 'comint-mode-map
     :states '(insert emacs)
     "-" #'(lambda () (interactive) (insert "_"))
     "_" #'(lambda () (interactive) (insert "-"))
     )
    (general-def
      :keymaps 'comint-mode-map
      "C-l" 'comint-clear-buffer
      "C-x C-l" 'recenter-top-bottom
      "C-r" 'comint-history-isearch-backward
      "<apps>" 'xc/kill-python
      )

    (general-define-key
     :keymaps 'elpy-mode-map
     :states '(insert emacs)
     "-" #'(lambda () (interactive) (insert "_"))
     "_" #'(lambda () (interactive) (insert "-"))
     )
    (general-def
      :keymaps 'elpy-mode-map
      "<C-return>" 'nil
      "<C-S-return>" 'nil
      )
    (general-def
      :keymaps 'elpy-mode-map
      "<C-S-return>"'elpy-shell-send-statement
      "<C-return>" 'elpy-shell-send-statement-and-step
      "<M-return>" 'elpy-shell-send-group
      "<M-S-return>" 'elpy-shell-send-group-and-step
      "<pause>" 'elpy-test-pytest-runner
      ;; "<insert>" 'elpy-test-pytest-runner
      "C-c o" 'elpy-occur-definitions
      "<apps>" 'xc/kill-python
      "<f6>" 'xc/insert-breakpoint
      "<C-S-f10>" 'peut-gerer-set-command-to-current-file
      "<S-f10>" 'peut-gerer-buffer-file-to-shell
      )
    (general-def
      :keymaps 'elpy-mode-map
      :states 'normal
      :prefix "SPC"
      "d" 'elpy-occur-definitions
      "c" 'xc/string-inflection-style-cycle
      "u" 'xc/pyside-lookup
      )

    (general-def
      :keymaps 'emacs-lisp-mode-map
      :states 'normal
      :prefix "SPC"
      "e" 'eval-last-sexp
      )

    (general-def
      :keymaps 'emacs-lisp-mode-map
      "C-<next>" 'forward-page  ; C-PgUp goto previous linebreak
      "C-<prior>" 'backward-page ; C-PgDown goto next linebreak
      )

    (general-def  ; won't work in terminal bc of how terminals work
      :keymaps 'evil-emacs-state-map
      "<escape>" 'evil-normal-state
      )

    (general-def :keymaps 'helm-map "<escape>"  'helm-keyboard-quit)

    ;; How to generate Info docs for Python (and others)!
    ;; https://stackoverflow.com/a/65100142/5065796
    (general-def
      :keymaps 'Info-mode-map
      "a" 'info-apropos
      "G" '(lambda () (interactive) (xc/Info-current-node-to-url 4))
      "P" '(lambda () (interactive) (Info-goto-node "(python)"))
      "U" 'xc/Info-current-node-to-url
      )

    (general-def
      :keymaps 'ledger-mode-map
      :states 'normal
      :prefix "SPC"
      ;; "n" 'ledger-display-balance-at-point
      "n" 'xc/balance-at-point
      "r" 'ledger-report
      "a" 'ledger-post-align-dwim
      "t" 'ledger-add-transaction
      "c" 'ledger-fully-complete-xact
      ";" 'xc/toggle-comment-contiguous-lines
      "k" 'xc/ledger-kill-current-transaction
      )

    (general-def
      :keymaps 'magit-status-mode-map
      "S-<tab>" 'magit-section-cycle-global
      "<tab>" 'magit-section-toggle
      )

    ;; Disable mouse click on minibuffer from opening messages
    (general-def :keymaps 'minibuffer-inactive-mode-map [mouse-1] nil)

    (general-def
      :keymaps 'org-mode-map
      :states 'normal
      :prefix "SPC"
      "SPC" 'org-ctrl-c-ctrl-c
      "t" 'org-insert-structure-template
      )

    (general-def
      :keymaps 'org-mode-map
      "C-c h o" 'org-clock-out
      "C-c h i" 'org-clock-in
      "C-c h d" 'org-clock-display
      "C-c C--" 'org-ctrl-c-minus
      )

    )

  (if xc/debug (message "general.el")))


(use-package elpy
  :straight (:fork "excalamus/elpy")
  :after (:all helm key-chord use-package-chords)
  :init
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'elpy-mode-hook #'hs-minor-mode)
  (add-hook 'elpy-mode-hook (lambda () (company-mode -1)))
  :config
  (pyvenv-mode 1)
  (elpy-enable)

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


(use-package ess
  :straight (:fork "excalamus/ess")
  :init (require 'ess-site)
  :config

  (if xc/debug (message "ess")))


(use-package evil
  :straight (:fork "excalamus/evil")
  ;; :after (:all dumb-jump key-chord nov sx)
  :after (:all dumb-jump key-chord sx)
  :config
  (evil-mode)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  ;; (evil-set-initial-state 'nov-mode 'emacs)
  (evil-set-initial-state 'sx-mode 'emacs)
  (evil-set-initial-state 'sx-question-mode 'emacs)
  (evil-set-initial-state 'sx-question-list-mode 'emacs)

  ;; https://github.com/emacs-evil/evil/issues/1074
  (setq evil-undo-system 'undo-redo)

  ;; Coordinate states with cursor color
  (if (not (eq xc/device 'terminal))
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

  (if xc/debug (message "evil")))


(use-package evil-lion
  :straight (:fork "excalamus/evil-lion")
  :after (:all evil)
  :config
  (evil-lion-mode 1)

  (if xc/debug (message "evil-lion")))


(use-package evil-surround
  :straight (:fork "excalamus/evil-surround")
  :after (:all evil)
  :config
  (global-evil-surround-mode 1)

  (if xc/debug (message "evil-surround")))


(use-package expand-region
  :straight (:fork "excalamus/expand-region.el")
  :config

  (if xc/debug (message "expand-region.el")))


(use-package flycheck
  :straight (:fork "excalamus/flycheck")
  :config

  (if xc/debug (message "flycheck")))


(use-package free-keys
  :straight (:fork "excalamus/free-keys")
  :config

  (if xc/debug (message "free-keys")))


(use-package git-timemachine
  :straight (:host github :repo "excalamus/git-timemachine")
  :config

  (if xc/debug (message "git-timemachine")))


(use-package helm
  :straight (:fork "excalamus/helm")
  :config

  (if xc/debug (message "helm")))


(use-package helm-swoop
  :straight (:fork "excalamus/helm-swoop")
  :after (:all helm)
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


(use-package iedit
  :straight (:fork "excalamus/iedit")
  :config

  (if xc/debug (message "iedit")))


(use-package htmlize
  :straight (:fork "excalamus/emacs-htmlize")
  :config

  (if xc/debug (message "emacs-htmlize")))

;; on windows, you need to install Hunspell
;; https://sourceforge.net/projects/ezwinports/files/
;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/EZWinPorts.html
(use-package ispell
  :if (eq system-type 'windows-nt)
  :config
  (setq ispell-program-name "C:/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))


(use-package ledger-mode
  :straight (:fork "excalamus/ledger-mode")
  :defer t
  :config
  (setq ledger-post-amount-alignment-column 60)
  (setq ledger-report-auto-refresh-sticky-cursor t)

  (if xc/debug (message "ledger-mode")))


(use-package magit
  :straight (:fork "excalamus/magit")
  :init
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide)))
  :config
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (if xc/debug (message "magit")))


(use-package markdown-toc
  :after (:all markdown-mode)
  :straight (:fork "excalamus/markdown-toc")
  :config

  (if xc/debug (message "markdown-toc")))


(use-package nameless
  :straight (:fork "excalamus/nameless")
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config

  (if xc/debug (message "nameless")))


;; This step works some magic.  Not even going to attempt building
;; from a fork. For details, see URL
;; `https://github.com/raxod502/straight.el#integration-with-org'
(use-package org
  :after (:all helm helm-swoop)
  :init
  (add-to-list 'load-path
               (expand-file-name
                (concat
                 straight-base-dir
                 "straight/repos/org/contrib/lisp/")))
  :config
  (require 'ox-texinfo)
  (require 'ox-md)
  (require 'ox-confluence)  ;; in contrib/lisp/

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
           "TODO"		  ; todo, not active
           "CURRENT"		  ; todo, active item
           "PENDING"		  ; requires more information (timely)
           "|"	; entries after pipe are considered completed in [%] and [/]
           ;; closed items
           "DONE"	 ; completed successfully
           "ON-HOLD"	 ; requires more information (indefinite time)
           "CANCELED"	 ; no longer relevant, not completed
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
  ;;
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (emacs-lisp . t)
     (ledger . t)
     (latex . t)
     ))

  (defun xc/new-clock-task ()
    "Switch to new task by clocking in after clocking out."
    (interactive)
    (org-clock-out)
    (org-clock-in))

  ;; org-mode doesn't automatically save archive files for some
  ;; reason.  This is a ruthless hack which saves /all/ org buffers in
  ;; archive.  https://emacs.stackexchange.com/a/51112/15177
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)

  (if xc/debug (message "org")))


(use-package peut-publier
  :straight (:repo "https://github.com/excalamus/peut-publier.git")
  :config

  (if xc/debug (message "peut-publier")))

;; 
(use-package peut-gerer
  :straight (:repo "https://github.com/excalamus/peut-gerer.git" :branch "main")
  :after (:all right-click-context)
  :config
  (if (eq system-type 'windows-nt)
      (load "~/peut-gerer-projects.el"))

  (setq peut-gerer-after-activate-functions '(pyvenv-activate))

  (setq peut-gerer-after-select-functions
        '((lambda (x) (funcall 'pyvenv-deactivate))
          pyvenv-activate))

  (add-to-list 'right-click-context-global-menu-tree
               '("Send region to shell"
                 :call (peut-gerer-send-region)
                 :if
                 (and (use-region-p)
                      ;; only disables to peut-gerer registered shells
                      (not
                       (member (string-trim (buffer-name) "*" "*")
                               peut-gerer--active-projects-alist)))))

  ;; quick hack; create an xc/on-demand-window (C-<f1>), then you can
  ;; select a region anywhere and send that region to the odw.  For
  ;; use in exploratory debugging.  Try stuff in the repl, then send
  ;; that to the script.
  (add-to-list 'right-click-context-global-menu-tree
               '("Send region to on-demand-window"
                 :call (xc/send-line-or-region)))

  (if xc/debug (message "peut-gerer")))


(use-package right-click-context
  :straight (:fork "excalamus/right-click-context")
  :config
  (right-click-context-mode 1)

  (if xc/debug (message "right-click-context")))


(use-package rg
  :straight (:fork "excalamus/rg.el")
  :config

  (if xc/debug (message "rg.el")))


;; skeeto fork
(use-package simple-httpd
  :straight (:fork "excalamus/emacs-web-server")
  :config

  (if xc/debug (message "emacs-web-server")))


(use-package smartparens
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


(use-package sx
  :straight (:fork "excalamus/sx.el")
  :config

  (setq sx-default-site 'stackoverflow)
  (if xc/debug (message "sx.el")))


(use-package web-mode
  :straight (:fork "excalamus/web-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (if xc/debug (message "web-mode")))


(use-package yaml-mode
  :straight (:fork "excalamus/yaml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (if xc/debug (message "yaml-mode")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(defun xc/emacs-standalone (&optional arg)
  "Start standalone instance of Emacs."
  (interactive "p")
  (cond ((eql arg 1)
         (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe")))
        ((eql arg 4)
         (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe" "-q")))
        (t (error "Invalid arg")))
  (set-process-query-on-exit-flag proc nil))


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


(defun xc/jira-issue (&optional issue)
  "Open Jira ISSUE.

If no issue is given, check for one at point."
  (interactive)
  (let* ((issue (or issue (thing-at-point 'symbol t)))
         (url (concat "https://ushrauto.atlassian.net/browse/" issue)))
    (if issue
        (browse-url-default-windows-browser url)
      (error "No directory to open"))))


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


(defun xc/newline-without-break-of-line ()
  "Create a new line without breaking the current line and move
the cursor down."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))


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
        (browse-url-of-file dir)
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
          (t (error "Unable to open terminal")))))


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
  (interactive "sNew name: ")
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


(defun xc/send-line-or-region (&optional beg end buff)
  "Send region defined by BEG and END to BUFF.

Use current region if BEG and END not provided.  If no region
provided, send entire line.  Default BUFF is that displayed in
`xc/on-demand-window'."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list nil nil nil)))
  (let* ((beg (or beg (if (use-region-p) (region-beginning)) nil))
         (end (or end (if (use-region-p) (region-end)) nil))
         (substr (string-trim
                  (or (and beg end (buffer-substring-no-properties beg end))
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
         (buff (or buff (window-buffer xc/on-demand-window))))
    (if substr
        (with-selected-window xc/on-demand-window
          (setq-local window-point-insertion-type t)
          (insert substr)
          (end-of-line)
          (newline-and-indent))
      (error "Invalid selection"))))


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
                 "killall -9 emacs" ; probably won't kill server administered by systemd
               "taskkill /f /fi \"IMAGENAME eq emacs.exe\" /fi \"MEMUSAGE gt 15000\"")))
    (shell-command cmd)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension-ledger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xc/ledger-kill-current-transaction (pos)
  "Kill transaction surrounding POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (kill-region (car bounds) (cadr bounds))))


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


(defun xc/conda-activate ()
  "Activate conda venv."
  (interactive)
  (insert "C:\\Users\\mtrzcinski\\Anaconda3\\condabin\\conda.bat activate "))


(setq xc/python-break-string "import ipdb; ipdb.set_trace(context=10)")

(defun xc/insert-breakpoint ()
  (interactive)
  (xc/newline-without-break-of-line)
  (insert xc/python-break-string)
  (bm-toggle)
  (save-buffer))


;; 16000
(defun xc/kill-python ()
  "Kill Python.

Note: This kills indiscriminantly.  It will kill any system
process, like the AWS CLI, that runs on the Python interpetor."
  (interactive)
  (if (eq system-type 'windows-nt)
      (shell-command "taskkill /f /fi \"IMAGENAME eq python.exe\" /fi \"MEMUSAGE gt 15000\"")))


(defun xc/pyside-lookup (&optional arg)
  "Lookup symbol at point in PySide online documentation.

When called with prefix, search within the online PySide
documentation.

\(fn)"
  (interactive "p")
  (let* (
         ;; (prev-sym (buffer-substring-no-properties
         ;;            (save-excursion (forward-word (- 2)) (point))
         ;;            (save-excursion (forward-word (- 1)) (point))
                    ;; ))
         (sym (thing-at-point 'symbol))
         (direct-url (concat
                      "https://doc-snapshots.qt.io/qtforpython-5.15/PySide2/QtWidgets/"
                      sym
                      ".html"
                      ))
         (search-url (concat
                      "https://doc-snapshots.qt.io/qtforpython-5.15/search.html?check_keywords=yes&area=default&q="
                      sym
                      )))
    (cond ((eql arg 1)
           (let ((buff (get-buffer-window "*eww*")))
             (if buff
                 (with-selected-window buff
                   (eww direct-url))
               (eww direct-url))))
          ((eql arg 4)
           (browse-url-default-browser search-url))
          (t (error "Invalid prefix")))))


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
