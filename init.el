;;; init.el -- Principium de Excalamus

;; Maintain package consistency across multiple devices using
;; straight.el with use-package.el.

(defvar my-debug nil
  "Toggle package load messaging.")

  ; C-q C-l
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(defvar my--backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p my--backup-directory))
        (make-directory my--backup-directory t))
(setq backup-directory-alist `(("." . ,my--backup-directory))) ; put backups in current dir and in my--backup-directory
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

(set-language-environment "UTF-8")
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

;; Remove Git prefix from vc since only using git
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git" " " vc-mode))))

;; make titlebar the filename
;; https://emacs.stackexchange.com/a/16836
(setq-default frame-title-format '("%f"))

;; Theme advice approach modified from
;; https://www.greghendershott.com/2017/02/emacs-themes.html
(use-package base16-theme)
(use-package zenburn-theme)

(defun my-disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar my-theme-hooks nil
  "((theme-id . function) ...)")

(defun my-add-theme-hook (theme-id hook-func)
  (add-to-list 'my-theme-hooks (cons theme-id hook-func)))

(defun my-load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `my-add-theme-hook'."
  (unless no-enable
    (my-disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id my-theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'my-load-theme-advice)

(defvar my-theme-dark nil
  "My dark theme.")

(defvar my-theme-light nil
  "My light theme.")

(setq my-theme-dark 'zenburn)
(setq my-theme-light 'base16-tomorrow)

;; Add to hook to reload these automatically
(defun my-dark-theme-hook ()
  "Run after loading dark theme."
  ;; zenburn
  (if (eq window-system nil)
      (set-face-attribute 'mode-line-inactive nil :background "color-236"))
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (setq evil-insert-state-cursor '("gray" bar))
  (set-face-attribute 'hl-line nil :background "gray29" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "gray40")
  (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified)
  (set-face-attribute 'my-hi-comint nil :background "dim gray"))

(defun my-light-theme-hook ()
  "Run after loading light theme."
  ;; base16-tomorrow
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (set-face-attribute 'hl-line nil :background "gray96" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "light gray")
  (set-face-attribute 'mode-line-inactive nil :background "white smoke")
  (set-face-attribute 'org-mode-line-clock nil :background "white" :inherit nil)
  (set-face-attribute 'bm-face nil :background "light cyan" :overline 'unspecified :foreground 'unspecified)
  (set-face-attribute 'my-hi-comint nil :background "light gray"))

(my-add-theme-hook my-theme-dark #'my-dark-theme-hook)
(my-add-theme-hook my-theme-light #'my-light-theme-hook)

(defvar my-theme-type nil
  "Type of current theme.")

(setq my-theme-type 'dark)

(defun my-theme-toggle (&optional type)
  "Toggle theme to TYPE."
  (interactive)
  (unless type (setq type my-theme-type))
  (cond ((eq type 'dark)
         (disable-theme my-theme-light)
         (load-theme my-theme-dark t nil)
         (setq my-theme-type 'dark))
        ((eq type 'light)
         (disable-theme my-theme-dark)
         (load-theme my-theme-light t nil)
         (setq my-theme-type 'light))))

(defun my-theme-switch ()
  "Switch from dark theme to light or vice versa."
  (interactive)
  (cond ((eq my-theme-type 'light)
         (my-theme-toggle 'dark))
        ((eq my-theme-type 'dark)
         (my-theme-toggle 'light))))

;; theme config depends on ace-window and bm
(with-eval-after-load "ace-window"
  (with-eval-after-load "bm"
    (with-eval-after-load "hi-lock"
      (my-theme-toggle))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)

  (if my-debug (message "ace-window")))


(use-package ag
  :config

  (if my-debug (message "ag")))


(use-package bm
  :init
  (setq bm-cycle-all-buffers t)
  :config

  (if my-debug (message "bm")))


(use-package comment-dwim-2
  :config

  (if my-debug (message "comment-dwim-2")))


(use-package define-word
  :config

  (if my-debug (message "define-word")))


(use-package dap-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (if my-debug (message "dap-mode")))


(use-package dumb-jump
  :config

  (if my-debug (message "dumb-jump")))


(use-package general
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

    (general-def
      :keymaps 'override
      :prefix "C-x i"
      "i" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "n" '(lambda () (interactive) (find-file "~/.emacs.d/notes.org"))
      "c" '(lambda () (interactive) (find-file "~/.emacs.d/classic-init.el"))
      )

    (general-def
      :keymaps 'override
      "C-x s" 'save-buffer
      "<f8>" 'my-switch-to-last-window
      "M-j" 'helm-semantic-or-imenu
      "C-j" 'helm-swoop
      "C-S-j" 'helm-swoop-without-pre-input
      "<f2>" 'bm-next
      "S-<f2>" 'bm-previous
      "C-<f2>" 'bm-toggle
      "<f10>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (my-kill-python)
                 (my-sh-send-command my-global-shell-command))
      "C-h j" 'describe-face  ; introspect colors
      "C-x b" 'helm-buffers-list
      "C-x g" 'magit-status
      "C-x C-f" 'my-find-file
      )

    (general-def
      :keymaps 'override
      :states '(normal insert)
      (general-chord "jk") 'my-newline-without-break-of-line
      "C-;" 'comment-dwim-2
      "<f9>" 'save-buffer
      "\M-Q" 'my-unfill-paragraph
      )

    (general-def
      :states '(normal insert emacs)
      ;; qwerty bindings
      "C-]" 'xref-find-definitions
      "C-}" 'xref-find-definitions-other-window
      "M-]" 'dumb-jump-go
      "M-}" 'dumb-jump-go-other-window

      ;; ;; dvp bindings
      ;; "C-@" 'xref-find-definitions
      ;; "C-^" 'xref-find-definitions-other-window
      ;; "M-@" 'dumb-jump-go
      ;; "M-^" 'dumb-jump-go-other-window

      ;; "C-o" 'dumb-jump-back  ; obsoleted
      "C-o" 'xref-pop-marker-stack
      )

    (general-def
      :states 'normal
      :prefix "SPC"
      ";" 'comment-dwim-2
      "=" 'er/expand-region
      "+" 'er/contract-region
      "b" 'helm-buffers-list
      "f" 'my-find-file
      "k" 'kill-buffer
      "o" 'ace-window
      "s" 'save-buffer
      "g" 'ffap-other-window
      )

    (general-define-key
     :keymaps 'comint-mode-map
     :states '(insert emacs)
     "-" #'(lambda () (interactive) (insert "_"))
     "_" #'(lambda () (interactive) (insert "-"))
     )
    (general-def
      :keymaps 'comint-mode-map
      "<f10>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (my-kill-python)
                 (my-sh-send-command my-global-shell-command))
      "C-l" 'comint-clear-buffer
      "C-x C-l" 'recenter-top-bottom
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
      "<apps>" 'my-kill-python
      "<f6>" 'my-insert-breakpoint
      "<f10>" '(lambda() (interactive)
                 (save-some-buffers t nil)
                 (my-kill-python)
                 (my-sh-send-command my-global-shell-command))
      "<C-S-f10>" 'my-set-global-shell-command-to-current-file
      "<S-f10>" 'my-buffer-file-to-shell
      )
    (general-def
      :keymaps 'elpy-mode-map
      :states 'normal
      :prefix "SPC"
      "d" 'elpy-occur-definitions
      "c" 'my-string-inflection-style-cycle
      )

    (general-def
      :keymaps 'emacs-lisp-mode-map
      :states 'normal
      :prefix "SPC"
      "x" 'eval-expression
      "e" 'eval-last-sexp
      )

    (general-def
      :keymaps 'emacs-lisp-mode-map
      "C-<next>" 'forward-page  ; C-PgUp goto previous linebreak
      "C-<prior>" 'backward-page ; C-PgDown goto next linebreak
      )

    ; won't work in terminal bc of how terminals work
    (general-def
      :keymaps 'evil-emacs-state-map
      "<escape>" 'evil-normal-state
      )

    (general-def :keymaps 'helm-map "<escape>"  'helm-keyboard-quit)

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

    ) ; general-after-init

  (if my-debug (message "general")))


(use-package elpy
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

  (if my-debug (message "elpy")))


(use-package ess
  :init (require 'ess-site)
  :config

  (if my-debug (message "ess")))


(use-package evil
  :after (:all dumb-jump key-chord)
  :config
  (evil-mode)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)

  ;; https://github.com/emacs-evil/evil/issues/1074
  (setq evil-undo-system 'undo-redo)

  ;; todo cond fails? maybe window var not set during init?
  ;; Coordinate states with cursor color
  (if (or (eq window-system 'w32) (eq window-system 'x))  ; todo use memq/member
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

  (if my-debug (message "evil")))


(use-package evil-lion
  :after (:all evil)
  :config
  (evil-lion-mode 1)

  (if my-debug (message "evil-lion")))


(use-package evil-surround
  :after (:all evil)
  :config
  (global-evil-surround-mode 1)

  (if my-debug (message "evil-surround")))


(use-package expand-region
  :config

  (if my-debug (message "expand-region")))


(use-package flycheck
  :config

  (if my-debug (message "flycheck")))


(use-package helm
  :config

  (if my-debug (message "helm")))


(use-package helm-swoop
  :after helm
  :config

  (defun my--reset-linum-hack ()
    "Hack to reset line numbers by switching to next buffer and switching back."
    (progn
      (switch-to-buffer (other-buffer (current-buffer) 1))
      (switch-to-buffer (other-buffer (current-buffer) 1))))

  (add-hook 'helm-after-action-hook 'my--reset-linum-hack)
  (add-hook 'helm-quit-hook 'my--reset-linum-hack)

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


  (if my-debug (message "helm-swoop")))


(use-package hi-lock
  :init
  (defun my-toggle-global-hl-line-sticky-flag ()
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

  (defface my-hi-comint
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

  (if my-debug (message "hi-lock")))


(use-package htmlize
  :config

  (if my-debug (message "htmlize")))


;; https://github.com/jwiegley/use-package#use-package-chords
(use-package key-chord
  :config (key-chord-mode 1)

  (if my-debug (message "key-chord")))


(when (eq system-type 'gnu/linux)
  (use-package ledger-mode
    :defer t
    :config
    (setq ledger-post-amount-alignment-column 60)

    (if my-debug (message "ledger-mode"))))


(use-package magit
  :init
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide)))
  :config
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (if my-debug (message "magit")))


(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config

  (if my-debug (message "markdown-mode")))


(use-package markdown-toc
  :config

  (if my-debug (message "markdown-mode")))


(use-package nameless
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config

  (if my-debug (message "nameless")))


;; This step works some magic. For details:
;; https://github.com/raxod502/straight.el#integration-with-org
(use-package org
  :after (helm helm-swoop)
  :config
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

  (defun my-new-clock-task ()
    "Switch to new task by clocking in after clocking out."
    (interactive)
    (org-clock-out)
    (org-clock-in))

  ;; org-mode doesn't automatically save archive files for some
  ;; stupid reason.  This is a ruthless hack which saves /all/ org
  ;; buffers in archive.
  ;; https://emacs.stackexchange.com/a/51112/15177
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)

  (if my-debug (message "org")))

;; 
;; (use-package ox-confluence
;;   :straight (:type git :repo "https://github.com/sdelafond/org-confluence")
;;   :config
;;   (require 'ox-confluence)

;;   (if my-debug (message "ox-confluence") ;;   ))

;; 
;; (use-package ox-confluence-en
;;   :load-path "~/.emacs.d/lisp/")


(use-package right-click-context
  :config
  (right-click-context-mode 1)

  (if my-debug (message "right-click-context")))


(use-package rg
  :config

  (if my-debug (message "rg")))


(use-package simple-httpd
  :config

  (if my-debug (message "simple-httpd")))


(use-package smart-tab
  ;; owner moved repo and uses "main" instead of "master"
  :straight (:type git :repo "https://git.genehack.net/genehack/smart-tab.git" :branch "main")
  :config
  (global-smart-tab-mode 1)

  (if my-debug (message "smart-tab")))


(use-package string-inflection
  :config
  (defun my--string-inflection-style-cycle-function (str)
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

  (defun my-string-inflection-style-cycle ()
    "foo-bar => foo_bar => FOO_BAR => fooBar => FooBar => foo-bar"
    (interactive)
    (string-inflection-insert
     (my--string-inflection-style-cycle-function
      (string-inflection-get-current-word))))

  (if my-debug (message "string-inflection")))


(use-package sx
  :config

  (if my-debug (message "sx")))


(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (if my-debug (message "yaml-mode")))


(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (if my-debug (message "web-mode")))

;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :config
;;   ;; (require 'smartparens)
;;   (sp-pair "(" ")" :unless '(sp-point-before-word-p))
;;   (sp-pair "\"" "\"" :unless '(sp-point-before-word-p sp-point-after-word-p))
;;   (sp-pair "[" "]" :unless '(sp-point-before-word-p))
;;   (sp-pair "'" "'" :unless '(sp-point-before-word-p))
;;   (smartparens-global-mode 1)
;;   (setq sp-highlight-pair-overlay nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://stackoverflow.com/a/1110487
(eval-after-load "dired"
  '(progn
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the
point, or when prefix arg, the next N files"
       (interactive "P")
       (mapc 'find-file (dired-get-marked-files nil arg)))
     (define-key dired-mode-map "F" 'my-dired-find-file)))

;; Auto-refresh dired on file change
;; https://superuser.com/a/566401/606203
(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun my-switch-to-last-window ()
  "Switch to most recently used window.

See URL `https://emacs.stackexchange.com/a/7411/15177'"
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

;; https://stackoverflow.com/a/21058075/5065796
(defun create-scratch-buffer ()
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
                     (org-mode)
  (if (= n 1) initial-major-mode)))

(add-hook 'before-save-hook 'whitespace-cleanup)

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

(setq my-python-break-string "import ipdb; ipdb.set_trace(context=10)")

(defun my-insert-breakpoint ()
  (interactive)
  (my-newline-without-break-of-line)
  (insert my-python-break-string)
  (bm-toggle)
  (save-buffer))

(let ((dir (cond ((eq system-type 'windows-nt) "C:\\projects\\")
                 ((eq window-system 'x) "~/Projects/")
                 ;; terminal
                 (t "/data/data/com.termux/files/home/projects/"))))
  (setq my-global-default-directory dir))

(defun my-global-default-directory (new-default-directory)
  "Set my-global-default-directory to NEW-DEFAULT-DIRECTORY."
  (interactive "DSet global default directory: ")
  (setq my-global-default-directory new-default-directory))

(defun my-find-file (&optional filename)
  "Switch to a buffer visiting FILENAME, defaulting to `my-global-default-directory'."
  (interactive)
  (if (null filename)
      (setq filename my-global-default-directory))
  (cd my-global-default-directory)
  (call-interactively 'find-file filename))

(defvar my-python nil
  "Python interpreter to be used in shell calls.")

(defvar my-shell "*shell*"
  "Shell process buffer to be used in shell calls.")

(if (eq system-type 'windows-nt)
    (setq my-python (concat "python" " "))
  (setq my-python (concat "python3" " ")))

(defun my-set-python (exe)
  "Set python executable."
  ;; (interactive "fSelect Python executable: ")
  (interactive
   (list (read-file-name "Python executable: " "C:/Users/mtrzcinski/Anaconda3/envs/" nil t)))
  (setq my-python (concat exe " "))
  (message "Set `my-python' to: %s" my-python))

(defun my-set-shell (pbuff)
  "Set `my-shell' to process associated with PBUFF buffer."
  (interactive
   (list (read-buffer "Process buffer: " nil t '(lambda (x) (processp (get-buffer-process (car x)))))))
  (setq my-shell pbuff)
  (message "Set `my-shell' to: %s" my-shell))

(defun my-create-shell (name)
    "Create shell with a given NAME.

NAME should have earmuffs (e.g. *NAME*) if it is to follow Emacs
naming conventions.  Earmuffs indicate that the buffer is special
use and not associated with a file.

Returns newly created shell process.

Adapted from URL `https://stackoverflow.com/a/36450889/5065796'"
    (interactive
     (let ((name (read-string "shell name: " nil)))
       (list name)))
    (let ((name (or name my-shell)))
      (get-buffer-process (shell name))))

(defun my-sh-send-command (command &optional pbuff)
  "Send COMMAND to shell process with buffer PBUFF.

Create new shell process if none exists.

See URL `https://stackoverflow.com/a/7053298/5065796'"
  (let* ((pbuff (or pbuff my-shell))
         (proc (or (get-buffer-process pbuff)
                   (let ((currbuff (current-buffer)))
                     (shell)
                     (switch-to-buffer currbuff)
                     (my-create-shell pbuff))))
         (command-and-go (concat command "\n")))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command-and-go)
      (move-marker (process-mark proc) (point)))
    (process-send-string proc command-and-go)))

(defun my-set-global-shell-command (new-command)
  "Set `my-global-shell-command' to NEW-COMMAND."
  (interactive "sShell command: ")
  (setq my-global-shell-command new-command))

;; 16000
(defun my-kill-python ()
  "Kill Python."
  (interactive)
  (if (eq system-type 'windows-nt)
      (shell-command "taskkill /f /fi \"IMAGENAME eq python.exe\" /fi \"MEMUSAGE gt 15000\"")))

;; todo make interactive, read envs dir for available envs
(defun my-conda-activate ()
  "Activate conda venv."
  (interactive)
  (insert "C:\\Users\\mtrzcinski\\Anaconda3\\condabin\\conda.bat activate "))

(defun my-set-global-shell-command-to-current-file ()
  "Set the global shell command to use the current file.

This is useful if, for instance, a project was started using one
file, but later in development another file needs to be called
frequently.  It is like a permanent version of
`my-buffer-file-to-shell'."
  (interactive)
  (setq my-global-shell-command (concat my-python (buffer-file-name)))
  (message "Set `my-global-shell-command' to \"%s\"" my-global-shell-command))

(defun my-buffer-file-to-shell ()
  "Send current buffer file to shell as python call."
  (interactive)
  (my-sh-send-command (concat my-python (buffer-file-name))))

(defun my-newline-without-break-of-line ()
  "Create a new line without breaking the current line and move
the cursor down."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun my-comint-exec-hook ()
  (interactive)
  ;; (highlight-lines-matching-regexp "-->" 'my-hi-comint)
  (setq comint-scroll-to-bottom-on-output t)
  (setq truncate-lines t)
  (set-window-scroll-bars (get-buffer-window "*shell*") nil nil 10 'bottom))

(add-hook 'comint-exec-hook #'my-comint-exec-hook)

(defun my-unfill-paragraph (&optional region)
  "Make multi-line paragraph into a single line of text.

REGION unfills the region.  See URL
`https://www.emacswiki.org/emacs/UnfillParagraph'"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my-open-windows-explorer ()
  "Open Windows Explorer to folder containing file."
  (interactive)
  (let* ((file (or (buffer-file-name (current-buffer)) my-global-default-directory))
         (dir (file-name-directory file))
         (windows-dir (subst-char-in-string ?/ ?\\ dir)))
    (start-process "explorer" nil "explorer.exe" windows-dir)))

;; (defun my-open-windows-explorer (&optional start end)
;;   (interactive "r")
;;   (message "%s" (buffer-substring-no-properties start end)

;;	   ))

(defun my-suicide ()
  "Kill all Emacs processes."
  (interactive)
  (let ((cmd (if (eq system-type 'gnu/linux)
                 "killall -9 emacs" ; probably won't kill server administered by systemd
               "taskkill /f /fi \"IMAGENAME eq emacs.exe\" /fi \"MEMUSAGE gt 15000\"")))
    (shell-command cmd)))
