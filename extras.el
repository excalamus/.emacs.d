
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


(use-package csound-mode
  :after (:all org)
  :straight (:fork "excalamus/csound-mode")
  :config

  (if xc/debug (message "csound-mode")))


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


(use-package elpher
  :after (:all org)
  :straight (:repo "git://thelambdalab.xyz/elpher.git"))


(use-package ess
  :after (:all org)
  :straight (:fork "excalamus/ess")
  :init (require 'ess-site)
  :config

  (if xc/debug (message "ess")))


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


(use-package flymake
  :after (:all org)
  :straight (:fork "excalamus/flymake")
  :config

  (if xc/debug (message "flymake")))


;; (straight-use-package 'flymake)


(use-package free-keys
  :after (:all org)
  :straight (:fork "excalamus/free-keys")
  :config

  (if xc/debug (message "free-keys")))


(use-package keycast
  :after (:all org)
  :straight (:fork "excalamus/keycast")
  :config

  (setq keycast-separator-width 30)
  (set-face-attribute 'keycast-key nil :background "gray29" :foreground "yellow3" :height 1.2)
  (set-face-attribute 'keycast-command nil :background "gray29" :foreground "yellow3" :height 1.2 :weight 'bold :box '(:line-width -3 :style released-button))
  (if xc/debug (message "keycast")))


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


(use-package markdown-toc
  :after (:all markdown-mode org)
  :straight (:fork "excalamus/markdown-toc")
  :config

  (if xc/debug (message "markdown-toc")))


(use-package nov
  :after (:all org)
  :init
  :config

  (if xc/debug (message "nov")))


(use-package qml-mode
  :after (:all org)
  :straight (:fork "excalamus/qml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

  (if xc/debug (message "qml-mode")))


;; skeeto fork
(use-package simple-httpd
  :after (:all org)
  :straight (:fork "excalamus/emacs-web-server")
  :config

  (if xc/debug (message "emacs-web-server")))


(use-package sql
  :after (:all org)
  :config
  ;; ;; load project profiles, kept here versus lisp/ for security sake
  ;; (if (eq xc/device 'windows)
  ;;     (load "~/sql-connections.el"))
  (setq sql-postgres-login-params nil)

  (if xc/debug (message "sql")))


(use-package sql-indent
  :after (:all org)
  :straight (:fork "excalamus/emacs-sql-indent")
  :config

  (if xc/debug (message "emacs-sql-indent")))


(use-package yaml-mode
  :after (:all org)
  :straight (:fork "excalamus/yaml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (if xc/debug (message "yaml-mode")))
