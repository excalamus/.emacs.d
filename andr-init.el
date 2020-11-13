;; -*- truncate-lines: t -*-

; init.el --- Emacs initialization file
;;
;; Author: Matt Trzcinski
;;
;; Documented with links and troubleshooting solutions.
;;
;; Rainy day:
;;; Configurations:
;;   - [ ] switch-to-previous-buffer doesn't work as expected on multiple frames
;;   - [ ] Make Occur mode behave like helm-swoop: change <ESC> to (quit-window), <RET> to goto occurance and quit
;;   - [ ] Bind switch-window with C-x o o (key-chord on submap?)
;;   - [ ] Open new buffers in 'vertical' windows; evil :reg opens new window and then, when quitting, leaves it open
;;   - [ ] Fix highlighting: hi-pink text is pink when line is highlighted. Get same behavior for hi-blue and hi-yellow
;;   - [ ] Fix inactive highlighting: https://emacs.stackexchange.com/questions/14638/change-highlight-color-when-window-isnt-in-focus
;;   - [ ] review Purpose package (https://github.com/bmag/emacs-purpose)
;;   - [ ] Figure out native indentation in org-mode source blocks
;;   - [-] Correct smartparens insertion of quotes/parens when modifying text
;;     Ex. |:ensure --> "":ensure happens when trying to quote
;;   - [ ] Configure evil-want-C-u-scroll
;;   - [ ] boolcase-mode
;;   - [ ] Consider using pyvenv-tracking-mode
;;   - [ ] webmode setup (https://github.com/gopar/.emacs.d/blob/6cc67b869b8687454990ceb23cf9f177d17033c3/init.el#L979)
;;   - [ ] Find non-customize groups solution to terminal coloring
;;   - [ ] comment-dwim2 comments three lines instead of two when using evil visual selction
;;   - [ ] Make my-find-file have a "start from directory of current file" when using universal
;;   - [ ] Create funtion to turn region into abbrev expansion (why the f doesn't this already exist?)
;;   - [ ] https://emacs.stackexchange.com/a/2302/15177
;;
;;; Customization:
;;   - [ ] Create function to clear all highlighting in buffer (repeated M-s h u)
;;   - [ ] (display-monitor-attributes-list)
;;   - [ ] Put dimmer-mode on a delay timer.  If you haven't typed anything for X
;;     minutes, enable dimmer-mode so that you know which buffer you're in.
;;     https://www.emacswiki.org/emacs/IdleTimers
;;   - [ ] Create mercurial mode that works!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom-set.el")

(package-initialize)
;; (toggle-debug-on-error)
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-refresh-contents)

;; Don't load undo-tree when loading evil.  It's the devil.
(setq load-path (remove "/data/data/com.termux/files/home/.emacs.d/elpa/undo-tree-0.6.5" load-path))

;; ;; https://github.com/jwiegley/use-package#extending-the-load-path
;; ;; for mercurial.el
;; (if (eq system-type 'windows-nt)
;;     (add-to-list 'load-path "~/.emacs.d/lisp")
;;   (add-to-list 'load-path "~/.emacs.d/lisp/"))

;; Make sure use-package is installed. All other packages are
;; installed via use-package
(defvar my-package-install-list
  '(
    use-package
    ))

;; The #' syntax is shorthand for 'function'
;; If package is not installed, then install it
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      my-package-install-list)

;; Base config
;; -----------
(set-language-environment "UTF-8")
(delete-selection-mode 1)
(show-paren-mode 1)

;; Binding to open init.el
(global-set-key (kbd "C-x i") '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))

;; Disable stupid minimize hotkeys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Rebind save-some-buffers because
;; sometimes I mistype C-x C-s.
;; If kill-some-buffers doesn't get a binding
;; why should save-some-buffers?
(global-set-key (kbd "C-x s") 'save-buffer)

(setq initial-scratch-message nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'org-mode)
(setq initial-major-mode 'emacs-lisp-mode)
; (setq help-window-select t)
(setq help-window-select nil)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq show-help-function nil)

;; Change yes-no prompts to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Make occur window open to the side
(setq
 display-buffer-alist
 `(("\\*Occur\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 0)
    (window-width . fit-window-to-buffer))))

;; Used for introspection
(global-set-key (kbd "C-h j") 'describe-face)

;; Emacs appearance
;; ----------------
(load-theme 'tango-dark)
;; (scroll-bar-mode -1)
;; (tool-bar-mode -1)

;; Only available in emacs-version >= 26.1
;; (global-display-line-numbers-mode)

;; This slows windows down...
;; (setq display-line-numbers-type 'relative)

(global-hl-line-mode 1)
(set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
(set-face-attribute 'mode-line nil :background "light blue")

;; https://dejavu-fonts.github.io/
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
			:family "DejaVu Sans Mono"
			:height 113
			:weight 'normal
			:width 'normal))

;;; Mode line
(column-number-mode t)
(setq mode-line-position
	    '((line-number-mode ("%l" (column-number-mode ":%c ")))
	      (-3 "%p")))

;; (setq mode-line-modes
;;       (mapcar (lambda (elem)
;;                 (pcase elem
;;                   (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
;;                    "")
;;                   (t elem)))
;;               mode-line-modes))

(which-function-mode)

(setq-default mode-line-format
	      '("%e"
		mode-line-position "  "
		mode-line-modified "  "
		mode-line-buffer-identification "  "
		mode-line-misc-info
		;; mode-line-modes "  "
		))

;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))

(with-eval-after-load "term"
  (set-face-attribute 'term-color-black   nil :foreground "#3F3F3F")
  (set-face-attribute 'term-color-blue    nil :foreground "#7CB8BB")
  (set-face-attribute 'term-color-cyan    nil :foreground "#93E0E3")
  (set-face-attribute 'term-color-green   nil :foreground "#7F9F7F")
  (set-face-attribute 'term-color-magenta nil :foreground "#DC8CC3")
  (set-face-attribute 'term-color-red     nil :foreground "#AC7373")
  (set-face-attribute 'term-color-white   nil :foreground "#DCDCCC")
  (set-face-attribute 'term-color-yellow  nil :foreground "#DFAF8F"))

;; User-defined-functions
;; ----------------------

;; (defun create-scratch-buffer nil
;;   "create a scratch buffer"
;;   (interactive)
;;   (switch-to-buffer (get-buffer-create "*scratch*"))
;;   (org-mode))

(defun my-buffer-exists-p (bufname)
  "Return t if BUFNAME exists, nil otherwise."
  (not (eq nil (get-buffer bufname))))

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
		     (emacs-lisp-mode)
		     ;;(flycheck-mode -1)
  (if (= n 1) initial-major-mode)))

;; (global-set-key (kbd "C-x C-;") 'comment-region)

(defun duplicate-line()
  "Duplicate current line and move down."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-S-d") 'duplicate-line)

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

(defvar my-global-default-directory "~/"
  "An Emacs-wide default directory.")

(if (eq system-type 'windows-nt)
    (setq my-global-default-directory "~/projects/"))

(defun my-find-file (&optional filename)
  "Switch to a buffer visiting FILENAME, defaulting to `my-global-default-directory'."
  (interactive)
  (if (null filename)
      (setq filename my-global-default-directory))
  (cd my-global-default-directory)
  (call-interactively 'find-file filename))

(global-set-key (kbd "C-x C-f") 'my-find-file)
;; (cd my-global-default-directory)

(defun my-set-global-default-directory (new-default-directory)
  "Set my-global-default-directory to NEW-DEFAULT-DIRECTORY."
  (interactive "DNew global default directory: ")
  (setq my-global-default-directory new-default-directory))

(defun my-pop-buffer-into-frame ()
  "Pop current buffer into its own frame."
  (interactive)
  (display-buffer-pop-up-frame (current-buffer) nil))

(global-set-key (kbd "C-c p") 'my-pop-buffer-into-frame)
(global-set-key (kbd "C-c C-p") 'my-pop-buffer-into-frame)

(defun my-newline-without-break-of-line ()
  "Create a new line without breaking the current line and move
the cursor down."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; ;; https://emacs.stackexchange.com/a/29721/15177
;; (defun org-table-select-cell ()
;;   "Mark the current table field."
;;   (interactive)
;;   ;; Do not try to jump to the beginning of field if the point is already there
;;   (when (not (looking-back "|\\s-?"))
;;     (org-table-beginning-of-field 1))
;;   (set-mark-command nil)
;;   (org-table-end-of-field 1))

;; (org-defkey org-mode-map (kbd "C-c v") 'org-table-select-cell)
;; (org-defkey org-mode-map (kbd "C-c e") 'org-table-end-of-field)

;; Useful when working with Jupyter
(defun my-set-region-read-only (begin end)
  "Set region between BEGIN and END to read-only.

See URL `http://stackoverflow.com/questions/7410125'"
  (interactive "r")
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

(defun my-set-region-writeable (begin end)
  "Make region between BEGIN and END writable.

See URL `http://stackoverflow.com/questions/7410125'"
  (interactive "r")
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

(defun my-sh-send-command (command)
  "Send COMMAND to current shell process.

Creates new shell process if none exists.

See URL `https://stackoverflow.com/a/7053298/5065796'"
  (let ((proc (get-process "shell"))
	pbuf)
    (unless proc
      (let ((currbuff (current-buffer)))
	(shell)
	(switch-to-buffer currbuff)
	(setq proc (get-process "shell"))
	))
    (setq pbuff (process-buffer proc))
    (setq command-and-go (concat command "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command-and-go)
      (move-marker (process-mark proc) (point))
      )
    (process-send-string proc command-and-go)))

(defun my-set-global-shell-command (new-command)
  "Set `my-global-shell-command' to NEW-COMMAND."
  (interactive "sShell command: ")
  (setq my-global-shell-command new-command))

(when (eq system-type 'windows-nt)
  (defun my-activate-project ()
    "Activate everything needed for a project"
    (interactive)
    (let*((project-dir
	   (file-name-as-directory
	    (read-directory-name "Project directory: " "c:/projects/" nil t)))
	  (ms-project-dir (replace-regexp-in-string "/" "\\" project-dir t t))
	  (venv-dir
	   (file-name-as-directory
	    (read-directory-name "Venv directory: " project-dir nil 1 "venv/")))
	  (main-file (read-file-name "Main file: " project-dir nil "confirm"))
	  (cmd-cd (concat "cd " "\"" ms-project-dir "\""))
	  (cmd-python (concat "python \"" main-file "\""))
	  )
      (setq my-global-default-directory project-dir)
      (my-sh-send-command cmd-cd)
      (my-sh-send-command "deactivate")
      (my-sh-send-command (concat venv-dir "Scripts/activate.bat"))
      (pyvenv-activate venv-dir)
      (setq my-global-shell-command cmd-python)
      (global-set-key (kbd "<f7>") (lambda() (interactive) (my-sh-send-command my-global-shell-command)))
      (delete-other-windows)
      (find-file main-file)
      (switch-to-buffer (get-file-buffer main-file))
      )))

(when (eq system-type 'gnu/linux)
  (defun my-activate-project ()
    "Activate everything needed for a project"
    (interactive)
    (let*((project-dir
	   (file-name-as-directory
	    (read-directory-name "Project directory: " "~/projects/" nil t)))
	  (venv-dir
	   (file-name-as-directory
	    (read-directory-name "Venv directory: " project-dir nil 1 "venv/")))
	  (main-file (read-file-name "Main file: " project-dir nil "confirm"))
	  (cmd-cd (concat "cd " project-dir ))
	  (cmd-python (concat "python " main-file)))
      (setq my-global-default-directory project-dir)
      (my-sh-send-command cmd-cd)
      (my-sh-send-command "deactivate")
      (my-sh-send-command (concat "source " venv-dir "bin/activate"))
      (pyvenv-activate venv-dir)
      (setq my-global-shell-command cmd-python)
      (global-set-key (kbd "<f7>") (lambda() (interactive) (my-sh-send-command my-global-shell-command)))
      (delete-other-windows)
      (find-file main-file)
      (switch-to-buffer (get-file-buffer main-file))
      (with-current-buffer "*shell*"
	(comint-clear-buffer)))))

(defun my-smart-beginning-of-line ()
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

(global-set-key [home] 'my-smart-beginning-of-line)
(global-set-key "\C-a" 'my-smart-beginning-of-line)


;; BUG: Doesn't work intuitively when last buffer was a temporary one
;; 1. Switch to a "main" type window
;; 2. Switch back
;; 3. Do C-h v or C-h f to open a help buffer
;; 4. Since *Help* is selected automatically, it becomes the last window,
;;    even after the buffer is closed.
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

(global-set-key (kbd "<f8>") 'my-switch-to-last-window)

(defun my-switch-to-shell ()
  "Attempt to switch to *shell* buffer."
  (interactive)
  (cond
   ((string-equal (buffer-name (current-buffer)) "*shell*")
    (progn
      (goto-char (point-max))
      (message "Already on *shell*!")))
   ((get-buffer-window "*shell*" t)
    (progn
      (switch-to-buffer-other-frame "*shell*")
      (goto-char (point-max))))
   ((get-buffer "*shell*")
    (message "The *shell* is not currently visible!"))
   ((message "No *shell* buffer exists!"))))

(global-set-key (kbd "<S-f8>") 'my-switch-to-shell)

;; (defun toggle-comment-on-line ()
;;   "Toggle comment on current line."
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; (global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; C-y to yank
;; M-y to cycle backwards through kill list
;; M-Y to cycle forward
(defun my-yank-pop-forwards (arg)
  "Pop ARGth item off the kill ring.

See URL `https://web.archive.org/web/20151230143154/http://www.emacswiki.org/emacs/KillingAndYanking'"
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key "\M-Y" 'my-yank-pop-forwards)

;; use-package
;; -----------

;; https://github.com/jwiegley/use-package
(require 'use-package)

;; https://github.com/jwiegley/use-package#package-installation
;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package ag
  :ensure t
  )

(use-package bm
  :ensure t
  ;; These are the recommended Visual Studio-like bindings.
  ;; Note that the <f2> is viewed as a modifier key by default
  ;; so that this overrides whatever may be there.
  :bind (("<f2>" . bm-next)
	 ("S-<f2>" . bm-previous)
	 ("C-<f2>" . bm-toggle))
  :config
  (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified))

(use-package comment-dwim-2
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'comment-dwim-2))

(use-package define-word
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'define-word-at-point))

(use-package dimmer
  :ensure t
  :bind ("<f9>" . dimmer-mode)
  :config
  (setq dimmer-fraction 0.30)
  ;; When first enabled, dimmer-mode requires the buffer to be switched
  ;; before it takes effect.  There may be a better way, but I couldn't
  ;; find it!
  (add-hook 'dimmer-mode-hook (lambda ()
				(progn (previous-buffer) (next-buffer))
				)))

(use-package ein
  :ensure t)

(use-package elpy
  :ensure t
  :chords (("jk" . my-newline-without-break-of-line))
  :init
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'elpy-mode-hook #'hs-minor-mode)
  (add-hook 'elpy-mode-hook (lambda () (company-mode -1)))
  :config
  (pyvenv-mode 1)
  (elpy-enable)

  (when (eq system-type 'gnu/linux)
    (setq-default indent-tabs-mode nil)
    (setq python-shell-interpreter "python"
	  python-shell-interpreter-args "-i")
    (setq elpy-rpc-python-command "python2"))

  (when (eq system-type 'windows-nt)
    (setq-default indent-tabs-mode nil)
    ;; https://emacs.stackexchange.com/questions/24750/emacs-freezes-with-ipython-5-0-0
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt")
    (setq elpy-rpc-python-command "python"))

  ;; For some reason, simple :bind doesn't work
  ;; https://emacs.stackexchange.com/a/46251/15177
  (unbind-key "<C-return>" elpy-mode-map)
  (unbind-key "<C-S-return>" elpy-mode-map)
  (bind-keys :map elpy-mode-map
	     ("<C-return>". elpy-shell-send-statement)
	     ("<C-S-return>" . elpy-shell-send-statement-and-step)
	     ("<M-return>" . elpy-shell-send-group)
	     ("<M-S-return>" . elpy-shell-send-group-and-step)))

(use-package evil
  :ensure t
  :after neotree
  :config
  (evil-mode)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "jj" 'evil-normal-state)

  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map "\C-n" 'elpy-nav-forward-block)
  (define-key evil-normal-state-map "\C-p" 'elpy-nav-backward-block)

  (define-key evil-normal-state-map (kbd "C-}") 'xref-find-definitions-other-window)

  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

  ;; (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

  ;; ;; Coordinate states with cursor color
  ;; (setq evil-emacs-state-cursor '(bar "color-117"))
  ;; (setq evil-normal-state-cursor '(box "color-214"))
  ;; (setq evil-insert-state-cursor '(bar "color-248"))
  ;; (setq evil-visual-state-cursor '(box "color-246"))
  ;; (setq evil-motion-state-cursor '(box "color-177"))

    (setq evil-emacs-state-tag (propertize " <E> " 'face '((:background "color-117"))))
    (setq evil-normal-state-tag (propertize " <N> " 'face '((:background "color-214"))))
    (setq evil-insert-state-tag (propertize " <I> " 'face '((:background "color-248"))))
    (setq evil-visual-state-tag (propertize " <V> " 'face '((:background "color-246"))))
    (setq evil-motion-state-tag (propertize " <M> " 'face '((:background "color-177"))))

  )

  ;; (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified))


(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package expand-region
  :ensure t
  :after evil
  :bind (("C-=" . er/expand-region)
	 ("C-+" . er/contract-region)))

;; (use-package flycheck
;;   :ensure t
;;   :config (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package helm
  :ensure t
  :bind
  (("C-x b" . helm-buffers-list)
   :map helm-map
   ("<escape>" . helm-keyboard-quit)))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("C-j" . helm-swoop)
	 ("M-j" . helm-semantic-or-imenu))
  :config

  (defun my-reset-linum-hack ()
    "Hack to reset line numbers by switching to next buffer and switching back."
    (progn
      (switch-to-buffer (other-buffer (current-buffer) 1))
      (switch-to-buffer (other-buffer (current-buffer) 1))))

  (add-hook 'helm-after-action-hook 'my-reset-linum-hack)
  (add-hook 'helm-quit-hook 'my-reset-linum-hack)

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
		      :inherit            'secondary-selection))

(use-package hi-lock
  :ensure t
  :config
  ;; Toggle on: M-s h r
  ;; Toggle off: M-s h u

  (set-face-attribute 'hi-yellow nil :background "yellow3" :foreground "gray30" :distant-foreground "yellow3 "    :box "dim gray")
  (set-face-attribute 'hi-pink   nil                       :foreground "gray30" :distant-foreground "pink"        :box "dim gray")
  (set-face-attribute 'hi-green  nil                       :foreground "gray30" :distant-foreground "light green" :box "dim gray")
  (set-face-attribute 'hi-blue   nil                       :foreground "gray30" :distant-foreground "light blue " :box "dim gray"))

(defun my-toggle-global-hl-line-sticky-flag ()
  "Toggle whether highlighted line persists when switching windows.

For more information, see `global-hl-line-sticky-flag'."
  (interactive)
  (if global-hl-line-sticky-flag
      (setq global-hl-line-sticky-flag nil)
    (setq global-hl-line-sticky-flag t)))

;; On Windows, you need to get Hunspell and it's sketchy.  Config
;; taken from: https://emacs.stackexchange.com/a/22311
;; A source: https://sourceforge.net/projects/hunspell/
(use-package ispell
  ;; :if (eq system-type 'windows-nt)
  :config
  (global-set-key (kbd "C-c i") 'ispell-word)
  ;; (setq ispell-program-name "C:/Program Files (x86)/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  (setq ispell-program-name "/data/data/com.termux/files/usr/bin/hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

(when (eq system-type 'gnu/linux)
  (use-package ledger-mode
    :ensure t
    :config
    (setq ledger-report-resize-window nil)
    ;; smart-tab is defined globally otherwise
    (add-hook 'ledger-mode-hook #'(lambda () (interactive) (smart-tab-mode-off)))
    (define-key ledger-mode-map [\\t] 'ledger-magic-tab))

  (use-package evil-ledger
    :ensure t
    :after ledger-mode
    :config
    (setq evil-ledger-sort-key "S")
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

;; (use-package linum-relative
;;   :ensure t
;;   :config
;;   (linum-relative-global-mode 1))

(use-package magit
  :ensure t)

;; ;; Abandoned package(?) that works better than all the others. It is
;; ;; not available in any of the repos.  Must be installed manually.
;; ;; http://hg.intevation.org/mercurial/file/tip/contrib/mercurial.el
;; (use-package mercurial
;;   :load-path "~/.emacs.d/lisp/")

(use-package ll-debug
  :load-path "~/.emacs.d/lisp/")

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-x M-f") 'neotree-toggle))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; At the time of writing, there is no easy way to automatically install
;; the latest version of org-mode, even though it is on the gnu repo.
;; It's simply easiest to install it manually via list-packages.
(use-package org
  :pin gnu
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select 'always)
  (setq org-todo-keywords
	'((sequence
	   ;; open items
	   "TODO"    ; todo, not active
	   "CURRENT" ; todo, active item
	   "PENDING" ; requires more information (timely)
	   "|"       ; entries after pipe are considered completed in [%] and [/]
	   ;; closed items
	   "DONE"     ; completed successfully
	   "ON-HOLD"  ; requires more information (indefinite time)
	   "CANCELED" ; no longer relevant, not completed
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
     (latex . t)
     ))
  ;;
  (when (eq system-type 'gnu/linux)
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
	     '(
	       (shell . t) ;; for Emacs < 26.1
	       ))))
  ;;
  (when (eq system-type 'windows-nt)
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
	     '(
	       (shell . t) ;; for Emacs == 26.1
	       ))))

  (org-defkey org-mode-map (kbd "C-c w") 'define-word-at-point)
  (org-defkey org-mode-map (kbd "C-c l") 'define-word)

  (message "Installed org-version is %s. If not what you want, then install manually via `list-packages'. This is the easiest method." org-version))

(use-package pylint
  :ensure t)

(use-package realgud
  :ensure t
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;; (use-package shell-pop
;;   :ensure t
;;   :init
;;   (setq shell-pop-universal-key "<f12>")
;;   (setq shell-pop-window-position "bottom")
;;   (setq shell-pop-window-size 30)
;;   (setq shell-pop-full-span t)
;;   ;;
;;   ;; Using hook allows for shell-pop to pick up changes to my-global-default-directory
;;   (when (eq system-type 'windows-nt)
;;     (add-hook 'shell-pop-in-hook (lambda() (interactive)(cd my-global-default-directory)))))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens)
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "\"" "\"" :unless '(sp-point-before-word-p sp-point-after-word-p))
  (sp-pair "[" "]" :unless '(sp-point-before-word-p))
  (sp-pair "'" "'" :unless '(sp-point-before-word-p))
  (smartparens-global-mode 1)
  (setq sp-highlight-pair-overlay nil)
  :diminish smartparens-mode)


(use-package switch-window
  :ensure t
  :bind
  ;; default C-x o is other-window
  ;; default C-x C-o is delete-blank-lines
  (("C-x o" . switch-window)
   ("C-x C-o" . switch-window))
  :config
  (setq switch-window-multiple-frames t)
  (setq switch-window-shortcut-style 'qwerty)
  ;; when emacs is run as client, the first shortcut does not appear
  ;; "x" acts as a dummy
  (setq switch-window-qwerty-shortcuts '("x" "a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "r" "u" "i" "o" "q" "t" "y" "p"))
  (setq switch-window-increase 3))

;; hooks
;; ----
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook (lambda () (toggle-truncate-lines t)))
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'comint-exec-hook
	  (setq comint-scroll-to-bottom-on-output t))

;; Apparently, it's better to use `pop-to-buffer' instead of
;; `switch-to-buffer-other-window'
;;
;; https://stackoverflow.com/a/8638685
(add-hook 'occur-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*Occur*")))
(add-hook 'grep-mode-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*grep*")))

;; staging
;; -------
;;; Code that is not yet fully tested and awaits integration
;;; into the main workflow

;; Change scrolling behavior to be "normal"
;; (setq scroll-step 1)
;; (setq scroll-conservatively 999)

;; Windows LaTeX integration (for use with org-mode(?))
;; (setq exec-path (append exec-path '("C:\\Users\\mtrzcinski\\AppData\\Local\\Programs\\MiKTeX 2.9\\miktex\\bin\\x64")))
;; (setq latex-run-command "pdflatex")

;; Highlight words in buffer
;; (add-to-list 'fic-highlighted-words "DEBUG")
;; (set-face-attribute 'fic-face nil :background 'unspecified)
;; (add-hook 'python-mode-hook 'fic-mode)

;; It would be better to use (kill-process (get-process "Python"))
;; Then, wait for the process to end, restart it, and clear the buffer
(defun my-reset-ipython ()
  "Clear iPython variables and *Python* buffer.

Print date and time of reset to *Python* and to *Messages*.  Does
not reset line numbering or history."
  (interactive)
  (let* ((reset-time (format-time-string "%A, %B %e, %Y @ %-I:%M %p"))
	 (reset-statement "Reset iPython on ")
	(python-command (concat "print('" reset-statement reset-time "')" )))
    (python-shell-send-string "%reset -f" "*Python*")
    (python-shell-send-string  python-command "*Python*")
    (with-current-buffer "*Python*"
      (comint-clear-buffer))
    (message (concat reset-statement "%s") reset-time)))

;; (define-key inferior-python-mode-map (kbd "C-c M-l") 'my-reset-ipython)

;; To start an ipdb process, do M-x ipdb then prefix "python -m " to
;; call a new instance
(defun my-ipdb-send-command (command)
  "Send COMMAND to current ipdb process."
  (let ((proc (get-process "ipdb"))
	pbuf)
    (if proc
	(progn
	  (setq pbuff (process-buffer proc))
	  (setq command-and-go (concat command "\n"))
	  (with-current-buffer pbuff
	    (goto-char (process-mark proc))
	    (insert command-and-go)
	    (move-marker (process-mark proc) (point)))
	  (process-send-string proc command-and-go)))
    (message "No ipdb process exists.")))

;; (define-key comint-mode-map (kbd "<f6>") (lambda() (interactive) (my-ipdb-send-command "n")))
;; (define-key comint-mode-map (kbd "S-<f6>") (lambda() (interactive) (my-ipdb-send-command "s")))

;; (define-key comint-mode-map (kbd "<M-left>") (lambda() (interactive) (my-ipdb-send-command "n")))
;; (define-key realgud-track-mode-map (kbd "<M-right>") (lambda() (interactive) (my-ipdb-send-command "s")))
;; (define-key comint-mode-map (kbd "<M-home>") (lambda() (interactive) (my-ipdb-send-command "r")))

;; (ipdb (concat "python -m ipdb " (buffer-file-name)))

;; Make ipdb usable; works assuming .pdbrc is defined as:
;; # .pdbrc
;; # Enable tab completion (does not work in Emacs)
;; import pdb
;; import rlcompleter
;; pdb.Pdb.complete=rlcompleter.Completer(locals()).complete

;; # Show context on startup
;; l

;; # Define custom aliases to retain the default s, n, c, r behavior
;; # Print behavior, perform action, show context
;; alias S 'Stepping into...' ;; step ;; l
;; alias N 'Stepping over...' ;; next ;; l
;; alias C 'Continuing...' ;; continue ;; l
;; alias R 'Going to return...' ;; return ;; l

(define-key comint-mode-map (kbd "<M-left>") (lambda() (interactive) (process-send-string "*shell*" "S\n")))
(define-key comint-mode-map (kbd "<M-right>") (lambda() (interactive) (process-send-string "*shell*" "N\n")))
(define-key comint-mode-map (kbd "<M-home>") (lambda() (interactive) (process-send-string "*shell*" "R\n")))
(define-key comint-mode-map (kbd "<M-S-home>") (lambda() (interactive) (process-send-string "*shell*" "C\n")))

(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-c h =") '(lambda () (interactive)
					       (hg-diff-repo my-global-default-directory)))
(define-key comint-mode-map (kbd "C-x C-l") 'recenter-top-bottom)

;; Might be able to non-dim the minibuffer with something
;; like (setq dimmer-exclusion-regexp "\\*Minibuf-[0-9]+\\*\\")
(defun my-kill-some-buffers (&optional list)
  "Display some buffers and decide whether to kill them.

Dims all buffers but the one in question.  Non-interactively, if
optional argument LIST is non-nil, it specifies the list of
buffers to kill, asking for approval for each one.  See
`kill-some-buffers'."
  (interactive)
  (require 'dimmer)
  (let* ((orig-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (dimmer-dim-all)
	  (if (null list)
	      (setq list (buffer-list)))
	  (while list
	    (let* ((buffer (car list))
		   (name (buffer-name buffer)))
	      (and name		; Can be nil for an indirect buffer
					; if we killed the base buffer.
		   (not (string-equal name ""))
		   (/= (aref name 0) ?\s)
		   )
	      (display-buffer-same-window buffer (dimmer-restore-buffer buffer))
	      (kill-buffer-ask buffer)
	      (if (buffer-live-p buffer) (dimmer-dim-buffer buffer 0.3))
	      )
	    (setq list (cdr list))))
      (progn
	(dimmer-restore-all)
	(dimmer-mode -1)
	(if (get-buffer orig-buffer)
	    (switch-to-buffer orig-buffer))
	(message "Done killing.")))))

;; Eval a sexp from anywhere in the expression
;; (defun my-eval-sexp (arg)
;;   (interactive "P")
;;   (move-beginning-of-line 1)
;;   (forward-sexp)
;;   (eval-last-sexp arg)
;;   (next-line)
;;   (move-beginning-of-line 1))

;; (local-set-key (kbd "<f7>") 'my-eval-sexp)

(defun my-buffer-file-to-shell ()
  "Send current buffer file to shell as python call."
  (interactive)
  (my-sh-send-command (concat "python " (buffer-file-name))))

(global-set-key (kbd "<S-f7>") 'my-buffer-file-to-shell)

(defun my-reset-global-shell-to-current-file ()
  "Reset the global shell command to use the current file.

This is useful if, for instance, a project was started using one
file, but later in development another file needs to be called
frequently.  It is like a permanent version of
`my-buffer-file-to-shell'."
  (interactive)
  (setq my-global-shell-command (concat "python " (buffer-file-name))))

;; Better handling for long strings.  Emacs doesn't handle long single
;; lines well.
(add-hook 'shell-mode-hook #'visual-line-mode)

;; (add-hook 'yas-after-exit-snippet-hook
;;           (lambda () (if (string-match-p (regexp-quote ", flush=True)") (thing-at-point 'line t))
;;                          (bm-toggle))))

;; (add-hook 'yas-after-exit-snippet-hook
;;           (lambda () (if (buffer-substring yas-snippet-beg yas-snippet-end)
;;                          (bm-toggle))))

;; https://github.com/npostavs/emacs.d/blob/master/elisp/np-utils.el
(defun describe-major-mode-bindings ()
  (interactive)
  (call-interactively 'describe-bindings)
  (with-current-buffer (help-buffer)
    (search-forward "Major Mode Bindings")
(narrow-to-page)))

;; This was a hack for selecting the ag results window.  It is no
;; longer needed for that purpose, but may be useful elsewhere.
;; Switch to buffer whose name contains PART.
;; https://emacs.stackexchange.com/a/47628
;; (defun my-switch-to-existing-buffer-other-window (part)
;;   "Switch to buffer with PART in its name."
;;   (interactive
;;    (list (read-buffer-to-switch "Switch to buffer in other window: ")))
;;   (let ((candidates
;;          (cl-remove
;;           nil
;;           (mapcar (lambda (buf)
;;                     (let ((pos (string-match part (buffer-name buf))))
;;                       (when pos
;;                         (cons pos buf))))
;;                   (buffer-list)))))
;;     (unless candidates
;;       (user-error "No buffer names contain %S" part))
;;     (setq candidates (cl-sort candidates #'< :key 'car))
;;     (switch-to-buffer-other-window (cdr (car candidates)))))

;; Make ag usable for project level searches
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(add-hook 'ag-search-finished-hook
	  '(lambda ()
	     (switch-to-buffer-other-window "*ag search*")))

;; Automatically reload files that have changed on disk
(global-auto-revert-mode 1)

;; (evil-define-key 'normal diff-mode-map (kbd "q") 'View-quit)
(evil-define-key 'normal diff-mode-map (kbd "q") 'View-quit)
(evil-define-key 'normal diff-mode-map (kbd "<return>") 'diff-goto-source)

;; Make *Occur* buffer usable for navigation
(evil-set-initial-state 'occur-mode 'normal)
(evil-define-key 'normal occur-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal occur-mode-map (kbd "<escape>") 'quit-window)

(evil-define-key 'normal occur-mode-map (kbd "TAB") 'occur-mode-display-occurrence)
(evil-define-key 'normal occur-mode-map (kbd "SPC") 'occur-mode-display-occurrence)

(evil-define-key 'normal occur-mode-map (kbd "<return>") '(lambda () (interactive)
							    (occur-mode-goto-occurrence-other-window)
							    (kill-buffer "*Occur*")))

;; Makes fit-window-to-buffer fit horizontally
(setq fit-window-to-buffer-horizontally t)
(add-hook 'occur-hook
       (lambda ()
	 (save-selected-window
	   (pop-to-buffer "*Occur*")
	   (fit-window-to-buffer))))

(use-package evil-numbers
  :ensure t
  :config
  ;; increment/decrement numbers
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Https://stackoverflow.com/a/1110487
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

;; Enable smart-tab, i.e. expand word or indent
;; put yasnippet in hippie-expansion list
;; https://github.com/haxney/smart-tab/issues/1
(add-to-list 'hippie-expand-try-functions-list
	     'yas/hippie-try-expand)

(setq smart-tab-using-hippie-expand t)

(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode t))

;; (with-eval-after-load "mercurial.el"
;;   (defun my-hg-branch ()
;;     "Run the Mercurial 'branch' command."
;;     (interactive)
;;     ;; strip newline character from end
;;     (message "Current branch: %s" (substring (cdr (hg-run "branch")) 0 -1)))
;;   (define-key hg-global-map "b" 'my-hg-branch))

;; Disable mouse click on minibuffer from opening messages
(define-key minibuffer-inactive-mode-map [mouse-1] nil)

(set-face-attribute 'font-lock-doc-face nil :background 'unspecified :foreground "light blue")

(display-time)

(defun my-start-windows-cmd ()
  "Open external Windows cmd prompt.

Sets default directory to `my-global-default-directory'.  It is
assumed that `my-global-default-directory' is defined.

See URL `https://stackoverflow.com/a/13509208/5065796'."
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/K" "cd" my-global-default-directory)))
    (set-process-query-on-exit-flag proc nil)))

(global-set-key (kbd "<f12>") 'my-start-windows-cmd)

(defun my-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.

Source URL: `https://stackoverflow.com/a/384346'
Author URL: `http://steve.yegge.googlepages.com/my-dot-emacs-file'"
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

(defun my-info (&optional file-or-node buffer)
  "My reimplementation of info that opens in the other window."
  (interactive (list
		(if (and current-prefix-arg (not (numberp current-prefix-arg)))
		    (read-file-name "Info file name: " nil nil t))
		(if (numberp current-prefix-arg)
		    (format "*info*<%s>" current-prefix-arg))))
  (info-setup file-or-node
	      (pop-to-buffer (or buffer "*info*"))))

(defun my-elisp-index-search (topic)
  "Look up TOPIC in the indices of the Emacs Lisp Reference Manual."
  (interactive "sSubject to look up: ")
  (my-info "elisp")
  (Info-index topic))

(global-set-key (kbd "C-h R") 'my-elisp-index-search)

(defun my-term-send-command (command)
  "Send COMMAND to current terminal process.

Creates new terminal process if none exists.

See URL `https://stackoverflow.com/a/7053298/5065796'"
  (let ((proc (get-process "terminal"))
	pbuf)
    (unless proc
      (let ((currbuff (current-buffer)))
	(term "/bin/bash")
	(switch-to-buffer currbuff)
	(setq proc (get-process "terminal"))))
    (setq pbuff (process-buffer proc))
    (setq command-and-go (concat command "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command-and-go)
      (move-marker (process-mark proc) (point))
      )
    (process-send-string proc command-and-go)))

(with-eval-after-load "cc-mode"
  (add-hook 'c-mode-hook
	    (lambda ()
	      (set (make-local-variable 'compile-command)
		   (concat "gcc -std=c99 -Wall "
			   buffer-file-name
			   " -o "
			   (file-name-sans-extension (buffer-file-name))
			   ))))
  (define-key c-mode-map (kbd "<f7>") 'compile))

(defun my-compile ()
  "Compile and switch to compilation buffer."
  (interactive)
  (compile compile-command)
  (switch-to-buffer-other-window "*compilation*"))

;; Taken from: https://www.reddit.com/r/emacs/comments/44jwh3/better_compile_buffer/czte0lx?utm_source=share&utm_medium=web2x
;; Make the compilation window automatically disappear - from enberg on #emacs
;; maybe should use: https://github.com/EricCrosson/bury-successful-compilation
(setq compilation-finish-functions
      (lambda (bug str)
	(if (null (string-match "exited abnormally" str))
	    ;; no errors, make the compilation window go away in a few seconds
	    (progn
	      (run-at-time "0.4 sec" nil
			   (lambda ()
			     (select-window (get-buffer-window (get-buffer-create "*compilation*")))
			     (switch-to-buffer nil)
			     (end-of-line)))
	      (message "No compilation errors!")))))

(setq compilation-scroll-output t)

(add-hook 'term-mode-hook
	  (setq term-scroll-to-bottom-on-output t))

;; This will create multiple timers if run more than once
;; timers can be cancelled with list-timers and pressing 'c'
;; https://emacs.stackexchange.com/a/29007/15177
;; (run-with-idle-timer 30 t 'dimmer-mode)

;; (defun my-run-fkpiawh ()
;;   "Hack to run first-key-press-in-a-while-hook only once."
;;   (remove-hook 'pre-command-hook #'my-run-fkpiawh)
;;   (run-hooks 'first-keypress-in-a-while-hook))

;; (run-with-idle-timer 1 t (lambda ()
;;                               (add-hook 'pre-command-hook
;;                                         #'my-run-fkpiawh)))

;; (add-hook 'first-keypress-in-a-while-hook '(lambda () (dimmer-mode -1)))

;; change mode-line color by evil state
;; #ffffff is white, which is the text color
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                 (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;          (lambda ()
;;            (let ((color (cond ((minibufferp) default-color)
;;                               ((evil-emacs-state-p)  '("SkyBlue2" . "#ffffff"))
;;                               ((evil-normal-state-p) '("DarkGoldenrod2" . "#ffffff"))
;;                               ((evil-insert-state-p) '("light gray" . "#ffffff"))
;;                               ((evil-visual-state-p) '("gray" . "#ffffff"))
;;                               ((evil-motion-state-p) '("plum3" . "#ffffff"))
;;                               ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                               (t default-color))))
;;              (set-face-background 'mode-line (car color))
;;              (set-face-foreground 'mode-line (cdr color))))))

;; (use-package evil-leader
;;   :ensure t
;;   :init
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leader/set-key
;;    "s" 'save-buffer
;;    "f" 'find-file
;;    "b" 'helm-buffers-list
;;    "k" 'kill-buffer
;;    "e" 'eval-last-sexp
;;    "i" 'org-clock-in
;;    "o" 'org-clock-out
;;    "d" 'org-clock-display))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def
   :keymaps 'normal
    "b" 'helm-buffers-list
    "s" 'save-buffer
    "k" 'kill-buffer
    "f" 'find-file
    "y" '(lambda () (interactive)
	   (my-sh-send-command "python main.py problem.py"))
    "g" '(lambda () (interactive)
	   (my-sh-send-command "python generate.py problem.py"))

    )

  ;; (general-create-definer my-ledger-leader-def
  ;;   :prefix "SPC")

  ;; (my-ledger-leader-def
  ;;  :states 'normal
  ;;  :keymap 'ledger-mode-map
  ;;   "g" 'free-money-xact-add-generic
  ;;   "e" 'free-money-xact-expense
  ;;   "p" 'free-money-xact-loan-payment
  ;;   "l" 'free-money-xact-loan
  ;;   "n" 'free-money-xact-lending-loan
  ;;   "c" 'free-money-xact-lending-collect
  ;;   ";" 'free-money-toggle-comment-xact
  ;;   "r" 'ledger-report
  ;;   "b" 'ledger-display-balance-at-point
  ;;   "t" 'ledger-add-transaction
  ;;   "f" 'ledger-fully-complete-xact))

  (general-create-definer my-elisp-leader-def
    :prefix "SPC")

  (my-elisp-leader-def
   :states 'normal
   :keymap 'emacs-lisp-mode-map
   "=" 'er/expand-region
   "-" 'er/contract-region
   ";" 'comment-dwim-2
   "l" 'recenter-top-bottom
   "e" 'eval-last-sexp
   "x" 'eval-expression
   "d" 'eval-defun
   ;; "t" '(call-interactively 'ert t (vector t))
   ))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (elnode-make-webserver "~/projects/blog/ideal/" 8080)
;; (elnode-make-webserver "~/projects/blog/publish/" 8080)
;; (elnode-make-webserver "~/site/publish/" 8080)
;; (elnode-make-webserver "~/my-site/publish/" 8080)
;; (elnode-make-webserver "~/projects/peut-publier-site/publish/" 8080)
;; (elnode-stop 8080)

;; (peut-publier-publish
;;  (peut-publier-dir-list "~/site/src/" ".org$")
;;  "~/site/publish/"
;;  )

(setq ediff-split-window-function 'split-window-horizontally)

;; (use-package slime
;;   :ensure t
;;   :config
;;   (setq inferior-lisp-program "/data/data/com.termux/files/usr/bin/ecl")
;   (setq slime-contribs '(slime-fancy)))


;; (defun my-indent-region (mode)
;;   "Indent region according to mode see https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_node/elisp_467.html"
;;   )

;; (("Emacs Wiki" "https://www.emacswiki.org/emacs?action=rss" nil 3600))

;; (setq newsticker-url-list
;;   ("my-deal-blog" "~/projects/blog/ideal/rsx.xml"))

;; (setq newsticker-url-list-defaults
;;       (quote (
;;               ("my-deal-blog" "~/projects/blog/ideal/rsx.xml")
;;               ("slashdot" "http://slashdot.org/index.rss" nil 3600)
;;               ("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/emacs?action=rss;showedit=1")
;;               )))

(defun my-pp ()
  "Resume development."
  (interactive)
  ;; (elnode-make-webserver "~/site/publish/" 8080)
  (find-file "~/projects/blog/oss.el")
  (find-file "~/projects/peut-publier/README.org")
  (find-file "~/projects/peut-publier/README.md")
  (find-file "~/projects/peut-publier/peut-publier.el")
  (nameless-mode)
  (split-window-horizontally)
  (switch-window)
  ;; (find-file "~/projects/peut-publier/peut-publier-test.el")
  (find-file "~/projects/peut-publier/test/peut-publier-test.el")
  (nameless-mode)
  ;; (mapc #'find-file (directory-files-recursively "~/site/" "\\.org$" nil)

  ;; goal: a static site generator with no template language, free
  ;; from superfluos dependencies and required syntax, which tries not
  ;; to dictate how the user's site should look.
	)

;; (setq my-site-to-publish "/data/data/com.termux/files/usr/tmp/test-file.html")
;; (setq my-site-to-publish "/data/data/com.termux/files/home/projects/peut-publier/scratch/post.html")
;; (elnode-make-webserver my-site-to-publish 8080)

;; (org-html-htmlize-generate-css)
;; (setq org-html-htmlize-output-type 'css)
;; (setq org-tufte-include-footnotes-at-bottom t)

;; (defface ledger-font-default-face
;;   `((t :inherit default))
;;   "Pesky default font face"
;;   :group 'ledger-faces)

;; (defface magit-popup-key
;;   `((t :inherit default))
;;   "Pesky default font face")

(add-hook 'emacs-lisp-mode-hook 'toggle-truncate-lines)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package nameless
  :ensure t)

;; (defun no-junk-please-were-unixish ()
;;   "Enforce unix-like line endings.

;; See `https://www.emacswiki.org/emacs/EndOfLineTips'."
;;   (let ((coding-str (symbol-name buffer-file-coding-system)))
;;     (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
;;       (set-buffer-file-coding-system 'unix))))

;; (add-hook 'find-file-hooks 'no-junk-please-were-unixish)

;; (defun dos2unix ()
;;   "Convert a DOS formatted text buffer to UNIX format"
;;   (interactive)
;;   (set-buffer-file-coding-system 'undecided-unix nil))

;; (defun unix2dos ()
;;   "Convert a UNIX formatted text buffer to DOS format"
;;   (interactive)
;;   (set-buffer-file-coding-system 'undecided-dos nil))

(defmacro redefvar (symbol newval &optional doc)
  "Reset the value of a symbol which was created using `defvar'.
This function is intended only for development purposes.  Simply
replace `defvar' with `redefvar' and SYMBOL will be assigned
NEWVAL.  DOC will be ignored."
  `(setq ,symbol ,newval))

(defmacro redefcustom (symbol newval &optional doc)
  "Reset the value of a symbol which was created using `defvar'.
This function is intended only for development purposes.  Simply
replace `defcustom' with `redefcustom' and SYMBOL will be
assigned NEWVAL.  DOC will be ignored."
  `(setq ,symbol ,newval))

;; (add-to-list 'Info-additional-directory-list "/data/data/com.termux/files/home/storage/downloads/")

;(setq Info-default-directory-list (remove "/data/data/com.termux/files/home/storage/downloads/emacs.info.gz" Info-default-directory-list)))

(define-key Info-mode-map (kbd "a") 'info-apropos)

(defun my-pdf-render ()
(interactive)
(save-buffer)
  (let ((my-pdf (org-latex-export-to-pdf)))
    (my-sh-send-command (concat "termux-open " my-pdf " --view")

			)))

;; stupid global map helm clash
(define-key global-map (kbd "M-j") nil)

;; ;; make work
;; ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html
;; (defmacro my-timeit (&rest body n)
;;   "Measure average time to evaluate BODY over N iterations.

;; Default number of iterations is 10.

;; \(fn BODY &optional N)"

;;   `(let ((n (or n ,n))
;;          (start (current-time)))
;;      (dotimes (i n)
;;        ,@body)
;;      (message "%.06f" (float-time (/ (time-since start) n)))))

;; bottom matter
;; -------------
(message "The init.el has finished loading. See log for more details.")

;; (ert-deftest peut-publier-test-get-keyword-value ()
;;   "Test that values can be extracted from meta-data via key."
;;   (let* ((test-file (concat (temporary-file-directory) "test-file"))
;;          (result (progn
;;                    (with-temp-file test-file
;;                      (insert peut-publier-test-post))
;;                    (peut-publier-get-keyword-value "TITLE" test-file))))
;;     (delete-file test-file)
;;     (should (string-equal "Test post" result))))

(defun my-shell (name)
  "Create shell with NAME."
  (interactive "sName: ")
  (shell name)
  (switch-to-buffer name)
  )

(defun my-code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
	 (let ((file buffer-file-name)
	      ;; (default-directory (file-name-directory buffer-file-name))
)
       (format "g++ -std=c++11 -Wall -Wextra -pedantic-errors %s -o %s"
	       file
	       (file-name-sans-extension file))))
    (compile compile-command)))

(setq org-adapt-indentation nil)

;; (setq httpd-root "~/storage/movies/6-006-fall-2011/contents/assignments/ps1/")
;; (setq httpd-indexes '("index.html" "index.htm" "index.xml")) ; reset default
;; (setq httpd-indexes '("visualizer.html"))
;; (httpd-start)

(defun my-count-lines (&optional buffer)
  "Count the number of lines in the current BUFFER."
  (interactive)
  (let ((buffer (or buffer (buffer-name))))
  (message "%s lines in %s"
	   (count-lines (point-min) (point-max))
	   buffer)))

(add-hook 'after-load-functions 'my-count-lines)
