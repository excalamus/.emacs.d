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
;;   - [ ] Have only one instance of shell, if open, jump to it, if not, brind up a window with it.
;;   - [ ] comment-dwim2 comments three lines instead of two when using evil visual selction
;;   - [ ] Make my-find-file have a "start from directory of current file" when using universal
;;   - [ ] Create funtion to turn region into abbrev expansion (why the f doesn't this already exist?)
;;
;;; Customization:
;;   - [ ] Create function to clear all highlighting in buffer (repeated M-s h u)
;;   - [ ] (display-monitor-attributes-list)
;;   - [ ] Put dimmer-mode on a delay timer.  If you haven't typed anything for X
;;     minutes, enable dimmer-mode so that you know which buffer you're in.
;;     https://www.emacswiki.org/emacs/IdleTimers
;;   - [ ] Create mercurial mode that works!
;;   - [ ] Change hl-line color for inactive windows: https://emacs.stackexchange.com/a/14658/15177
;;   - Is it possible to adapt org named footnotes to render on export
;;     with different id/class html properties?  If so, making a
;;     margin/side note would become relatively trivial.
;;     https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
;;     https://github.com/alphapapa/unpackaged.el/issues/8
;;   - org captures clash with footnotes since they simply are appended to document
;;
;;; Over-the-shoulder
;;   - [ ] create-scratch-buffer: when called with universal, prompt for mode. otherwise use initial-major-mode
;;   - [ ] literate-org-mode: seemless code/text editing in org mode
;;         When in org src block, call org-edit-special automatically.
;;         This circumvents the multiple modes problem.  When editing
;;         src, if trying to go beyond buffer, up/down, exit editing
;;         buffer and return to main org file.  Also, Hide block
;;         delimeters automatically.
;;         https://emacs.stackexchange.com/questions/27467/way-to-hide-src-block-delimiters
;;   - [ ] formalize my-search-engine
;;   - [ ] make simple project management library out of my-activate-python and friends
;;   - [ ] capture snippets with org-store-link
;;   - [ ] example of emacs being nice: send text to shell
;;         Need to step through requitements.txt b/c python sucks
;;         https://stackoverflow.com/a/7053298
;;
;;         (defun my-send-line-or-region ()
;;           (interactive)
;;           (let ((proc (get-process "shell"))
;;                 pbuf min max command)
;;             (setq pbuff (process-buffer proc))
;;             (if (use-region-p)
;;                 (setq min (region-beginning)
;;                       max (region-end))
;;               (setq min (point-at-bol)
;;                     max (point-at-eol)))
;;             (setq command (concat "pip install \"" (buffer-substring min max)  "\"\n"))
;;             (with-current-buffer pbuff
;;               (goto-char (process-mark proc))
;;               (insert command)
;;               (move-marker (process-mark proc) (point))
;;               ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
;;             (process-send-string  proc command)
;;             ;; (display-buffer (process-buffer proc) t)
;;                   (goto-char max)
;;               (next-line)
;;             ))
;;
;;; Articles
;;  - rethinking literate programming: only works if everyone buys in.
;;    Otherwise collaboration is impossible.  It seems like a literate
;;    system like Org mode, WEB, LEO is tied to that system.  But what
;;    if you want to collaborate?  I don't see any systems for
;;    literate programming which are independent of the literate
;;    system.  It seems to me that a better approach would be to do
;;    literate programming in the source code itself through comments.
;;    The literate system would interpret block/contiguous comments
;;    per some mode, such as Org or markdown.  This would enable
;;    collaboration with non-literate programming styles. It might
;;    even be possible to keep comments adjacent/within to code
;;    blocks.  It seems like the only advantage a literate system
;;    provides is 1) the ability to work with multiple languagues and
;;    2) marking up the text.  The first doesn't really apply in
;;    practice, as you would never really tangle multiple languages.
;;    The second is all superficial.  The only advantages of a
;;    outside-in system is controlling code execution order.
;;    Evertythin else could be better handled through comments.  A toc
;;    is better handled through a def/class tree in IDE.  The LP
;;    system would need to be aware of comment sections.  From that
;;    the Literate part could be extracted.  Non-LP programmers could
;;    just hide comments.  It's a system basically like Doxygen, but
;;    rather than a custom sytax, just hook into whatever markup
;;    parser you want.  http://www.literateprogramming.com/lpfaq.pdf
;;    https://github.com/alphapapa/unpackaged.el#convert-elisp-to-org-format
;;
;;  - open source software: develop to step away.  It seems that the
;;    natural tendency of many programmers is to want to do
;;    maintenance forever.  Why is this?  Endless maintenance seems to
;;    be the primary reason behind FOSS burnout: people complaining
;;    and being rude, along with the ever increasing time demand.
;;    Does it need to be this way?  Is it any surprise this happens
;;    when code is designed to be maintained?  Can software be
;;    designed to be given away?  Consider a system like TeX.  TexLive
;;    continues to be developed, but apparently the core TeX system
;;    has been stable since 2008
;;    (https://en.wikipedia.org/wiki/TeX#Development).What can a
;;    developer ask themselves about a design which avoids Endless
;;    Maintenance?  Are there practices which allow for maintenance to
;;    be off-loaded to users? Plugins? Clear mission statement?
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
(if (eq system-type 'windows-nt)
    (setq load-path (remove "c:/Users/mtrzcinski/.emacs.d/elpa/undo-tree-0.6.5" load-path))
(setq load-path (remove "/home/ahab/.emacs.d/elpa/undo-tree-0.6.5" load-path)))

;; https://github.com/jwiegley/use-package#extending-the-load-path
;; for mercurial.el, hide-comnt.el
(if (eq system-type 'windows-nt)
    (add-to-list 'load-path "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; configure autosave directory
;; https://stackoverflow.com/a/18330742/5065796
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
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
(put 'narrow-to-region 'disabled nil)

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
(setq initial-major-mode 'emacs-lisp-mode)
;; (setq help-window-select t)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq show-help-function nil)
(set-default 'truncate-lines t)

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
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Only available in emacs-version >= 26.1
;; (global-display-line-numbers-mode)

;; This slows windows down...
;; (setq display-line-numbers-type 'relative)

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
(setq mode-line-position
            '((line-number-mode ("%l" (column-number-mode ":%c ")))
              (-3 "%p")))
(which-function-mode)

;; Just a hack, needs proper attention
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
                " "
                mode-line-end-spaces))

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
                     (org-mode)
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

;; Create a global default directory
(defvar my-global-default-directory "~/Documents/"
  "An Emacs-wide default directory.")

(if (eq system-type 'windows-nt)
    (setq my-global-default-directory "C:/projects/"))

(defun my-find-file (&optional filename)
  "Switch to a buffer visiting FILENAME, defaulting to `my-global-default-directory'."
  (interactive)
  (if (null filename)
      (setq filename my-global-default-directory))
  (cd my-global-default-directory)
  (call-interactively 'find-file filename))

(global-set-key (kbd "C-x C-f") 'my-find-file)

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

;; (defun my-activate-python-project (&optional project-dir venv-dir main-file switch-to)
;;   "Activate everything needed for a project

;; todo Make this more elegant."
;;   (interactive)
;;   (let*((project-dir (or project-dir
;;                          (file-name-as-directory
;;                           (read-directory-name "Project directory: " "c:/projects/" nil t))))
;;         (ms-project-dir (replace-regexp-in-string "/" "\\" project-dir t t))
;;         (venv-dir (or venv-dir
;;                       (file-name-as-directory
;;                        (read-directory-name "Venv directory: " project-dir nil 1 "venv/"))))
;;         (main-file (or main-file (read-file-name "Main file: " project-dir nil "confirm")))
;;         (cmd-cd (concat "cd " "\"" ms-project-dir "\""))
;;         (cmd-python (concat "python \"" main-file "\"")))
;;     (setq my-global-default-directory project-dir)
;;     (my-sh-send-command cmd-cd)
;;     (my-sh-send-command "deactivate")
;;     (my-sh-send-command (concat venv-dir "Scripts/activate.bat"))
;;     (pyvenv-activate venv-dir)
;;     (setq my-global-shell-command cmd-python)
;;     (global-set-key (kbd "<f7>") (lambda() (interactive) (my-sh-send-command my-global-shell-command)))
;;     ;; (delete-other-windows)
;;     (find-file-noselect main-file)
;;     (if switch-to
;;         (switch-to-buffer (get-file-buffer main-file)))
;;     (message "Project loaded!")))
;; )

(defun my-activate-python-project (&optional top-dir venv-dir project-dir main-file) ; switch-to)
  "Activate everything needed for a project

todo Make this more elegant."
  (interactive)
  (let*((top-dir (or top-dir
                         (file-name-as-directory
                          (read-directory-name "Top project directory: " "c:/projects/" nil t))))
        (ms-top-dir (replace-regexp-in-string "/" "\\" top-dir t t))
        (venv-dir (or venv-dir
                      (file-name-as-directory
                       (read-directory-name "Venv directory: " top-dir nil 1 "venv/"))))
        (project-dir (or project-dir
                         (file-name-as-directory
                          (read-directory-name "Project-dir: " top-dir nil 1
                                               (concat (car (last (split-string top-dir "/" t " "))) "/" )))))
        (main-file (or main-file (read-file-name "Main file: " top-dir nil "confirm")))
        (cmd-cd (concat "cd " "\"" ms-top-dir "\""))
        (cmd-python (concat "python \"" main-file "\"")))
    (setq my-global-default-directory top-dir)
    (my-sh-send-command cmd-cd)
    (my-sh-send-command "deactivate")
    (my-sh-send-command (concat venv-dir "Scripts/activate.bat"))
    (pyvenv-activate venv-dir)
    (setq my-global-shell-command cmd-python)
    (global-set-key (kbd "<f7>") (lambda() (interactive) (my-sh-send-command my-global-shell-command)))
    (delete-other-windows)
    (my-open-all-files-in-dir project-dir ".py")
    (split-window-horizontally)
    (switch-to-buffer "*shell*" nil t)
    (find-file-noselect main-file)
    ;; (if switch-to
    (switch-to-buffer-other-window (get-file-buffer main-file))
    (message "Project loaded!")))

  )

(defalias 'my-ap 'my-activate-python-project)

;; todo make callable from elisp like the windows version
(when (eq system-type 'gnu/linux)
  (defun my-activate-python-project ()
    "Activate everything needed for a project"
    (interactive)
    (let*((project-dir
           (file-name-as-directory
            (read-directory-name "Project directory: " "~/Projects/" nil t)))
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

(defun my-switch-to-tests ()
  "Attempt to switch to test buffer.

Should probably use URL `https://emacs.stackexchange.com/a/47628/15177'"
  (interactive)
  (let ((testfile "test_parser.py"))
    (cond
     ((string-equal (buffer-name (current-buffer)) testfile)
      (progn
        (goto-char (point-max))
        (message "Already on tests!")))
     ((get-buffer-window testfile t)
      (progn
        (switch-to-buffer-other-frame testfile)
        ;; (goto-char (point-max))
        ))
     ((get-buffer testfile)
      (message "The tests buffer is not currently visible!"))
     ((message "No tests_parser.py buffer exists!")))))

(global-set-key (kbd "<C-f8>") 'my-switch-to-tests)

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
  :init
  (require 'bm)
  (setq bm-cycle-all-buffers t))

(use-package comment-dwim-2
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'comment-dwim-2)
  ;; (global-set-key (kbd "M-;") 'comment-dwim-2)
  )

(use-package define-word
  :ensure t)

(use-package dimmer
  :ensure t
  ;; :bind ("<f9>" . dimmer-mode)
  :config
  (setq dimmer-fraction 1.00)
  ;; When first enabled, dimmer-mode requires the buffer to be switched
  ;; before it takes effect.  There may be a better way, but I couldn't
  ;; find it!
  (add-hook 'dimmer-mode-hook (lambda ()
                                (progn (previous-buffer) (next-buffer))
                                )))

(use-package dumb-jump
  :ensure t)

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
  ;; color docstings differently than strings
  ;; https://stackoverflow.com/questions/27317396/how-to-distinguish-python-strings-and-docstrings-in-an-emacs-buffer
  (setq py-use-font-lock-doc-face-p t)
  ;; (set-face-attribute 'py-XXX-tag-face nil :foreground "red")

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

  ;; For some reason, simple :bind doesn't work
  ;; https://emacs.stackexchange.com/a/46251/15177
  (unbind-key "<C-return>" elpy-mode-map)
  (unbind-key "<C-S-return>" elpy-mode-map)
  (bind-keys :map elpy-mode-map
             ("<C-S-return>". elpy-shell-send-statement)
             ("<C-return>" . elpy-shell-send-statement-and-step)
             ("<M-return>" . elpy-shell-send-group)
             ("<M-S-return>" . elpy-shell-send-group-and-step)
             ("<pause>" . elpy-test-pytest-runner)
             ("C-c o" . elpy-occur-definitions)
             ;; ("<insert>" . elpy-test-pytest-runner)
             ("C-j" . helm-swoop)
             ("C-M-j" . helm-swoop-without-pre-input)
             ("M-j" . helm-semantic-or-imenu)
             ("C-c C-j" . helm-semantic-or-imenu)))

(use-package helm
  :ensure t
  :bind
  (("C-x b" . helm-buffers-list)
   :map helm-map
   ("<escape>" . helm-keyboard-quit)))

;; (use-package evil-leader
;;   :ensure t
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leader/set-key "f" 'my-find-file)
;;   (evil-leader/set-key "k" 'kill-buffer)
;;   (evil-leader/set-key "s" 'save-buffer)
;;   (evil-leader/set-key "o" 'ace-window)
;;   (evil-leader/set-key "b" 'helm-buffers-list)
;;   (evil-leader/set-key "m" 'helm-semantic-or-imenu)
;;   (evil-leader/set-key "z" 'evil-emacs-state)
;;   (evil-leader/set-key "n" 'my-string-inflection-style-cycle)
;;   (evil-leader/set-key "d" 'elpy-occur-definitions)
;;   (evil-leader/set-key "c" 'comment-dwim-2)
;;   (evil-leader/set-key "i" '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))
;;   )

 (use-package neotree
   :ensure t
   :config
   (global-set-key (kbd "C-x M-f") 'neotree-toggle))

(use-package evil
  :ensure t
  ;; FIXME: after was not working, so moved those loads above evil
  :after (:all neotree)
  :config
  (evil-mode)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "ww" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "ww" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "ww" 'evil-normal-state)

  ;; (key-chord-define evil-emacs-state-map ".." 'my-string-inflection-style-cycle)
  ;; (key-chord-define evil-normal-state-map ".." 'my-string-inflection-style-cycle)

  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map "\C-n" 'elpy-nav-forward-block)
  (define-key evil-normal-state-map "\C-p" 'elpy-nav-backward-block)
  ;; (define-key evil-normal-state-map (kbd "<insert>") 'elpy-test-pytest-runner)

  ;; qwerty bindings
  (define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "C-}") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "M-]") 'dumb-jump-go)
  (define-key evil-normal-state-map (kbd "M-}") 'dumb-jump-go-other-window)
                                        
  ;; ;; dvp bindings
  ;; (define-key evil-normal-state-map (kbd "C-@") 'xref-find-definitions)
  ;; (define-key evil-normal-state-map (kbd "C-^") 'xref-find-definitions-other-window)
  ;; (define-key evil-normal-state-map (kbd "M-@") 'dumb-jump-go)
  ;; (define-key evil-normal-state-map (kbd "M-^") 'dumb-jump-go-other-window)

  (define-key evil-normal-state-map (kbd "gd") 'dumb-jump-go)
  ;; (define-key evil-normal-state-map (kbd "C-o") 'dumb-jump-back)

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
  ;; Coordinate states with cursor color
  (setq evil-emacs-state-cursor '("SkyBlue2" bar))
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("light gray" bar))
  (setq evil-visual-state-cursor '("gray" box))
  (setq evil-motion-state-cursor '("plum3" box))
  )

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
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package flycheck
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("C-j" . helm-swoop)
         ("M-j" . helm-semantic-or-imenu)
         ("C-M-j" . helm-swoop-without-pre-input))
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

  (defface my-hi-comint
    '((t (:background "dim gray")))
    "Face for comint mode."
    :group 'hi-lock-faces)

  (set-face-attribute 'hi-yellow nil :background "yellow3" :foreground "gray30" :distant-foreground "yellow3 "    :box "dim gray")
  (set-face-attribute 'hi-pink   nil                       :foreground "gray30" :distant-foreground "pink"        :box "dim gray")
  (set-face-attribute 'hi-green  nil                       :foreground "gray30" :distant-foreground "light green" :box "dim gray")
  (set-face-attribute 'hi-blue   nil                       :foreground "gray30" :distant-foreground "light blue " :box "dim gray"))

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

;; On Windows, you need to get Hunspell and it's sketchy.  Config
;; taken from: https://emacs.stackexchange.com/a/22311
;; A source: https://sourceforge.net/projects/hunspell/
(use-package ispell
  :if (eq system-type 'windows-nt)
  :config
  (setq ispell-program-name "C:/Program Files (x86)/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

(when (eq system-type 'gnu/linux)
  (use-package ledger-mode
    :ensure t)

  (use-package evil-ledger
    :ensure t
    :after ledger-mode
    :config
    (setq evil-ledger-sort-key "S")
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

(when (eq system-type 'windows-nt)
  (use-package ledger-mode
    :ensure t
    :init
    (add-hook 'ledger-mode-hook #'turn-off-auto-fill)
    :config
    (custom-set-variables '(ledger-post-amount-alignment-column 70))
    (setq ledger-binary-path "C:/Users/mtrzcinski/Downloads/ledger_3.1.2_win_bin/ledger.exe")))

 ;; (use-package linum-relative
 ;;   :ensure t
 ;;   :config
 ;;   (linum-relative-global-mode 1))

 (use-package magit
   :ensure t
   :bind
   (("S-<tab>" . magit-section-cycle-global))
   :init
   (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide))))

 ;; Abandoned package(?) that works better than all the others. It is
 ;; not available in any of the repos.  Must be installed manually.
 ;; http://hg.intevation.org/mercurial/file/tip/contrib/mercurial.el
 (use-package mercurial
   :load-path "~/.emacs.d/lisp/")

 (use-package nov
   :ensure t
   :config
   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

 ;; At the time of writing, there is no easy way to automatically install
 ;; the latest version of org-mode, even though it is on the gnu repo.
 ;; It's simply easiest to install it manually via list-packages.
 (use-package org
   :pin gnu
   :bind
   ;; default C-x o is other-window
   ;; default C-x C-o is delete-blank-lines
   (("C-c h o" . org-clock-out)
    ("C-c h i" . org-clock-in)
    ("C-c h d" . org-clock-display))
   :config
   (setq org-edit-src-content-indentation 0)
   (setq org-src-tab-acts-natively t)
   (setq org-src-fontify-natively t)
   (setq org-confirm-babel-evaluate nil)
   (setq org-support-shift-select 'always)
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
   ;;
   (when (eq system-type 'gnu/linux)
     (org-babel-do-load-languages
      'org-babel-load-languages
      (append org-babel-load-languages
              '(
                (sh . t) ;; for Emacs < 26.1
                ))))
   ;;
   (when (eq system-type 'windows-nt)
     (org-babel-do-load-languages
      'org-babel-load-languages
      (append org-babel-load-languages
              '(
                (shell . t) ;; for Emacs == 26.1
                (plantuml . t)
                )))
     (setq org-plantuml-jar-path (expand-file-name "C:/plantUML/plantuml.jar"))
     (setq org-startup-with-inline-images t)

     (setq plantuml-jar-path org-plantuml-jar-path)
     (setq plantuml-default-exec-mode 'jar)

     (defun my-org-babel-after-exec-hook ()
         "Things to do in the hook."

        (let ((compilation-buffer "*compilation*")
            (babel-error-buffer "*Org-Babel Error Output*"))

        (if (get-buffer compilation-buffer)
            (kill-buffer compilation-buffer))

        (if (get-buffer babel-error-buffer)
            (kill-buffer babel-error-buffer)))

        ;; (my-switch-to-last-window)

        (org-redisplay-inline-images))

     ;; Kill the hook if you have too many images
     ;; (setq org-babel-after-execute-hook nil)

     (add-hook 'org-babel-after-execute-hook #'my-org-babel-after-exec-hook))
     ;; (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))

   (defun my-new-clock-task ()
     Switch to new task by clocking in after clocking out.
     (interactive)
     (org-clock-out)
     (org-clock-in))

   ;; (define-key org-mode-map (kbd "<pause>") 'my-new-clock-task)
   (define-key org-mode-map (kbd "C-c C--") 'org-ctrl-c-minus)

   ;; org-mode doesn't automatically save archive files for some
   ;; stupid reason.  This is a ruthless hack which saves /all/ org
   ;; buffers on archive.
   ;;
   ;; https://emacs.stackexchange.com/a/51112/15177
   (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)

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

 (use-package string-inflection
   :ensure t
   :config
   ;; (defun my-string-inflection-style-cycle-function (str)
   ;;   "foo_bar => fooBar => foo_bar"
   ;;   (cond
   ;;    ;; foo_bar => fooBar
   ;;    ((string-inflection-underscore-p str)
   ;;     (string-inflection-camelcase-function str))
   ;;    ;; fooBar => foo_bar
   ;;    ((string-inflection-camelcase-p str)
   ;;     (string-inflection-underscore-function str))))

   (defun my-string-inflection-style-cycle-function (str)
     "foo-bar => foo_bar => fooBar => FooBar => foo-bar"
     (cond
      ;; foo-bar => foo_bar
      ((string-inflection-kebab-case-p str)
       (string-inflection-underscore-function str))
      ;; foo_bar => fooBar
      ((string-inflection-underscore-p str)
       (string-inflection-camelcase-function str))
      ;; fooBar => FooBar
      ((string-inflection-camelcase-p str)
       (string-inflection-pascal-case-function str))
      ;; FooBar => foo-bar
      ((string-inflection-pascal-case-p str)
       (string-inflection-kebab-case-function str))))

   (defun my-string-inflection-style-cycle ()
     "foo_bar => FOO_BAR => FooBar => foo_bar"
     (interactive)
     (string-inflection-insert
      (my-string-inflection-style-cycle-function (string-inflection-get-current-word))))

   (global-set-key (kbd "M-<SPC>") 'my-string-inflection-style-cycle))


 ;; (use-package switch-window
 ;;   :ensure t
 ;;   :bind
 ;;   ;; default C-x o is other-window
 ;;   ;; default C-x C-o is delete-blank-lines
 ;;   (("C-x o" . switch-window)
 ;;    ("C-x C-o" . switch-window))
 ;;   :config
 ;;   (setq switch-window-multiple-frames t)
 ;;   (setq switch-window-shortcut-style 'qwerty)
 ;;   ;; when emacs is run as client, the first shortcut does not appear
 ;;   ;; "x" acts as a dummy
 ;;   (setq switch-window-qwerty-shortcuts '("a" "x" "s" "d" "f" "j" "k" "l" ";" "w" "e" "r" "u" "i" "o" "q" "t" "y" "p"))
 ;;   (setq switch-window-increase 3))

;; hooks
;; ----
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun my-comint-exec-hook ()
  (interactive)
  (highlight-lines-matching-regexp "-->" 'my-hi-comint)
  (setq comint-scroll-to-bottom-on-output t)
  (setq truncate-lines t)
  (set-window-scroll-bars (get-buffer-window "*shell*") nil nil 10 'bottom))

(add-hook 'comint-exec-hook #'my-comint-exec-hook)


;; (set-window-scroll-bars (get-buffer-window (current-buffer)) nil nil nil nil)

;; (add-hook 'comint-exec-hook (setq comint-scroll-to-bottom-on-output t))
;; (add-hook 'comint-exec-hook (lambda () (setq toggle-truncate-lines t)))

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

(add-hook 'compilation-mode-hook
          '(lambda ()
             (switch-to-buffer-other-window "*compilation*")))

;; staging
;; -------
;;; Code that is not yet fully tested and awaits integration
;;; into the main workflow

;; Taken from customed-set-faces.  This needs to be implemented without
;; customize
;; '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
;; '(term-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
;; '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
;; '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
;; '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
;; '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
;; '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
;; '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
;; '(term-default-bg-color ((t (:inherit term-color-black))))
;; '(term-default-fg-color ((t (:inherit term-color-white)))))

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

(define-key comint-mode-map (kbd "<M-left>") (lambda() (interactive) (progn (comint-clear-buffer) (process-send-string "*shell*" "s\n"))))
(define-key comint-mode-map (kbd "<M-right>") (lambda() (interactive) (progn (comint-clear-buffer) (process-send-string "*shell*" "n\n"))))
(define-key comint-mode-map (kbd "<M-home>") (lambda() (interactive) (progn (comint-clear-buffer) (process-send-string "*shell*" "r\n"))))
(define-key comint-mode-map (kbd "<M-S-home>") (lambda() (interactive) (progn (comint-clear-buffer) (process-send-string "*shell*" "c\n"))))

;; (define-key comint-mode-map (kbd "<f5>") (lambda() (interactive) (process-send-string "*shell*" "N\n")))
;; (define-key comint-mode-map (kbd "<f6>") (lambda() (interactive) (process-send-string "*shell*" "S\n")))

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

;; could be set up to use pyvenv-virtual-env
(defvar my-python
  (concat
   "python"
   ;; "C:/python/python3.6.8-64/python.exe"
   " "
   )
  "Python interpreter to be used in shell calls.")

(defun my-buffer-file-to-shell ()
  "Send current buffer file to shell as python call."
  (interactive)
  (my-sh-send-command (concat my-python (buffer-file-name))))

(defun my-smart-buffer-file-to-shell ()
  "Send current buffer file to shell."
  (interactive)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond ((string-equal ext "py")
           (my-sh-send-command (concat my-python (buffer-file-name))))
          ((string-equal ext "c")
           (my-sh-send-command (concat "cl.exe /W4 " (buffer-file-name)))))))

(global-set-key (kbd "<S-f7>") 'my-smart-buffer-file-to-shell)
(global-set-key (kbd "<S-f10>") '(lambda() (interactive)
                                   (save-some-buffers t nil)
                                   (my-kill-python)
                                   (my-smart-buffer-file-to-shell)))

(defun my-reset-global-shell-to-current-file ()
  "Reset the global shell command to use the current file.

This is useful if, for instance, a project was started using one
file, but later in development another file needs to be called
frequently.  It is like a permanent version of
`my-buffer-file-to-shell'."
  (interactive)
  (setq my-global-shell-command (concat my-python (buffer-file-name)))
  (message "Set `my-global-shell-command' to \"%s\"" my-global-shell-command))

(global-set-key (kbd "<C-S-f10>") 'my-reset-global-shell-to-current-file)

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

;; increment/decrement numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


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

(global-smart-tab-mode t)

;; (setq smart-tab-using-hippie-expand nil)

(global-set-key (kbd "<S-tab>") 'undo)

;; (add-to-list 'smart-tab-completion-functions-alist
;;              '(shell-mode . completion-at-point))

(with-eval-after-load "mercurial.el"
  (defun my-hg-branch ()
    "Run the Mercurial 'branch' command."
    (interactive)
    ;; strip newline character from end
    (message "Current branch: %s" (substring (cdr (hg-run "branch")) 0 -1)))
  (define-key hg-global-map "b" 'my-hg-branch))

;; Disable mouse click on minibuffer from opening messages
(define-key minibuffer-inactive-mode-map [mouse-1] nil)

(display-time)

(defun my-start-windows-cmd ()
  "Open external Windows cmd prompt.

Sets default directory to `my-global-default-directory'.  It is
assumed that `my-global-default-directory' is defined.

See URL `https://stackoverflow.com/a/13509208/5065796'."
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/K" "cd" my-global-default-directory)))
    (set-process-query-on-exit-flag proc nil)))

(defun my-start-windows-cmd-python-venv ()
  "Open external Windows cmd prompt.

Sets default directory to `my-global-default-directory'.  It is
assumed that `my-global-default-directory' is defined.

See URL `https://stackoverflow.com/a/13509208/5065796'."
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/K"
                             (concat "cd " my-global-default-directory " & venv\\Scripts\\activate"))))
    (set-process-query-on-exit-flag proc nil)))

(global-set-key (kbd "<f12>") 'my-start-windows-cmd-python-venv)

(defun my-open-windows-explorer ()
  "Open Windows Explorer to folder containing file."
  (interactive)
  (let* ((file (or (buffer-file-name (current-buffer)) my-global-default-directory))
         (dir (file-name-directory file))
         (windows-dir (subst-char-in-string ?/ ?\\ dir)))
    (start-process "explorer" nil "explorer.exe" windows-dir)))

(global-set-key (kbd "C-<f12>") 'my-open-windows-explorer)

;; ;; https://emacs.stackexchange.com/questions/7742/what-is-the-easiest-way-to-open-the-folder-containing-the-current-file-by-the-de
;; ;; protect against spaces
;; ;; use Explorer.exe /select,"C:\Demo\SS64App.exe"
;; (defun se-open-windows-explorer ()
;;   "Open Windows Explorer to folder containing buffer file.

;; If no file is found, try default directory.  If that does not
;; exist, just open Explorer.

;; For more information on explorer.exe, See URL
;; `https://ss64.com/nt/explorer.html'"
;;   (interactive)
;;   (let* ((default (or default-directory ""))                     ; explorer.exe fails when passed nil
;;          (file (or (buffer-file-name (current-buffer)) default)) ; try to get file buffer is visiting
;;          (dir (file-name-directory file))                        ; extract the directory
;;          (windows-dir (subst-char-in-string ?/ ?\\ dir))         ; convert to windows slashes
;;          (safe-dir (concat "\"" windows-dir "\"")))              ; windows people use spaces in their paths
;;     (message "%s" safe-dir)))
;;     (start-process "explorer" nil "explorer.exe" safe-dir)))  ; pass directory to be opened by explorer

(defun my-delete-process-interactive ()
  "Interactively delete the selected process.

Presents user with a list of active processes.  The selected
process will be killed.

See URL `https://stackoverflow.com/a/11573495/5065796'"
  (interactive)
  (let ((pname (ido-completing-read "Delete process: "
                                    (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))

(defun my-open-all-files-in-dir (dirname &optional type)
  "Open all files with EXTENSION in DIRNAME.

Optional regex TYPE to open.

See URL `https://emacs.stackexchange.com/a/46480/15177'"

  (interactive "DOpen files in: ")
  (let ((type (or type "\\.py$"))
        (dirname (or dirname my-global-default-directory)))
    (mapc #'find-file (directory-files dirname t type nil))))

(defun my-recursive-open (dirname &optional ext)
  "Open all files in DIRNAME with EXT extension.  Default EXT is '.py'.

See URL `https://emacs.stackexchange.com/a/46480'"
  (interactive "DRecursively open dir: ")
  (unless ext (setq ext "py"))
  (let ((regexp (concat "\\." ext "$")))
    (mapc #'find-file (directory-files-recursively dirname regexp nil))))

(setq compilation-scroll-output 'first-error)

(defun my-right-justify-rectangle (start end)
  "Right justify the contents of rectangle selection.

See URL `https://stackoverflow.com/a/10916207'"
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (apply-on-rectangle (lambda (c0 c1)
                          (move-to-column c1 t)
                          (let ((start (- (point) (- c1 c0)))
                                (end (point)))
                            (when (re-search-backward "\\S-" start t)
                              (transpose-regions start (match-end 0)
                                                 (match-end 0) end))))
                        start end))
  (when indent-tabs-mode (tabify start end)))

;; embrace-add-pair is only buffer-local for some asinine reason.
;; This needs to be fixed to be global.  Currently just placing it in
;; the python-mode-hook.  Note that embrace-commander can be used to
;; create and destroy pairs.
(use-package evil-embrace
  :ensure evil-embrace
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration))

(defun my-embrace-pairs ()
  "Define custom evil-embrace pairs.

This is necessary because the package is stupid and doesn't have
a global config, requiring you to add a hook to every goddamn
mode."
  (embrace-add-pair ?p "('" "')")
  (embrace-add-pair ?P "(\"" "\")")
  (embrace-add-pair ?r "['" "']")
  (embrace-add-pair ?R "[\"" "\"]"))

(add-hook 'python-mode-hook 'my-embrace-pairs)
(add-hook 'comint-mode-hook 'my-embrace-pairs)

;; (add-hook 'python-mode-hook (lambda ()
;;                               (embrace-add-pair ?p "('" "')")
;;                               (embrace-add-pair ?P "(\"" "\")")
;;                               (embrace-add-pair ?r "['" "']")
;;                               (embrace-add-pair ?R "[\"" "\"]")))

(defun my-info (&optional file-or-node buffer)
  "A hacky reimplementation of `info' which instead opens in the other window."
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

;; (use-package paredit
;;    :ensure t
;;    :init
;;    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

;;    :config
;;    ;; Ensure C-j is helm-swoop
;;    (define-key paredit-mode-map "\C-j" nil))

(when (eq system-type 'windows-nt)
  (defun my-activate-c-project ()
    "Activate everything needed for a C project."
    (interactive)
    (let*((project-dir
           (file-name-as-directory
            (read-directory-name "Project directory: " "c:/projects/" nil t)))
          (ms-project-dir (replace-regexp-in-string "/" "\\" project-dir t t))
          (main-file (read-file-name "Main file: " project-dir nil "confirm"))
          (cmd-cd (concat "cd " "\"" ms-project-dir "\""))
          (cmd-compile (concat "cl.exe /W4 " main-file)))
      (setq my-global-default-directory project-dir)
      (my-sh-send-command (concat "\"C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\Common7\\Tools\\LaunchDevCmd.bat\""))
      (my-sh-send-command cmd-cd)
      (setq my-global-shell-command cmd-compile)
      (global-set-key (kbd "<f7>") (lambda() (interactive) (my-sh-send-command my-global-shell-command)))
      (delete-other-windows)
      (find-file main-file)
      (switch-to-buffer (get-file-buffer main-file))
      )))

;; Remap the key that's next to the Fn key
(global-set-key (kbd "C-<apps>") #'(lambda () (interactive) (insert "µ")))

;; Note that scrolling is possible in Vim via zl, zh, zL, and zH.
;; https://stackoverflow.com/a/1249665
(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

;; Toggle line numbers
(global-set-key (kbd "C-c C-x l") 'linum-mode)
(global-set-key (kbd "C-c C-x C-l") 'linum-mode)

(global-set-key (kbd "C-c C-x r") 'linum-relative-mode)
(global-set-key (kbd "C-c C-x C-r") 'linum-relative-mode)

;; (global-set-key (kbd "C-<f12>") 'org-clock-goto)

(defvar my-on-demand-window nil
  "Target on-demand window.

An on-demand window is one which you wish to return to within the
current Emacs session but whose importance doesn't warrant a
permanent binding.")

(defvar my-on-demand-buffer nil
  "Target on-demand buffer.")

(defun my-on-demand-window-set ()
  "Set the value of the on-demand window to current window."
  (interactive)
  (setq my-on-demand-window (selected-window))
  (setq my-on-demand-buffer (current-buffer))
  (message "Set on-demand window to: %s" my-on-demand-window))

(defun my-on-demand-window-goto ()
  "Goto MY-ON-DEMAND-WINDOW with MY-ON-DEMAND-BUFFER."
  (interactive)
  (let ((win my-on-demand-window))
    (unless win (error "No on-demand window set! See `my-on-demand-window-set'."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win)
      (switch-to-buffer my-on-demand-buffer))))

(global-set-key (kbd "<f1>") 'my-on-demand-window-goto)  ; f1 => ondemand window
(global-set-key (kbd "S-<f1>") 'my-on-demand-window-set)  ; "set" ondemand window

;; elpy-test-pytest-runner-command
;; (funcall '(lambda () (interactive) (if current-prefix-arg 'true 'false)))


;; (defun my-see-shell-history ()
;;   "Open comint command history and switch to *Input History* buffer.

;; A wrapper around COMINT-DYNAMIC-LIST-INPUT-RING."
;;      (interactive)
;;      (comint-dynamic-list-input-ring)
;;      (switch-to-buffer-other-window "*Input History*"))

;; dim-mode after 30 seconds; disable dimmer on first keypress
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

(defun my-kill-all-visiting-buffers (dir)
  "Kill all buffers visiting DIR."
  (interactive)
  (mapcar
   (lambda (buf)
     (let ((bfn (buffer-file-name buf)))
       (and (not (null bfn))
            (string-match-p (regexp-quote dir) (file-name-directory bfn))
            (kill-buffer buf))))
   (buffer-list)))

(defun my-deactivate-project ()
  "Deactivate project by killing all buffers visiting the global
default directory."
  (interactive)
  (my-kill-all-visiting-buffers my-global-default-directory)
  (message "Project deactivated."))

;; (require 'cl-seq)

;; ;; Create a global default directory
;; (defvar my-global-default-directory "~/Documents/"
;;   "An Emacs-wide default directory.")

;; (if (eq system-type 'windows-nt)
;;     (setq my-global-default-directory "C:/projects/"))

;; ;; List to check if new buffers have been created
;; (defvar my-buffer-list (buffer-list)
;;   "Master record of the buffer-list.")

;; ;; change default-directory on newly created buffers
;; (defun my-reset-default-directory ()
;;   "Change default-directory on new buffers to
;; MY-GLOBAL-DEFAULT-DIRECTORY."
;;   (interactive)
;;   (let ((new-buffs (cl-set-difference (buffer-list) my-buffer-list)))
;;     (mapc (lambda (buffer)
;;             (with-current-buffer buffer
;;               (cd my-global-default-directory)))
;;           new-buffs)
;;   (my-update-my-buffer-list)))

;; ;; Any time the buffer list updates (a new buffer is created), change
;; ;; its default directory
;; (add-hook 'buffer-list-update-hook 'my-reset-default-directory)

;; ;; Ensure that the list used for difference is kept in sync with
;; (defun my-update-my-buffer-list ()
;;   "Update MY-BUFFER-LIST to match current buffer-list."
;;   (interactive)
;;   (setq my-buffer-list (buffer-list)))

;; (add-hook 'kill-buffer-hook 'my-update-my-buffer-list)

(defun my-create-shell ()
    "Create shell with a given name.

Taken from URL `https://stackoverflow.com/a/36450889/5065796'"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(use-package ace-window
  :ensure t
  :bind
  ;; default C-x o is other-window
  ;; default C-x C-o is delete-blank-lines
  (("C-x o" . ace-window)
   ("C-x C-o" . ace-window))
  :init
  (require 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

;;; Theme hooks
;; https://www.greghendershott.com/2017/02/emacs-themes.html
(use-package base16-theme
  :ensure t)

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
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (setq evil-insert-state-cursor '("gray" bar))
  (set-face-attribute 'hl-line nil :background "gray29" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "gray40")
  (set-face-attribute 'bm-face nil :background "RoyalBlue4" :foreground 'unspecified)
  (set-face-attribute 'my-hi-comint nil :background "dim gray")
  )

(defun my-light-theme-hook ()
  "Run after loading light theme."
  ;; base16-tomorrow
  (set-face-attribute 'aw-leading-char-face nil :background 'unspecified :foreground "#CC9393" :height 3.0)
  (set-face-attribute 'hl-line nil :background "gray96" :foreground 'unspecified)
  (set-face-attribute 'mode-line nil :background "light gray")
  (set-face-attribute 'mode-line-inactive nil :background "white smoke")
  (set-face-attribute 'org-mode-line-clock nil :background "white" :inherit nil)
  (set-face-attribute 'bm-face nil :background "light cyan" :overline 'unspecified :foreground 'unspecified)
  (set-face-attribute 'my-hi-comint nil :background "light gray")
  ;; (set-face-attribute 'font-lock-comment-face nil :background 'unspecified :foreground "gray63")

  ;; leuven
  ;; (setq evil-insert-state-cursor '("dim gray" bar))
  ;; (set-face-attribute 'font-lock-doc-face nil :foreground "cornflower blue")
  ;; (set-face-attribute 'hl-line nil :background "gray96" :foreground 'unspecified)
  ;; (set-face-attribute 'font-lock-comment-face nil :background 'unspecified :foreground "gray63")
  ;; (set-face-attribute 'bm-face nil :background "light cyan" :overline 'unspecified :foreground 'unspecified)

  ;; misc
  ;; (set-face-attribute 'region nil :background "LightSkyBlue1" :foreground 'unspecified)
  ;; (set-face-attribute 'mode-line nil :background "lavender")
  ;; (set-face-attribute 'mode-line-inactive nil :background 'unspecified)
  )

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

(my-theme-toggle)

;; split ediff vertically
(setq ediff-split-window-function 'split-window-right)

(use-package org-download
  :ensure t
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)

  (define-key org-mode-map (kbd "C-S-y") 'org-download-yank))

(setq my-search-engines
      '(
        (("pyside2" "pys2") "https://doc.qt.io/qtforpython/search.html?check_keywords=yes&area=default&q=")
        (("pyside1.2.1" "pys") "https://pyside.github.io/docs/pyside/search.html?q=")
        ;; (("duckduckgo" "ddg") "https://duckduckgo.com/?q=%s")
        (("duckduckgo" "ddg") "https://duckduckgo.com/?q=")
        ))

(setq my-search-engine-default "duckduckgo")

(defun my-search-get-engine-name (engine-name engine-list)
  (cond
   ((null engine-list) nil)
   ((member engine-name (caar engine-list)) (cadar engine-list))
   (t (my-search-get-engine-name engine-name (cdr engine-list)))))

(defun my-search-engine (engine-name term)
  "Search for a term using an engine.

Needs to be updated with `completing-read' for mini-buffer
completion.  See URL
`https://gist.github.com/brenns10/69d39f6c46170093f73d'
"
  (interactive "MEngine: \nMTerm: ")
  (let* ((url (my-search-get-engine-name engine-name my-search-engines)))
    (if (equal url nil)
        (message "Error: search engine \"%s\" unknown." engine-name)
      (browse-url (concat url (url-hexify-string term)))
      )))

(defun my-search-pyside2 (term)
  "Search the PySide2 documentation."
  (interactive "MQuery (Press <enter> to search symbol at point): ")
  (if (equal term "")
      (setq term (symbol-name (symbol-at-point))))
  (my-search-engine "pyside2" term))

(defun my-search-ddg (term)
  "Search the PySide2 documentation."
  (interactive "MQuery (Press <enter> to search symbol at point): ")
  (if (equal term "")
      (setq term (symbol-name (symbol-at-point))))
  (my-search-engine "duckduckgo" term))

;; (global-set-key (kbd "C-m") 'my-search-ddg)

(use-package hide-comnt
  :load-path "~/.emacs.d/lisp/")

;; (defun my-swap-dash ()
;;   "Make dash insert and underscore and vice-versa."
;;   (interactive)
;;   (let ((just-inserted (char-before (point))))
;;     (cond
;;      ((equal just-inserted 45) ;; '-'
;;       (progn
;;         (delete-backward-char 1)
;;         (insert "_")))
;;      ((equal just-inserted 95) ;; '_'
;;       (progn
;;         (delete-char 1)
;;         (insert "-"))))))

;; (defvar swap-dash-state-p nil
;;   "Is swap-undash enabled?")

;; (defun toggle-swap-dash ()
;;   "Toggle `my-swap-dash'."
;;   (interactive)
;;   (if swap-dash-state-p
;;       (progn
;;         (remove-hook 'post-self-insert-hook #'my-swap-dash)
;;         (setq swap-dash-state-p nil))
;;     (progn
;;       (add-hook 'post-self-insert-hook #'my-swap-dash)
;;       (setq swap-dash-state-p t))))

(defun my-remap-dash-and-underscore ()
  (interactive)
  (define-key python-mode-map "-" #'(lambda () (interactive) (insert "_")))
  (define-key python-mode-map "_" #'(lambda () (interactive) (insert "-"))))

(add-hook 'python-mode-hook 'my-remap-dash-and-underscore)

(defun my-undo-remap-dash-and-underscore ()
  (interactive)
  (define-key python-mode-map "-" 'self-insert-command)
  (define-key python-mode-map "_" 'self-insert-command))

(define-key comint-mode-map "-" #'(lambda () (interactive) (insert "_")))
(define-key comint-mode-map "_" #'(lambda () (interactive) (insert "-")))

;; (defun my-toggle-dash-to-underscore ()
;;   "Toggle dash to underscore."
;;   (interactive)
;;   (cl-labels
;;       ((remapped-p (key)
;;                    "Has key been remapped?"
;;                    (when (lookup-key (current-local-map) (kbd key)) 't))
;;        (keymap-symbol (keymap)
;;                       "What is the local keymap? See URL `https://stackoverflow.com/a/14490054'"
;;                       (catch 'gotit
;;                         (mapatoms (lambda (sym)
;;                                     (and (boundp sym)
;;                                          (eq (symbol-value sym) keymap)
;;                                          (not (eq sym 'keymap))
;;                                          (throw 'gotit sym))))))
;;        (swap-keys (map key1 key2)
;;                   "Swap KEY1 and KEY2 in MAP."
;;                   (define-key (symbol-value map) (kbd key1) #'(lambda () (interactive) (insert (kbd key2))))
;;                   (define-key (symbol-value map) (kbd key2) #'(lambda () (interactive) (insert (kbd key1)))))

;;     (if (not (remapped-p "-"))

;;       ;;   (swap-keys (list (keymap-symbol (current-local-map))) "-"  "_")
;;       ;; (swap-keys (list (keymap-symbol (current-local-map))) "_"  "-")

;;         (swap-keys (keymap-symbol (current-local-map)) "-"  "_")
;;         (swap-keys (keymap-symbol (current-local-map)) "_"  "-")

;;         )
;;         ;; (symbol-value (keymap-symbol (current-local-map)))
;;       )


;;     ))


(defvar my-layout 'qwerty
  "Current keyboard layout.

Either 'qwerty or 'dvp.")

(defun my-swap-keys ()
  "Swap keys.  Works when in function.  When not ctrs keys don't
swap on init."
  (interactive)

  (cond ((eq my-layout 'qwerty)

         (define-key key-translation-map (kbd "M-q") (kbd "M-x"))
         (define-key key-translation-map (kbd "M-x") (kbd "M-q"))

         ;; (keyboard-translate ?\M-q ?\M-x)
         ;; (keyboard-translate ?\M-x ?\M-q)

         ;; (define-key key-translation-map [?\C-x] [?\C-q])
         ;; (define-key key-translation-map [?\C-q] [?\C-x])
         ;; (keyboard-translate ?\C-q ?\C-x)
         ;; (keyboard-translate ?\C-x ?\C-q)

         (setq my-layout 'dvp))

        ((eq my-layout 'dvp)

         (define-key key-translation-map (kbd "M-x") (kbd "M-q"))
         (define-key key-translation-map (kbd "M-q") (kbd "M-x"))
         ;; (define-key key-translation-map [?\M-x] [?\M-q])
         ;; (define-key key-translation-map [?\M-q] [?\M-x])

         ;; ;; swap C-q and C-x
         ;; (define-key key-translation-map [?\C-x] [?\C-q])
         ;; (define-key key-translation-map [?\C-q] [?\C-x])
         ;; (keyboard-translate ?\C-q ?\C-x)
         ;; (keyboard-translate ?\C-x ?\C-q)

         ;; (keyboard-translate ?\M-q ?\M-x)
         ;; (keyboard-translate ?\M-x ?\M-q)

         (setq my-layout 'qwerty)))


       ;; (define-key key-translation-map [?\C-x] [?\C-q])
       ;; (define-key key-translation-map [?\C-q] [?\C-x])

       (message "Swapped keys to %s!" my-layout))

;; (characterp ?\C-q)
;; (characterp ?\C-x)
;; (characterp ?\M-q)
;; (characterp ?\M-x)

;; (my-swap-keys)
(global-set-key (kbd "M-;") 'fill-paragraph)

;; bottom matter
(setq my-python-break-string "import ipdb; ipdb.set_trace(context=10)")

(defun my-insert-breakpoint ()
  (interactive)
  (my-newline-without-break-of-line)
  (insert my-python-break-string)
  (bm-toggle)
  (save-buffer))

(define-key python-mode-map (kbd "<f6>") 'my-insert-breakpoint)
(global-set-key (kbd "<f9>") 'save-buffer)

(right-click-context-mode t)

;; 16000
(defun my-kill-python ()
  "Kill Python."
  (interactive)
  (shell-command "taskkill /f /fi \"IMAGENAME eq python.exe\" /fi \"MEMUSAGE gt 15000\""))

(global-set-key (kbd "<apps>") 'my-kill-python)

(global-set-key (kbd "<f10>")
                (lambda() (interactive)
                  (save-some-buffers t nil)
                  (my-kill-python)
                  (my-sh-send-command my-global-shell-command)))

;; (setq httpd-root "C:/projects/doxyserv/html/")
;; (httpd-start)
;; load impatient-mode in buffer
;; http://localhost:8080/imp/

(use-package general
  :ensure t
  :config

  (general-create-definer my-leader-def
    :prefix "SPC")

  ;; ** Global Keybindings
  (my-leader-def
   :keymaps 'normal
     ";" 'comment-dwim-2
     "=" 'er/expand-region
     "b" 'helm-buffers-list
     "c" 'my-string-inflection-style-cycle
     ;; "c" '(lambda () (interactive) (org-capture nil "s"))
     "d" 'elpy-occur-definitions
     "e" 'eval-last-sexp
     "f" 'my-find-file
     "i" '(lambda() (interactive) (find-file "~/.emacs.d/init.el"))
     "k" 'kill-buffer
     ;; "m" 'helm-semantic-or-imenu
     "m" 'my-search-ddg
     "o" 'ace-window
     "s" 'save-buffer
     ;; "'" 'my-toggle-edit-src
     "SPC" 'org-ctrl-c-ctrl-c
     "t" 'org-insert-structure-template
     "x" 'eval-expression
     "g" 'ffap-other-window
     )

     ;; (general-create-definer my--leader-def
     ;;   ;; :prefix my-local-leader
     ;;   :prefix "SPC")

     ;; (my-local-leader-def
     ;;  :states 'normal
     ;;  :keymaps 'override 'python-mode-map
     ;;  ;; "y" 'org-store-link
     ;;  )

     )

(defun my-replace-last-sexp-with-result ()
  "Replace last sexp with result.

See `https://stackoverflow.com/a/3035574/5065796'. "
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun my-venv-create ()
  "Create Python venv.

I don't keep Python on my path.  Unfortunately, the autocomplete
in shell.el pulls completions from other buffers, creating a
chicken and egg problem."
  (interactive)
  (insert "\"C:\\python\\python3.6.8-64\\python.exe\" -m venv venv"))

(defun my-venv-activate ()
  "Activate venv."
  (interactive)
  (insert "venv\\Scripts\\activate"))

;; (setq elpy-test-pytest-runner-command '("py.test" "-s"))

;; (setq my-python-break-regexp "[i]?pdb.set-trace")

;; (defun my-compile-advice (args)
;;   "Advises `compile' so it sets the argument COMINT to t
;; if breakpoints are present in `python-mode' files"
;;   (when (derived-mode-p major-mode 'python-mode)
;;     (save-excursion
;;       (save-match-data
;;         (goto-char (point-min))
;;         (if (re-search-forward my-python-break-regexp (point-max) t)
;;             ;; set COMINT argument to `t'.
;;             (let ((command (car args)))
;;               (setq args (list command t)))))))
;;   args)

;; (advice-add 'compile :filter-args #'my-compile-advice)


;; ;; black magic taken from https://emacs.stackexchange.com/a/338/15177
;; (add-to-list 'display-buffer-alist
;;          '("^\\*compilation\\*". ((display-buffer-reuse-window)  .
;;                                   ((reusable-frames . t)
;;                                   (inhibit-same-window . t)))))

(defun my-kill-excel ()
  "Kills all the Excel."
  (interactive)
  (shell-command "taskkill /f /im excel.exe"))

;; (setq compilation-filter-hook nil)

;; (setq same-window-regexps '("\*compilation\*"))
;; (setq same-window-regexps '("."))

;;; ots: literate-org-mode

;; Create Inferior python session identified by file name
;; (add-to-list 'org-structure-template-alist '("p" . "src python :session (concat \"*Python: \" (file-name-nondirectory (buffer-file-name)) \"*\") :results output :exports both :tangle yes"))

;; (add-hook 'org-src-mode-hook 'evil-insert-state)

;; delete structure template (from beginning of alist)
;; (setq org-structure-template-alist (cdr org-structure-template-alist))

;; The boundary of src blocks not ideal.
;; two cases: begin/end adj or not
;; need a fn which tells you if you're actually inside
;; the toggle also needs to preserve point in original buffer
;; could parse line above, if #+begin_src... the you're in block
;; unless current line has #+end_src.

;; org-at-block detects begin
;; (defun org-at-block-begin-p nil
;;   "Is cursor at a block keyword?"
;;   (save-excursion
;;     (move-beginning-of-line 1)
;;     (looking-at org-block-regexp)))

;; (defconst org-block-end-regexp
;;   "^[ \t]*#\\+\\(?:END\\|end\\)\\(_src\\|_SRC)[: \t\r\n]\\|$\\)"
;;   "Custom regexp expression for end_src blocks.")

;; (defun org-at-block-end-p nil
;;   "Is cursor at a block end?"
;;   (save-excursion
;;     (move-beginning-of-line 1)
;;     (looking-at org-block-end-regexp)))

;; (defun my-toggle-edit-src ()
;;   "Toggle org src edit."
;;   (interactive)
;;   (if (org-src-edit-buffer-p)
;;       (org-edit-src-exit)
;;     ;; or org-edit-special, but exit fns differ
;;     (org-edit-src-code)))

;; (defun my-post-command-hook ()
;;   "Post command hook."
;;   (when (or ;; Trying to go below buffer
;;       (and (eobp) (eq last-command 'next-line))
;;       ;; Trying to go above buffer
;;       (and (bobp) (eq last-command 'previous-line)))
;;     (my-toggle-edit-src)))

;; (add-hook 'post-command-hook 'my-post-command-hook)
;; (global-set-key (kbd "<f1>") 'my-toggle-edit-src)

;;; end-ots: literate-org-mode

;;; capture code snippets
;; insert code snippet with file/line reference into file hacked from
;; https://web.archive.org/web/20181019173343/http://ul.io/nb/2018/04/30/better-code-snippets-with-org-capture/
;; rewrite to use org-store-link

(defvar my-capture-target "~/.notes"
  "File to send captured code to.

#+COMMENT: -*- eval: (setq my-capture-target (buffer-file-name)); -*-

")

;; (setq my-capture-target "~/Documents/notes/understanding-audio-display.org")
(defun my-org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language

E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun my-org-capture-code-snippet (f)
  (with-current-buffer (find-buffer-visiting f)
    (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
          (file-name (buffer-file-name))
          (line-number (line-number-at-pos (region-beginning)))
          (org-src-mode (my-org-capture-get-src-block-string major-mode)))
      (format
       "#+begin_example %s
%s
#+end_example
file:%s::%s"
       org-src-mode
       code-snippet
       file-name
       line-number))))

(defun my-org-capture-locate-above-footnotes ()
  "Locate point above footnotes section"
  (goto-char (point-max))
  (search-backward "* Footnotes")
  (forward-line -1)
  ;; (point)  ; uncomment if we need point
  )

(require 'org-capture)
;; Capture Template
(add-to-list 'org-capture-templates
             '(;; shortcut
               "s"
               ; description
               "code snippet"
               ;; insert as plaintext
               plain
               ;; target file
               (file my-capture-target)
               ;; (file+function my-capture-target my-org-capture-locate-above-footnotes)
               ;; Put cursor at top, followed by newline and then capture snippet
               ;; by passing current file into function
               "\n\n%(my-org-capture-code-snippet \"%F\")\n%?"))
               ;; "%?\n\n%(my-org-capture-code-snippet \"%F\")"))

;; (setq org-capture-templates nil)

;; Set notes using on demand window, S-f1, goto notes then with f1,
;; capture intro with C-f1
(global-set-key (kbd "C-<f1>") '(lambda () (interactive) (org-capture nil "s")))  ; "capture" into on demand

;; src_python[:exports results :results output]{}
(font-lock-add-keywords 'org-mode
                    '(("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
                       (1 '(:foreground "#555555" :weight 'normal :height 2)) ; src_ part
                       (2 '(:weight 'bold)) ; "lang" part.
                       (3 '(:foreground "#555555" :height 4)) ; [:header arguments] part.
                       (4 'org-code) ; "code..." part.
                       )))


;; todo org-mks doesn't have user friendly way to cancel; must be C-g
(define-key org-mode-map (kbd "<insert>") 'org-insert-structure-template)
;; (define-key evil-normal-state-map (kbd "<insert>") 'org-insert-structure-template)



(yas-global-mode)

(defun my-org-toggle-link-display ()
  "Toggle the literal or descriptive display of links.

Modified from `https://emacs.stackexchange.com/a/5390'"
  (interactive)
  (if org-descriptive-links
      (progn (remove-from-invisibility-spec '(org-link))
         (org-restart-font-lock)
         (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
       (org-restart-font-lock)
       (setq org-descriptive-links t))))

(defun my-org-remove-link ()
  "Replace an org link by its description or if empty an address.

See `https://emacs.stackexchange.com/a/21945'."
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3)
                               (match-string-no-properties 3)
                             (match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))


;; https://emacs.stackexchange.com/a/3990/15177
(defun my--yank-hander (text)
  (if (derived-mode-p 'org-mode)
      (insert text)
    (string-match org-bracket-link-regexp text)
    (insert (substring text (match-beginning 1) (match-end 1)))))

(defun my-org-yank-link ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my--yank-handler)) text)
      (kill-new text))))

;; (org-defkey org-mode-map (kbd "C-c y") 'my-org-yank-link)

;; replace text in org with symbol
;; (add-hook 'org-mode-hook (lambda ()
;;    "Beautify Org Checkbox Symbol"
;;    (push '("[ ]" .  "☐") prettify-symbols-alist)
;;    (push '("[X]" . "☑" ) prettify-symbols-alist)
;;    (push '("[-]" . "❍" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
;;    (prettify-symbols-mode)))

;; todo: define these in only one place
(global-set-key (kbd "C-j") 'helm-swoop)
(global-set-key (kbd "M-j") 'helm-semantic-or-imenu)

;; (defun my-eww-new-buffer (url)
;;   "Like `eww', but fetch URL in a new EWW buffer.

;; See `https://emacs.stackexchange.com/a/44344'
;; "
;;   (interactive (advice-eval-interactive-spec (cadr (interactive-form 'eww))))
;;   (let ((eww-suggest-uris (list (lambda () url))))
;;     (eww-open-in-new-buffer)))

;; match Emacs 27 behavior
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34374
;; https://emacs.stackexchange.com/questions/24472/simple-method-for-creating-multiple-eww-buffers
(with-eval-after-load 'eww
  (defun eww (url &optional arg)
    "Fetch URL and render the page.
    If the input doesn't look like an URL or a domain name, the
    word(s) will be searched for via `eww-search-prefix'.

    If called with a prefix ARG, use a new buffer instead of reusing
    the default EWW buffer."
    (interactive
     (let* ((uris (eww-suggested-uris))
            (prompt (concat "Enter URL or keywords"
                            (if uris (format " (default %s)" (car uris)) "")
                            ": ")))
       (list (read-string prompt nil 'eww-prompt-history uris)
             (prefix-numeric-value current-prefix-arg))))
    (setq url (eww--dwim-expand-url url))
    (pop-to-buffer-same-window
     (cond
      ((eq arg 4)
       (generate-new-buffer "*eww*"))
      ((eq major-mode 'eww-mode)
       (current-buffer))
      (t
       (get-buffer-create "*eww*"))))
    (eww-setup-buffer)
    ;; Check whether the domain only uses "Highly Restricted" Unicode
    ;; IDNA characters.  If not, transform to punycode to indicate that
    ;; there may be funny business going on.
    (let ((parsed (url-generic-parse-url url)))
      (when (url-host parsed)
        (unless (puny-highly-restrictive-domain-p (url-host parsed))
          (setf (url-host parsed) (puny-encode-domain (url-host parsed)))))
      ;; When the URL is on the form "http://a/../../../g", chop off all
      ;; the leading "/.."s.
      (when (url-filename parsed)
        (while (string-match "\\`/[.][.]/" (url-filename parsed))
          (setf (url-filename parsed) (substring (url-filename parsed) 3))))
      (setq url (url-recreate-url parsed)))
    (plist-put eww-data :url url)
    (plist-put eww-data :title "")
    (eww-update-header-line-format)
    (let ((inhibit-read-only t))
      (insert (format "Loading %s..." url))
      (goto-char (point-min)))
    (url-retrieve url 'eww-render
                  (list url nil (current-buffer))))
  )

(defun my-read-lines (file &optional N)
  "Return the first N lines of FILE or all lines is N nil."
  (with-temp-buffer
    (insert-file-contents file)
    (subseq
     (split-string (buffer-string) "[\r\n]" t) 0 N)))

(defun my-fringe ()
  "Set fringe on left to 65."
  (interactive)
  (set-window-fringes nil 65))

;; (global-set-key (kbd "<pause>") 'my-fringe)
(global-set-key (kbd "<scroll>") '(lambda () (interactive) (find-file "C:\\Users\\mtrzcinski\\Documents\\time-tracker.org")))

(defun my-slat-pref ()
  "Activate slat pref project"
  (interactive)
  (my-activate-python-project
   "c:/projects/slat_pref/"
   "c:/projects/slat_pref/venv/"
   "c:/projects/slat_pref/slat_pref/"
   "c:/projects/slat_pref/tests/demo.py"))

;; https://emacs.stackexchange.com/a/58887/15177
(define-key mode-line-buffer-identification-keymap [mode-line mouse-1] nil)
(define-key mode-line-buffer-identification-keymap [mode-line mouse-3] nil)

;; -------------
(message "The init.el has finished loading. See log for more details.")
