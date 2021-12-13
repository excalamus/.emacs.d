;;; experimental.el -- Lisp Laboratory

;; Develop Lisp extensions prior to incorporation into init.el or a
;; separate package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - yank to mark, set mark, move to other location (other window or
;; within buffer), kill something, yank to the marked location

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quick bind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar xc/quick-bind nil
;;   "Quick binding for F5 key.

;; Must be a function that takes no arguments.")

;; (defun xc/set-quick-bind ()
;;   "Set function to `xc/quick-bind'."
;;   (interactive)
;;   (let ((func (intern (completing-read "Set `xc/quick-bind' to: " obarray #'functionp))))
;;     (fset 'xc/quick-bind (symbol-function 'func))
;;     (message "Set `xc/quick-bind' to %s" func)))

;; (defun xc/test ()
;;   "Say hello"
;;   (message "Hello!"))

;; (funcall
;; (lambda () (interactive)
;;                 (let ((func (symbol-function xc/quick-bind)))
;;                   (if (commandp func)
;;                       (call-interactively func)
;;                     (funcall func)))))

;; (defun my-inferior-python-workaround ()
;;   "Open inferior comint in current buffer."
;;   ;; replace whatever buffer was displaced by run-python

;;   ;; There are two windows and two buffers to consider. There is the
;;   ;; window you want the inferior python buffer displayed in (target
;;   ;; window).  There is the window that run-python actuall puts the
;;   ;; inferior process (afflicted-window).  There is the buffer
;;   ;; displayed in the target window prior to running run-python
;;   ;; (displaced-buff).  There is the buffer that is the inferior
;;   ;; Python process (python-buff).
;;   ;;

;;   ;; run-python is called.  It creates the new process buffer but it
;;   ;; isn't yet displayed.  We can get the python buffer through
;;   ;; python-shell-get-buffer but because it's not yet displayed, we
;;   ;; must force it to display in order to determine the afflicted
;;   ;; window.

;;   ;; We work around run-python's nonsense by first getting a handle on
;;   ;; each of the windows and buffers described above.  We then set the
;;   ;; target window to show the python-buff (like we wanted).  We reset
;;   ;; the afflicted-window to show the displaced-buff.

;;   (let* (
;;         (displaced-buff
;;          (caar (window-prev-buffers (previous-window))))
;;           ;; (window-buffer (get-mru-window t)))
;;         (target-win (get-buffer-window displaced-buff))

;;          (python-buff (python-shell-get-buffer))
;;          (afflicted-win (with-displayed-buffer-window python-buff nil nil
;;                           (get-buffer-window python-buff))))
;;     (message "\n----\ndisplaced-buff %s\ntarget-win %s\npython-buff %s\nafflicted-win %s"
;;              displaced-buff target-win python-buff afflicted-win)
;;     (with-selected-window afflicted-win
;;       (switch-to-buffer displaced-buff))
;;     (with-selected-window target-win
;;       (switch-to-buffer python-buff))
;;   ;; (switch-to-buffer (other-buffer (current-buffer)))
;;     ))

;; (with-temp-buffer-window "*Python*" nil nil)
;; (with-displayed-buffer-window "*Python*" nil nil
;;   (get-buffer-window (python-shell-get-buffer)))

;;  (add-hook 'inferior-python-mode-hook 'my-inferior-python-workaround)
;;  (remove-hook 'inferior-python-mode-hook 'my-inferior-python-workaround)

;; (get-buffer-window ())
;; (python-shell-get-buffer)

;; (previous-window)

(defvar exp-on-demand-window nil
  "Target on-demand window.

An on-demand window is one which you wish to return to within the
current Emacs session but whose importance doesn't warrant a
permanent binding.")

(defvar exp-on-demand-buffer nil
  "target on-demand buffer.

An on-demand buffer is one which you wish to return to within the
current Emacs session but whose importance doesn't warrant a
permanent binding.")


(defun exp-set-on-demand-window (&optional win)
  "Set the value of the `exp-on-demand-window' to WIN.

Use selected window if WIN is nil."
  (interactive)
  (let ((win (or win (selected-window))))
    (if (windowp win)
        (setq exp-on-demand-window win)
      (error (format "Invalid window %s" win)))
    (message "Set on-demand window to: %s" exp-on-demand-window)))


(defun exp-set-on-demand-buffer (&optional buff)
  "Set the value of the `exp-on-demand-buffer' to BUFF.

Use current buffer if BUFF is nil."
  (interactive)
  (let ((buff (or buff (current-buffer))))
    (if (bufferp buff)
        (setq exp-on-demand-buffer buff)
      (error (format "Invalid buffer %s" buff)))
    (message "Set on-demand buffer to: %s" exp-on-demand-buffer)))


(defun exp-send-line-or-region (&optional buff prefix postfix beg end)
  "Send region defined by BEG and END to BUFF.

Use current region if BEG and END not provided.  If no region
provided, send entire line.  Default BUFF is that displayed in
`exp-on-demand-buffer'."
  (interactive (if (use-region-p)
                   (list nil nil nil (region-beginning) (region-end))
                 (list nil nil nil nil nil)))
  (let* ((beg (or beg (if (use-region-p) (region-beginning)) nil))
         (end (or end (if (use-region-p) (region-end)) nil))
         (substr (string-trim
                  (or (and beg end (buffer-substring-no-properties beg end))
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
         (buff (or buff (if exp-on-demand-buffer
                            exp-on-demand-buffer
                          (error "No target buffer")))))
    (if substr
        (with-current-buffer buff
          (dolist (fun prefix) (funcall fun))
          (insert substr)
          (dolist (fun postfix) (funcall fun)))
      (error "Invalid selection"))))

(general-def
    :keymaps 'override
    "<f5>" '(lambda () (interactive) (progn (funcall 'exp-send-line-or-region

                                                    ;; note that these
                                                    ;; won't work as
                                                    ;; funcall and
                                                    ;; apply must take
                                                    ;; a function, not
                                                    ;; a macro.  The
                                                    ;; send-line
                                                    ;; function
                                                    ;; probably needs
                                                    ;; to be a macro,
                                                    ;; then.  We
                                                    ;; really want to
                                                    ;; insert some
                                                    ;; code before or
                                                    ;; after the
                                                    ;; insert.

                                                     nil
                                                     (list (funcall 'setq-local 'window-point-insertion-type 't))
                                                     (list
                                                       (end-of-line)
                                                       (newline-and-indent))
                                                     )))
    )

- [pywebchannel](https://github.com/MenloSystems/pywebchannel): qwebchannel.js implemented in Python


(defun org-fix-markdown-links ()
  "Fix ill-formatted internal links.
E.g. replace [[*TODO Headline][headline]] by [[*Headline][headline]].
Go through the buffer and ask for the replacement."
  (interactive)
  (visible-mode 1)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp markdown-regex-link-inline))
      (while (re-search-forward regexp nil t)
        (when (and (save-excursion
                     (goto-char (match-beginning 0))
                     (looking-at-p org-link-bracket-re))
                   (y-or-n-p "Fix link (remove TODO keyword)? "))
          (replace-match "[[*")))))
  (visible-mode -1))



;; https://emacs.stackexchange.com/a/16513
(defun my-indent ()
  (let ((last-indent (if (> (line-number-at-pos) 1)
                         (save-excursion
                           (previous-line)
                           (back-to-indentation)
                           (current-column))
                       0)))
    (save-excursion
      (back-to-indentation)
      (if (and (eq last-command this-command)
               (> (point) (line-beginning-position)))
          (delete-region (max (line-beginning-position) (- (point) 4)) (point))
        (while (< (current-column) (+ 4 last-indent))
          (insert " "))))
    (if (< (point) (save-excursion (back-to-indentation) (point)))
        (back-to-indentation))))


(defun my-sql-mode-hook ()
  (setq indent-line-function 'my-indent))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(remove-hook 'sql-mode-hook 'my-sql-mode-hook)

;; https://stackoverflow.com/a/14490054
(defun my-keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

;; in *scratch*:
(defun my-get-keymap-symbol ()
  "Get keymap of current local map."
  (interactive)
  (message "%s" (my-keymap-symbol (current-local-map))))

(defun xc/xref-find-definitions ()
  "Wrap `xref-find-definitions' and add to evil jump list."
  ;; goto definition
  (evil--jumps-push)
  (xref-find-definitions))

(defun xc/xref-find-definitions-other-window ()
  "Wrap `xref-find-definitions' and add to evil jump list."
  ;; goto definition
  (evil--jumps-push)
  (xref-find-definitions-other-window))

(defun my-find-file-at-point-goto-line (ret)
  "Ignore RET and jump to line number given in `ffap-string-at-point'."
  (interactive)
  (when (and
     (stringp ffap-string-at-point)
     (string-match ":\\([0-9]+\\)\\'" ffap-string-at-point))
    (goto-char (point-min))
    (forward-line (string-to-number (match-string 1 ffap-string-at-point))))
  (message "%s" ret)
  ;; ret
  )

;; https://stackoverflow.com/a/2122436/5065796
;; if compilation-shell-minor-mode is on, then these regexes
;; will make errors linkable
(defun matt-add-global-compilation-errors (list)
  (dolist (x list)
    (add-to-list 'compilation-error-regexp-alist (car x))
    (setq compilation-error-regexp-alist-alist
      (cons x
            (assq-delete-all (car x)
                             compilation-error-regexp-alist-alist)))))

(matt-add-global-compilation-errors
 `(
   (matt-python ,(concat "^ *File \\(\"?\\)\\([^,\" \n    <>]+\\)\\1"
                    ", lines? \\([0-9]+\\)-?\\([0-9]+\\)?")
           2 (3 . 4) nil 2 2)
   (matt-pdb-stack ,(concat "^>?[[:space:]]*\\(\\([-_./a-zA-Z0-9 ]+\\)"
                       "(\\([0-9]+\\))\\)"
                       "[_a-zA-Z0-9]+()[[:space:]]*->")
              2 3 nil 0 1)
   (matt-python-unittest-err "^  File \"\\([-_./a-zA-Z0-9 ]+\\)\", line \\([0-9]+\\).*" 1 2)
   )
 )

(defun matt-set-local-compilation-errors (errors)
  "Set the buffer local compilation errors.

Ensures than any symbols given are defined in
compilation-error-regexp-alist-alist."
  (dolist (e errors)
     (when (symbolp e)
      (unless (assoc e compilation-error-regexp-alist-alist)
        (error (concat "Error %s is not listed in "
                       "compilation-error-regexp-alist-alist")
               e))))
  (set (make-local-variable 'compilation-error-regexp-alist)
       errors))

(defun xc/experimental-csv ()
  (interactive)
  (csv-align-set-column-width 1 9) ; truncate year from date
  (csv-align-set-column-width 2 75) ; description
  (csv-align-set-column-width 5 0) ; hide category
  )

(use-package geiser)
(use-package geiser-guile)
(use-package geiser-mit)

(defun xc/calculate-tip ()
  (interactive)
  (let* ((arg (prefix-numeric-value current-prefix-arg))
        (total (string-to-number (read-string "Enter total: $")))
        (percentage (/ (string-to-number (read-string "Tip percentage: %" "20")) 100.0))
        (amount (/ total (+ 1.0 percentage)))
        (tip (* amount percentage)))
    ;; (message "%s = %s + %s" total amount tip)
    (if (= arg 1) (insert (format "%.02f" tip))
      (insert (format "%.02f" amount)))))





(use-package writeroom-mode
  :after (:all org)
  :config
  (setq writeroom-restore-window-config t)

  (if xc/debug (message "writeroom-mode")))



(defun my-minimize-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (window-resize
   window
   (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
   nil nil window-resize-pixelwise))

(defun my-1/4-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (my-maximize-window)
  (window-resize
   window
   (- (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4))
    nil nil window-resize-pixelwise))

(defun my-center-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (my-maximize-window)
  (window-resize
   window
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 2)
    nil nil window-resize-pixelwise))

(defun my-3/4-window (&optional window)
  (interactive)
  (when switch-to-buffer-preserve-window-point
    (window--before-delete-windows window))
  (setq window (window-normalize-window window))
  (my-maximize-window)
  (window-resize
   window
    (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4)
    nil nil window-resize-pixelwise))

(defun my-maximize-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise))

(setq my-last-window-op 'center)

(defun my-recenter-window-top-bottom (&optional arg)
  (interactive "P")

  (cond ((eq my-last-window-op 'center)
         (my-maximize-window)
         (setq my-last-window-op 'max))
        ((eq my-last-window-op 'max)
         (my-3/4-window)
         (setq my-last-window-op 'three-quarter))
        ((eq my-last-window-op 'three-quarter)
         (my-1/4-window)
         (setq my-last-window-op 'one-quarter))
        ((eq my-last-window-op 'one-quarter)
         (my-minimize-window)
         (setq my-last-window-op 'min))
        ((eq my-last-window-op 'min)
         (my-center-window)
         (setq my-last-window-op 'center))))

(define-key global-map [?\M-l] 'my-recenter-window-top-bottom)

