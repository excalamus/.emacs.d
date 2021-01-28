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
