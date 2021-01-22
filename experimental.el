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
