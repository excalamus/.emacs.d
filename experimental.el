;;; experimental.el -- Lisp Laboratory

;; Develop Lisp extensions prior to incorporation into init.el or a
;; separate package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add fixed case abbrev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-add-case-fixed-abbrev (table name expansion)
;;   "Add abbrev with case-fixed t property."
;;   (interactive "XAbbrev table (global-abbrev-table): "
;;    (let ((table (intern-soft (completing-read
;;           "Abbrev table (global-abbrev-table): "
;;           abbrev-table-name-list nil t nil nil "global-abbrev-table" )))
;;          (name (read-string "Abbrev name: "))
;;          (expansion (read-string "Expansion: ")))
;;      (list name expansion table)))
;;   (let ((table (or table global-abbrev-table)))
;;     (define-abbrev table name expansion nil :case-fixed t)))

;; (eq 'global-abbrev-table (intern-soft "global-abbrev-table"))

;; (define-abbrev (intern-soft "global-abbrev-table") "duder" "man" nil :case-fixed t)

;; (my-add-case-fixed-abbrev "my-abbrev" "my-expansion" "global-abbrev-table")
;; (write-abbrev-file)

(defun my-add-case-fixed-abbrev (name expansion &optional table)
  "Add abbrev with case-fixed t property."
  (interactive
   (let ((table (symbol-value (intern-soft (completing-read
          "Abbrev table (global-abbrev-table): "
          abbrev-table-name-list nil t nil nil "global-abbrev-table" ))))
         (name (read-string "Abbrev name: "))
         (expansion (read-string "Expansion: ")))
     (list name expansion table)))
  (let ((table (or table global-abbrev-table)))
    (define-abbrev table name expansion nil :case-fixed t)))

(defun my-test (&optional table)
  "junk"
   (let ((table (intern-soft (completing-read
          "Abbrev table (global-abbrev-table): "
          abbrev-table-name-list nil t nil nil "global-abbrev-table" )))
         )
   (message "%s" (symbol-value table))
  ))

(my-test)

(my-add-case-fixed-abbrev "myAbbrev" "myExpansion")
(my-add-case-fixed-abbrev "myAbbrev" "myExpansion" global-abbrev-table)
(write-abbrev-file "~/.emacs.d/test-abbrev")
