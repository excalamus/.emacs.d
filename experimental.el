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
  "Add fixed case abbrev with NAME and EXPANSION into TABLE.

TABLE is optional; defaults to `global-abbrev-table'."
  (interactive
   (let ((table (symbol-value (intern-soft (completing-read
          "Abbrev table (global-abbrev-table): "
          abbrev-table-name-list nil t nil nil "global-abbrev-table"))))
         (name (read-string "Abbrev name: "))
         (expansion (read-string "Expansion: ")))
     (list name expansion table)))
  (let ((table (or table global-abbrev-table)))
    (define-abbrev table name expansion nil :case-fixed t)))

(defun my-add-case-fixed-abbrev2 (name expansion &optional fixed table)
  "Add fixed case abbrev with NAME and EXPANSION into TABLE.

FIXED optionally sets case-fixed; default if nil.
TABLE is optional; defaults to `global-abbrev-table'."
  (interactive
   (let* ((name (read-string "Abbrev name: "))
          (arg (prefix-numeric-value current-prefix-arg))
          (exp (and (>= arg 0)
                    (buffer-substring-no-properties
                     (point)
                     (if (= arg 0) (mark)
                       (save-excursion (forward-word (- arg)) (point))))))
          (expansion (read-string "Expansion: " exp))
          (table (symbol-value (intern-soft (completing-read
            "Abbrev table (global-abbrev-table): "
            abbrev-table-name-list nil t nil nil "global-abbrev-table"))))
          (fixed (y-or-n-p (format "Fix case? "))))
     (list name expansion fixed table)))
  (let ((table (or table global-abbrev-table))
        (fixed (or fixed nil)))
    (set-text-properties 0 (length name) nil name)
    (set-text-properties 0 (length expansion) nil expansion)
    (if (or (null expansion)
            (not (abbrev-expansion name table))
            (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                              name (abbrev-expansion name table))))
        (define-abbrev table name expansion nil :case-fixed fixed))))

(defun add-abbrev (table type arg)
  (let ((exp (and (>= arg 0)
                  (buffer-substring-no-properties
                   (point)
                   (if (= arg 0) (mark)
                     (save-excursion (forward-word (- arg)) (point))))))
        name)
    (setq name
          (read-string (format (if exp "%s abbrev for \"%s\": "
                                 "Undefine %s abbrev: ")
                               type exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
            (not (abbrev-expansion name table))
            (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                              name (abbrev-expansion name table))))
        (define-abbrev table (downcase name) exp))
    ))

(defun my-test (arg)
  (interactive
   (let (
         (arg (read))
     (list banana arg)
                 ))

  (message "%s %s" arg fruit)
  )

  (write-abbrev-file "~/.emacs.d/test_abbrev")
  (write-abbrev-file)

setText

two words

fruit

FOO
