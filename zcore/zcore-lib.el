;;; zcore-lib.el --- Util functions
;;; Commentary:
;;; Code:

(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defvar zen-settings '())

(setq buffers-to-avoid '("scratch" "*Messages*" "*Help*" "*Completions*"))

(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice for avoiding buffers using next buffer"
  (when (-any? (-partial 'string= (buffer-name)) buffers-to-avoid (buffer-name))
    (next-buffer)))

;; New empty file
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
(interactive)
(let (($buf (generate-new-buffer "untitled")))
  (switch-to-buffer $buf)
  (funcall initial-major-mode)
  (setq buffer-offer-save t)
  $buf))


(defun doom-unquote (exp)
  "Return EXP unquoted."
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

1
(defun doom--resolve-hook-forms (hooks)
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (doom-enlist (doom-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
	    else collect (intern (format "%s-hook" (symbol-name hook)))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))


(defmacro add-hook! (&rest args)
  "https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-lib.el#L187
  A convenience macro for `add-hook'. Takes, in order:
  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.
Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))
Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(nreverse forms)))))

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. Like `defmacro', this should return a form to be executed
when called with `set!'. FORMS are not evaluated until `set!' calls it.
See `zen/describe-setting' for a list of available settings.
Do not use this for configuring Doom core."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (let ((fn (intern (format "zen--set%s" keyword))))
    `(progn
       (defun ,fn ,arglist
         ,docstring
         ,@forms)
       (cl-pushnew ',(cons keyword fn) zen-settings :test #'eq :key #'car))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist. See
`zen/describe-setting' for a list of available settings."
  (declare (indent defun))
  (unless values
    (error "Empty set! for %s" keyword))
  (let ((fn (cdr (assq keyword zen-settings))))
    (if fn
        (apply fn values)
      (when doom-debug-mode
        (message "No setting found for %s" keyword)
        nil))))

;https://unix.stackexchange.com/a/152151
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))


(provide 'zcore-lib)
;;; zcore-lib.el ends here
