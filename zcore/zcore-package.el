;; zcore-package.el --- Package utilities
(setq package-enable-at-startup nil)

(setq load-path (append (seq-drop (directory-files (concat zen-package-load-path nil ".") t) 5)
			load-path))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure nil)

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').
FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).
If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (cl-assert (symbolp filesym) t)
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror)
        (unless noerror
          (error "Could not load file '%s' from '%s'" file path))))))

(defmacro def-package! (name &rest plist)
  "https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L323
A thin wrapper around `use-package' that assumes the package is included on emacs."
  ;; If byte-compiling, ignore this package if it doesn't meet the condition.
  ;; This avoids false-positive load errors.
  (unless (and (bound-and-true-p byte-compile-current-file)
               (or (and (plist-member plist :if)     (not (eval (plist-get plist :if))))
                   (and (plist-member plist :when)   (not (eval (plist-get plist :when))))
                   (and (plist-member plist :unless) (eval (plist-get plist :unless)))))
   `(use-package ,name ,@plist)))


(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.
Under the hood, this uses use-package's `use-package-inject-hooks'.
PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config :disable
If WHEN is :disable then BODY is ignored, and DOOM will be instructed to ignore
all `def-package!' blocks for PACKAGE.
WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (cond ((eq when :disable)
         (push package zen-disabled-packages)
         nil)
        ((memq when '(:pre-init :post-init :pre-config :post-config))
         `(progn
            (setq use-package-inject-hooks t)
            (add-hook!
              ',(intern (format "use-package--%s--%s-hook"
                                package
                                (substring (symbol-name when) 1)))
              ,@body)))
        (t
         (error "'%s' isn't a valid hook for def-package-hook!" when))))


(defun load-modules! ()
  (mapc (lambda (d) (load (concat zen-module-load-path "/" d "/config.el"))) zen-active-modules))


(provide 'zcore-package)
;;; zcore-package.el ends here
