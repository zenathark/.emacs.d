;;; zcore-packages.el --- package management system

;; @see https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el


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

(provide 'zcore-packages)
;;; zcore-packages.el ends here
