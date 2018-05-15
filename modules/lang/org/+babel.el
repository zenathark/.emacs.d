;;; lang/org/+babel.el -*- lexical-binding: t; -*-
(def-package! ox)
(def-package! ox-latex)
(def-package! ox-beamer)
(def-package! ox-md)
;(def-package! ob-shell (conact zen-emacs-dir "el-get/org/contrib/lisp/ob-shell.el"))

(defvar +org-babel-languages
  '(calc
    css
    emacs-lisp
    haskell
    js
    latex
    ledger
    lilypond
    lisp
    matlab
    plantuml
    python
    restclient ; ob-restclient
    ruby
    rust       ; ob-rust
    shell
    sqlite
    sql-mode   ; ob-sql-mode
  ;  translate ; ob-translate
    )
  "A list of org-babel languages to load.")


(after! org
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop for sym in +org-babel-languages
            collect (cons sym t)))

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins
  ;; update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))
