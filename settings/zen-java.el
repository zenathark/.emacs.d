;;; zen-scala.el --- Scala mode configuration
;;; Commentary:
;;; Code:

(use-package ensime
  :ensure t
  :pin melpa-stable
  :mode "\\.java\\'")

(use-package sbt-mode
  :ensure t)

(use-package java-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(add-to-list 'exec-path "/usr/local/bin/")

(setq ensime-search-interface 'helm)
(setq ensime-eldoc-hints 'all)
(add-hook 'java-mode-hook 'turn-on-eldoc-mode)

(sp-local-pair 'java-mode "(" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
(sp-local-pair 'java-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
;; (defun set-junk-directory ()
;;   "Set the directory for junk code files or
;;    scripting files (sc) for scala"
;;   (defvar-local open-junk-file-directory (expand-file-name "src/junk/scala/%Y/%m/%d-%H%M%S.sc" projectile-project-root)))

;; (add-hook 'scala-mode-hook 'set-junk-directory)
;; (setq (make-local-variable 'open-junk-file-directory) "~/.emacs.d/cache/junk/%Y/%m/%d-%H%M%S.sc")
;;; zen-scala ends here
