;;; zen-java.el --- Java mode configuration
;;; Commentary:
;;; Code:

;; (use-package ensime
;;   :ensure t
;;   :pin melpa-stable)

;; (use-package sbt-mode
;;   :ensure t)

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :ensure t
  :defer t)

(use-package highlight-symbol
  :ensure t
  :defer t)

(use-package realgud
  :ensure t)

(use-package meghanada
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
	    (lambda ()
	      (google-set-c-style)
	      (google-make-newline-indent)
	      (meghanada-mode t)
	      (smartparens-mode t)
	      (rainbow-delimiter-mode)
	      (highlight-symbol-mode t)
	      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing"))

(use-package gradle-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(add-to-list 'exec-path "/usr/local/bin/")

;; (setq ensime-search-interface 'helm)
;; (setq ensime-eldoc-hints 'all)
;; (add-hook 'java-mode-hook 'turn-on-eldoc-mode)

(sp-local-pair 'java-mode "(" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
(sp-local-pair 'java-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
;; (defun set-junk-directory ()
;;   "Set the directory for junk code files or
;;    scripting files (sc) for scala"
;;   (defvar-local open-junk-file-directory (expand-file-name "src/junk/scala/%Y/%m/%d-%H%M%S.sc" projectile-project-root)))

;; (add-hook 'scala-mode-hook 'set-junk-directory)
;; (setq (make-local-variable 'open-junk-file-directory) "~/.emacs.d/cache/junk/%Y/%m/%d-%H%M%S.sc")
(provide 'zen-java)
;;; zen-java ends here
