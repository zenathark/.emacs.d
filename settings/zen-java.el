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
	      (rainbow-delimiters-mode)
	      (highlight-symbol-mode t)
	      (gradle-mode t)
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
  :ensure t
  :init
  (add-hook 'groovy-mode-hook
	    (lambda ()
	      (smartparens-mode t)
	      (rainbow-delimiters-mode)
	      (highlight-symbol-mode t)
	      (gradle-mode t))))

(add-to-list 'exec-path "/usr/local/bin/")

(sp-local-pair 'java-mode "(" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
(sp-local-pair 'java-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
;; (defun set-junk-directory ()
;;   "Set the directory for junk code files or
;;    scripting files (sc) for scala"
;;   (defvar-local open-junk-file-directory (expand-file-name "src/junk/scala/%Y/%m/%d-%H%M%S.sc" projectile-project-root)))

;; (add-hook 'scala-mode-hook 'set-junk-directory)
;; (setq (make-local-variable 'open-junk-file-directory) "~/.emacs.d/cache/junk/%Y/%m/%d-%H%M%S.sc")

(push '("*meghanada-typeinfo*" :dedicated t :position right :stick t :noselect nil :width 0.4)
		   popwin:special-display-config)

(push '("*helm-imenu*" :dedicated t :position right :stick t :noselect nil :width 0.4)
		   popwin:special-display-config)

(defvar zen:java-test-suffixes '("Spec.groovy", "Test.groovy", "Test.java"))

(defun zen:spock-jump-to-testcase ()
  "Jumps to spock spec by assuming the name of the file being similar to the original file plus spec."
  (interactive)
  (let ((name (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "Spec.groovy")))
    (when (file-exists-p name)
      (find-file name))))

;; Meghanada snippet changed in order to load spec.groovy and test.groovy files
(defun zen:meghanada--switch-testcase-callback (result)
  "TODO: FIX DOC OUT."
  (let ((filtered (replace-regexp-in-string "\/java" "\/groovy" result)))
    (message filtered)))
  ;; (when (and result (file-exists-p result))
  ;;   (find-file result)))

(defun zen:meghanada-switch-testcase ()
  "TODO: FIX DOC CALLBACK."
  (interactive)
  (if (and meghanada--server-process (process-live-p meghanada--server-process))
      (meghanada--send-request "st" #'zen:meghanada--switch-testcase-callback (buffer-file-name))
    (message "client connection not established")))

(provide 'zen-java)
;;; zen-java ends here
