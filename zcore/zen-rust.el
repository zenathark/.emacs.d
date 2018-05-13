;;; zen-rust.el --- Rust mode configuration
;;; Commentary:
;;; Code:

(use-package cargo
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'rustfmt-before-save)
			    (setq tab-width 2)
			    'smartparens-mode)))

(sp-local-pair 'rust-mode "(" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
(sp-local-pair 'rust-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))

(use-package racer
  :ensure t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rusc-src-path "~/projects/rust/src/github.com/rust-lang/rust")
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; (defvar popwin:special-display-config)
;; (push `(,go-watch-test-units-buffername :dedicated t :position bottom :stick t :noselect t :height 0.2) popwin:special-display-config)

(provide 'zen-rust)
;;; zen-rust ends here
