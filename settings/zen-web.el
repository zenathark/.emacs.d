(use-package web-mode
  :ensure t
  :config
  (mapc (lambda (l)
	  (add-to-list 'auto-mode-alist l))
	(mapcar (lambda (i)
		  `(,i . web-mode))
		'("\\.phtml\\'"
		  "\\.tpl\\.php\\'"
		  "\\.[agj]sp\\'"
		  "\\.as[cp]x\\'"
		  "\\.erb\\'"
		  "\\.mustache\\'"
		  "\\.djhtml\\'"
		  "\\.html?\\'"
		  "\\.js[x]?\\'")))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (add-hook 'web-mode-hook 'electric-pair-mode)
  (add-hook 'web-mode-hook (lambda ()
			     (setq  web-mode-markup-indent-offset 2)
			     (setq web-mode-css-indent-offset 2)
			     (setq web-mode-code-indent-offset 2))))


(use-package js2-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package json-mode
  :ensure t)

(use-package web-beautify
  :ensure t)

(provide 'zen-web)
