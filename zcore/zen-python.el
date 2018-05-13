;;; zen-python.el --- Python lantguage configuration
;;; Commentary:
;;; This is a configuration for docker based development in python (based on spacemacs)
;;; Code:

(use-package python
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :init
  (progn
    (setq anaconda-mode-installation-directory "~/.emacs.d/cache/anaconda-mode")
    (add-hook 'python-mode-hook 'anaconda-mode)))

(use-package cython-mode
  :ensure t)

(use-package evil-matchit
  :ensure t)

(use-package hy-mode
  :ensure t)

(use-package pip-requirements
  :ensure t)

(use-package py-isort
  :ensure t)

(use-package semantic
  :ensure t)

(use-package stickyfunc-enhance
  :ensure t)

(use-package xcscope
  :ensure t)

(use-package yapfify
  :ensure t)

(use-package helm-pydoc
  :ensure t)

(use-package nose
  :ensure t
  :config
  (progn
    (add-to-list 'nose-project-root-files "setup.cfg")
    (setq nose-use-verbose nil)))

(add-hook 'inferior-python-mode (lambda ()
				  (setq-local company-minimum-prefix-length 0)
				  (setq-local company-idle-delay 0.5)))

(mapc (lambda (x)
	(add-hook 'python-mode-hook x))
      '(eldoc-mode
	flycheck-mode
	smartparens-mode
	(add-hook 'before-save-hook 'py-isort-before-save)
	yapf-mode
	(lambda ()
	  (setq tab-width 4)
	  (setq-local comment-inline-offset 2)
	  (setq electric-indent-chars (delq ?: electric-indent-chars)))))


(general-define-key :states '(normal)
		    :keymaps '(python-mode-map)
		    "," 'hydra-python/body
		    "gd" 'meghanada-jump-declaration
		    "gb" 'meghanada-back-jump)

(defhydra hydra-python (:hint nil)
  "
^Containers^   ^Actions^
^^^^^^--------------------------------------------------------
_D_: docker    _z_: leave
	       _q_: quit
"
  ("D" hydra-docker/body)
  ("z" nil "leave")
  ("q" quit-window "quit" :color blue))

(provide 'zen-python)
;;; zen-python ends here
