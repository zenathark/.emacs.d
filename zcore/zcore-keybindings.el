;;; zcore-keybindings.el --- Custom emacs keybindings
;;; Commentary:
;;; Code:

(use-package general)
(use-package dash)

;; Sane key-bindings
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)
(delete-selection-mode 1)
(def-package! undo-tree
  :config
  (global-undo-tree-mode))
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)
(global-set-key (kbd "C-<f4>") 'kill-current-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-S-e") (lambda ()
				(interactive)
				(dired "~/")))
(global-set-key (kbd "C-o") 'find-file)

(global-set-key (kbd "C-n") 'new-empty-buffer)

(general-define-key :keymaps '(dired-mode-map)
		    "C-b" 'switch-to-previous-buffer
		    "C-S-e" 'kill-current-buffer)



(provide 'zcore-keybindings)
;;; zcore-keybindings.el ends here
