;;; packages.el --- Basic UI configuration
;;; Commentary:
;;; Code:

;; Utilities
(use-package dash :ensure t) ;;list manipulation https://github.com/magnars/dash.el
(use-package s :ensure t) ;;string manipulation https://github.com/magnars/s.el
;; UI
(use-package all-the-icons :ensure t) ;; icons for ui ala seti-ui https://github.com/domtronn/all-the-icons.el
(use-package fringe-helper :ensure t) ;; drawing on the fringe https://github.com/nschum/fringe-helper.el
(use-package highlight-indentation :ensure t) ;; draw line on indentation https://github.com/antonj/Highlight-Indentation-for-Emacs
(use-package highlight-numbers :ensure t) ;; highlight numbers on source code https://github.com/Fanael/highlight-numbers
(use-package rainbow-delimiters :ensure t) ;; colored brackets https://github.com/Fanael/rainbow-delimiters
(use-package visual-fill-column :ensure t) ;; wrap at fill column https://github.com/joostkremers/visual-fill-column
(use-package shackle :ensure c) ;; rules for popup windows https://github.com/wasamasa/shackle

(provide 'packages)
;;; packages.el ends here
