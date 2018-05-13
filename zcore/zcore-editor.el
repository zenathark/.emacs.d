;;; zcore-editor.el -*- lexical-binding: t; -*-

(defvar zen-large-file-size 1
  "Size of file (MB) for open warning")

(defvar zen-large-file-modes-alist
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
	doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes ignored for check large file")

(setq-default
 vc-follow-symlinks t
 save-interprogram-paste-befor-kill t
 bookmark-default-file (concat zen-etc-dir "bookmarks")
 bookmark-zane-flag t
 delete-trailing-lines nil
 fill-column 79
 sencence-end-double-space nil
 word-wrap t
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; :retab?
 truncate-lines t
 truncate-partial-width-windows 50
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
       trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› \t])
   
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\ [?·] [?.])))

(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun zen*quit-window (orig-fn &optional kill window)
  (funcall orig-fn (not kill) window))
(advice-add #'quit-window :around #'zen*quit-window)

(defun zen|check-large-file ()
  "Check if file too big. If so, ask for open confirmation"
  (let* ((filename (buffer-file-name))
	 (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode zen-large-file-modes-alist))
	       size (> size (* 1024 1024 zen-large-file-size))
	       (y-or-n-p
		(format (concat "%s is a large file, open literaly to "
				"avoid performance issues?")
			(file-relative-name filename))))
      (setq buffer-read-only-t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'zen|check-large-file)

(push '("/LICENSE$" . text-mode) auto-mode-alist)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(electric-indent-mode -1)

(setq savehist-file (concat zen-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat zen-cache-dir "saveplace"))
(add-hook! 'zen-init-hook #'(savehist-mode save-place-mode))

(def-package! recentf
  :hook (zen-init . recentf-mode)
  :config
  (setq recentf-save-file (concat zen-cache-dir "recentf")
	recentf-max-menu-items 0
	recentf-max-saved-items 3000
	recentf-filename-handlers '(file-truename)
	recentf-exclude
	(list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
	      "^/var/folders.+$"
	      ;; ignore private temp files
	      (concat "^" (file-truename zen-local-dir)))))


(def-package! editorconfig
  :config
  (add-hook 'doom-init-hook #'editorconfig-mode)

  (defvar zen-editorconfig-mode-alist
    '((sh-mode        . "sh")
      (python-mode    . "py")
      (ruby-mode      . "rb")
      (perl-mode      . "pl")
      (php-mode       . "php"))
    "Major modes list")

  (defun zen*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (file-name-extension buffer-file-name)
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (let ((ext (cdr (assq major-mode zen-editorconfig-mode-alist))))
                       (or (and ext (concat "." ext))
                           ""))))))
      (apply orig-fn args)))

  (advice-add #'editorconfig-call-editorconfig-exec :around #'zen*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation. I prefer dynamic indentation support
  ;; built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (setq editorconfig-indentation-alist
      (assq-delete-all mode editorconfig-indentation-alist)))

  (defvar whitespace-style)
  (defun zen|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'zen|editorconfig-whitespace-mode-maybe))

  
(def-package! editorconfig-conf-mode
  :mode "\\.?editorconfig$")

(def-package! smartparens ; https://github.com/Fuco1/smartparens
  :hook (zen-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)

  (setq sp-autowrap-region nil ; evil will do this
	sp-highlight-pair-overlay nil
	sp-cancel-autoskips-on-backward-movement nil
	sp-show-pair-delay 0
	sp-max-pair-length 3)

  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
		 :post-handlers '(("|" "SPC"))))
 
(def-package! undo-tree ; built in
  :config
  (add-hook 'zen-init-hook #'global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil
	undo-tree-history-directory-alist
	(list (cons "." (concat zen-cache-dir "undo-tree-hist/")))))

(def-package! ace-link ; https://github.com/abo-abo/ace-link
  :commands (ace-link-help ace-link-org))

(def-package! avy ; https://github.com/abo-abo/avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
	avy-background t))

(def-package! command-log-mode ; https://github.com/lewang/command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set! :popup "*command-log*" :size 40 :align 'right :noselect t)
  (setq command-log-mode-auto-show t
	command-log-mode-open-log-turns-on-mode t))

(def-package! expand-region; https://github.com/magnars/expand-region.el
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(def-package! help-fns+ ; https://www.emacswiki.org/emacs/help-fns+.el
  :commands (describe-buffer
	     describe-command
	     describe-file
	     describe-keymap
	     describe-option
	     describe-option-of-type))

(def-package! pcre2el ; https://github.com/joddie/pcre2el
  :commands rxt-quote-pcre)

(def-package! smart-forward ; https://github.com/magnars/smart-forward.el
  :commands (smart-up smart-down smart-backward smart-forward))

(def-package! wgrep ;https://github.com/mhayashi1120/Emacs-wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))

(provide 'zcore-editor)
;;; zcore-editor.el ends here
			    
