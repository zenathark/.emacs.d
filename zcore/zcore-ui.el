;;; zen-ui.le --- Basic UI configuration
;;; Commentary:
;;; Code:

(def-package! use-package)

(defvar zen-fringe-size 4
  "Default fringe size in pixes")

(defvar zen-theme 'base16-onedark
  "Theme to load")

(defvar zen-font 
  (cond (IS-WINDOWS "Fira Code-11")
        (IS-MAC "Fira Code-12"))
  "Default font of the buffer")

(defvar zen-variable-pitch-font
  (cond (IS-WINDOWS "Fira Sans-15")
        (IS-MAC "Fira Sans-15"))
  "Default font for the status bar")

(defvar zen-unicode-font 
  (cond (IS-WINDOWS "DejaVu Sans-11")
        (IS-MAC "DejaVu Sans-12"))
  "glyphs font")

(setq-default
 bidi-display-reordering nil ;; checking from https://github.com/hlissner/doom-emacs/blob/master/core/core-ui.el if improve something
 blink-matching-paren nil
 frame-inhibit-implied-resize t
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
			      fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ;no mouseover
 mouse-yank-at-point t           ;middle-click paste
 ibuffer-use-other-window t
 resize-mini-windows 'grow-only
 show-help-function nil
 split-width-threshold 160
 uniquify-buffer-name-style 'forward
 use-dialog-box nil
 visible-cursor nil
 x-stretch-cursor nil
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ring-bell-function #'ignore
 visible-bell nil)

(fset #'yes-or-no-p #'y-or-n-p)

(defun zen-quit-p (&optional prompt)
  "Return t if the session should be killed. Prompts the user for
confirmation"
  (if (ignore-errors (zen-real-buffer-list))
      (or (yes-or-no-p (format ">>> %s" (or prompt "Quit Emacs?")))
	  (ignore (message "Aborted")))
    t))
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions #'zen-quit-p)

(setq echo-keystrokes 0.02)


;;;TODO
;; A minor mode for toggling the mode-line
(defvar-local zen--modeline-format nil
  "The modeline format to use when `zen-hide-modeline-mode' is active. Don't
set this directly. Let-bind it instead.")
(defvar-local zen--old-modeline-format nil
  "The old modeline format, so `zen-hide-modeline-mode' can revert when it's
disabled.")
(define-minor-mode zen-hide-modeline-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if zen-hide-modeline-mode
      (setq zen--old-modeline-format mode-line-format
            mode-line-format zen--modeline-format)
    (setq mode-line-format zen--old-modeline-format
          zen--old-modeline-format nil))
  (force-mode-line-update))
;; Ensure major-mode or theme changes don't overwrite these variables
(put 'zen--modeline-format 'permanent-local t)
(put 'zen--old-modeline-format 'permanent-local t)
(put 'zen-hide-modeline-mode 'permanent-local t)

(defun zen|hide-modeline-mode-reset ()
  "Sometimes, a major-mode is activated after `zen-hide-modeline-mode' is
activated, thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically seems to kill `mode-line-format's
local value, whether or not it's permanent-local. Therefore, we cycle
`zen-hide-modeline-mode' to fix this."
  (when zen-hide-modeline-mode
    (zen-hide-modeline-mode -1)
    (zen-hide-modeline-mode +1)))
(add-hook 'after-change-major-mode-hook #'zen|hide-modeline-mode-reset)

;; no modeline in completion popups
(add-hook 'completion-list-mode-hook #'zen-hide-modeline-mode)


(setq-default window-divider-default-places t
	      window-divider-default-bottom-width 0
	      window-divider-default-right-width 1)



(defvar zen/base16-colors nil
  "color theme")
(def-package! base16-theme
  :config
  (load-theme 'base16-onedark)
  (defvar base16-onedark-colors)
  (setq zen/base16-colors base16-onedark-colors)
  (set-face-attribute 'fringe nil
		      :foreground (plist-get zen/base16-colors :base04)
		      :background (plist-get zen/base16-colors :base00)))

(set-frame-font zen-font nil)
(set-face-attribute 'fixed-pitch nil :font zen-font)
(set-fontset-font t 'unicode zen-unicode-font)
(set-face-attribute 'variable-pitch nil :font zen-variable-pitch-font)

;; todo delete-frame
(setq-default frame-title-format '("ZEN Emacs"))
(global-eldoc-mode -1)
(if (fboundp 'fringe-mode) (fringe-mode zen-fringe-size))
(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun zen|no-fringes-in-minibuffer ()
  "Disables fringes in minibuffer
https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-ui.el#L226"
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(zen-post-init-hook minibuffer-setup-hook)
	   #'zen|no-fringes-in-minibuffer)

(defun zen|protect-visible-buffers ()
  "Don't kill the current buffer if it is visible in another window (bury it instead)"
  (not (delq (selected-window)
	     (get-buffer-window-list nil nil t))))

(add-hook! doom-post-init
	   (add-hook 'kill-buffer-query-funcitons #'zen|project-visible-buffers))

(def-package! all-the-icons
  :commands (all-the-icons-octicon
	     all-the-icons-faicon
	     all-the-icons-fileicon
	     all-the-icons-wicon
	     all-the-icons-material
	     all-the-icons-alltheicon
	     all-the-icons-install-fonts))

(def-package! fringe-helper ; https://github.com/nschum/fringe-helper.el
  :commands (fringe-helper-define fringe-helper-convert))

(def-package! hideshow ; build in hide/show block comment
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config (setq hs-hide-comments-when-hiding-all nil))

(def-package! highlight-indentation ; https://github.com/antonj/Highlight-Indentation-for-Emacs
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))

(def-package! highlight-numbers ; https://github.com/Fanael/highlight-numbers
  :commands highlight-number-mode)

(def-package! hl-line ; build-in
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
	global-hl-line-sticky-flag nil)
  (when (boundp 'display-line-number) ; https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-ui.el#L287
    (defun zen--line-range ()
      (cons (line-beginning-position)
	    (cond ((save-excursion
		     (goto-char (line-end-position))
		     (and (eobp) (not (bolp))))
		   (1- (line-end-position)))
		  ((or (eobp) (save-excursion (forward-line) (eobp)))
		   (line-end-position))
		  (t
		   (line-beginning-position 2)))))
    (setq hl-line-range-function #'zen--line-range))
  (after! evil
	  (defvar-local zen-buffer-hl-line-smode nil)
	  (defun zen|disable-hl-line ()
	    (when hl-line-mode
	      (setq zen-buffer-hl-line-mode t)
	      (hl-line-mode -1)))
	  (defun zen|enable-hl-line-maybe ()
	    (if zen-buffer-hl-line-mode (hl-line-mode +1)))

	  (add-hook 'evil-visual-state-entry-hook #'zen|disable-hl-line)
	  (add-hook 'evil-visual-state-exit-hook #'zen|enable-hl-line-maybe)))

(def-package! rainbow-delimiters ; https://github.com/Fanael/rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

(def-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-width
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column)))

(defvar zen-line-numbers-style t)

(defun zen|enable-line-numbers (&optional arg)
  (cond ((boundp 'display-line-numbers)
	 (setq display-line-numbers
	       (pcase arg
		 (+1 zen-line-numbers-style)
		 (-1 nil)
		 (_ zen-line-numbers-style))))
	((eq zen-line-numbers-style 'relative)
	 (if (= arg -1)
	     (nlinum-relative-off)
	   (nlinum-relative-on)))
	((not (null zen-line-numbers-style))
	 (nlinum-mode (or arg +1)))))

(defun zen|disable-line-numbers ()
  (zen|enable-line-numbers -1))

(add-hook! (prog-mode text-mode conf-mode) #'zen|enable-line-numbers)

(def-package! nlinum
  :unless (boundp 'display-line-numbers)
  :commands nlinum-mode
  :init
  (defvar zen-line-number-lpad 4
    "padding before number")
  (defvar zen-line-number-rpad 1
    "padding after number")
  (defvar zen-line-number-pad-char 32
    "Character for padding")
  :config
  (setq nlinum-highlight-current-line t)

  (add-hook! 'hl-line-mode-hook
    (remove-overlays (point-min) (point-max) 'face 'hl-line))

  (defun zen-nlinum-format-fn (line _width)
    (let ((str (number-to-string line)))
      (setq str (concat (make-string (max 0 (- zen-line-number-lpad (length str)))
				     zen-line-number-pad-char)
			str
			(make-string zen-line-number-rpad zen-line-number-pad-char)))
      (put-text-property 0 (length str) 'face
			 (if (and nlinum-highlight-current-line
				  (= line nlinum--current-line))
			     'nlinum-current-line
			   'linum)
			 str)
      str))
  (setq nlinum-format-function #'zen-nlinum-format-fn)

  (defun zen|init-nlinum-width ()
    "Calculate line number width"
    (setq nlinum--width
	  (length (save-excursion (goto-char (point-max))
				  (format-mode-line "%1")))))
  (add-hook 'nlinum-mode-hook #'zen|init-nlinum-width))

(def-package! nlinum-hl
  :unless (boundp 'display-line-numbers)
  :after nlinum
  (advice-add #'markdown-fontify-code-block-natively
	      :after #'nlinum-hl-do-markdown-fontify-region)

  (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)
  (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows))

(def-package! nlinum-relative
  :unless (boundp 'display-line-numbers)
  :commands nlinum-relative-mode
  :config
  (after! evil (nlinum-relative-setup-evil)))

(defmacro def-modeline-segment! (name &rest forms)
  "Defines modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
	  `(let (byte-compile-warnings)
	     (byte-compile #',sym))))))

(defsubst zen--prepare-modeline-segments (segments)
  (cl-loop for seg in segments
	   if (stringp seg)
	   collect seg
	   else
	   collect (list (intern (format "zen-modeline-segment--%s" (symbol-name seg))))))

(defmacro def-modeline! (name lhs &optional rhs)
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom--prepare-modeline-segments lhs))
        (rhs-forms (doom--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun zen-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun zen-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))

;; Line numbers
;; (use-package hlinum
;;  :ensure t
;;  :config
;;  (hlinum-activate)
;;  (set-face-attribute 'linum nil;
;; 		      :foreground (plist-get zen/base16-colors :base04)
;; 		      :background (plist-get zen/base16-colors :base00))
;;  (set-face-attribute 'linum-highlight-face nil
;; 		      :foreground (plist-get zen/base16-colors :base05)
;; 		      :background (plist-get zen/base16-colors :base02)))




(provide 'zen-ui)
;;; zen-ui.el ends here
