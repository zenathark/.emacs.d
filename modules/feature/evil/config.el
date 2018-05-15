;;; features/evil/configure.el -- Evil configuration

(def-package! evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        shift-select-mode nil)

  :config
  (add-hook 'zen-init-hook #'evil-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)
  
  (set! :popup
    '("*evil-registers*" :size 0.3)
    '("*Command Line*" :size 8))

  (define-key evil-motion-state-map "\\" nil)

  (defun +evil*init-cursors (&rest _)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor `(,(face-foreground 'warning) box)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'+evil*init-cursors)

  (dolist (mode '(tabulated-list-mode view-mode comint-mode term-mode calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))

  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  (defun minibuffer-inactive-mode-hook-setup ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup)

  (map! (:after wgrep
          :map wgrep-mode-map [remap evil-delete] #'+evil-delete)
          [remap evil-toggle-fold] #'+evil:fold-toggle
          [remap evil-close-fold]  #'+evil:fold-close
          [remap evil-open-fold]   #'+evil:fold-open
          [remap evil-open-fold-rec] #'+evil:fold-open
          [remap evil-close-folds] #'+evil:fold-close-all
          [remap evil-open-folds]  #'+evil:fold-open-all)


  (defvar +evil-esc-hook '(t))
  (defun +evil*attach-escape-hook ()
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (abort-recursive-edit))
          ((evil-ex-hl-active-p 'evil-ex-search)
           (evil-ex-nohighlight))
          (t
           (run-hook-with-args-until-success '+evil-esc-hook))))
  (advice-add #'evil-force-normal-state :after #'+evil*attach-escape-hook)

  (defun +evil*restore-normal-state-on-windmove (orig-fn &rest args)
    (unless (memq evil-state '(normal motion emacs))
      (enil-normal-state +1))
    (apply orig-fn args))
  (advice-add #'windmove-do-window-select :around #'+evil*restore-normal-state-on-windmove)

  (defun +evil*static-reindent (orig-fn &rest args)
    (save-excursion (apply orig-fn args)))
  (advice-add #'evil-indent :around #'+evil*static-reindent)

  (advice-add #'evil-ex-replace-special-filenames :override #'zen-resolve-vim-patch)
  
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)

  (evil-ex-define-argument-type global-delim-match :runner +evil-ex-global-delim-match)

  (dolist (sym '(evil-ex-global evil-ex-global-inverted))
    (evil-set-command-property sym :ex-arg 'global-delim-match))

  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match (list (when (evil-ex-p) evil-ex-argument)))

  (evil-define-interactive-code "<//g>"
    :ex-arg global-match (list (when (evil-ex-p) evil-ex-argument)))

  (evil-set-command-properties
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :evil-mc t :keep-visual t)
  (evil-set-command-properties
   '+evil:mc :move-point nil :ex-arg 'global-match :ex-bang t :evil-mc t)

  (defun +evil*window-follow (&rest _) (evil-window-down 1))
  (defun +evil*window-vfollow (&rest _) (evil-window-right 1))
  (advice-add #'evil-window-split :after #'+evil*window-follow)
  (advice-add #'evil-window-vsplit :after #'evil-*window-vfollow))
  
(def-package! evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(def-package! evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))
  
  (defun +evil--embrace-escaped ()
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun +evil--embrace-latex ()
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun +evil--embrace-elisp-fn ()
    (cons (format "(%s " (or (read-string "(") "")) ")"))
  
  (push (cons ?\\ (make-embrace-pair-struct
                   :key ?\\
                   :read-function #'+evil--embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace-pairs-list))

  (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook #'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex)))

(def-package! evil-escape
  :commands evil-escape-mode
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
       evil-escape-excluded-major-modes '(neotree-mode)
       evil-escape-key-sequence "C-c"
       evil-escape-delay 0.25)
  (add-hook 'zen-post-init-hook #'evil-escape-mode)
  :config
  (push #'minibufferp evil-escape-inhibit-functions)
  (map! :irvo "C-g" #'evil-escape))

(def-package! evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-exchange))

(def-package! evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (map! [remap evil-jump-item] #'evilmi-jump-items
        :textobj "%" #'evilmi-text-object #'evilmi-text-object)
  :config
  (defun +evil|simple-matchit ()
    (setq-local evimi-always-simple-jump t))
  (add-hook 'python-mode-hook #'evil|simple-matchit))

(def-package! evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match))

(def-package! evil-mc
  :commands (evil-mc-make-cursor-here evil-mc-make-all-cursors
             evil-mc-undo-all-cursors evil-mc-pause-cursors
             evil-mc-resume-cursors evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)

    ;; Add custom commands to whitelisted commands
  (dolist (fn '(zen/deflate-space-maybe zen/inflate-space-maybe
                zen/backward-to-bol-or-indent zen/forward-to-last-non-comment-or-eol
                zen/backward-kill-to-bol-and-indent zen/newline-and-indent))
    (push (cons fn '((:default . evil-mc-execute-default-call)))
          evil-mc-custom-known-commands))

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes)

  (defun +evil|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-multiple-cursors))


(def-package! evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
             evil-snipe-local-mode evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t
        evil-snipe-disabled-modes '(magit-mode elfeed-show-mode elfeed-search-mode)
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))
  (add-hook 'zen-post-init-hook #'evil-snipe-mode)
  (add-hook 'zen-post-init-hook #'evil-snipe-override-mode))


(def-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(def-package! evil-vimish-fold
  :commands evil-vimish-fold-mode
  :init
  (setq vimish-fold-dir (concat zen-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  (add-hook 'zen-post-init-hook #'evil-vimish-fold-mode t))


;; Without `evil-visualstar', * and # grab the word at point and search, no
;; matter what mode you're in. I want to be able to visually select a region and
;; search for other occurrences of it.
(def-package! evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (map! :v "*" #'evil-visualstar/begin-search-forward
        :v "#" #'evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))


;;
;; Text object plugins
;;

(def-package! evil-args
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args))


(def-package! evil-indent-plus
  :commands (evil-indent-plus-i-indent
             evil-indent-plus-a-indent
             evil-indent-plus-i-indent-up
             evil-indent-plus-a-indent-up
             evil-indent-plus-i-indent-up-down
             evil-indent-plus-a-indent-up-down))


(def-package! evil-textobj-anyblock
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block))


;;
;; Multiple cursors compatibility (for the plugins that use it)
;;

;; mc doesn't play well with evil, this attempts to assuage some of its problems
;; so that any plugins that depend on multiple-cursors (which I have no control
;; over) can still use it in relative safety.
(after! multiple-cursors-core
  (map! :map mc/keymap :ne "<escape>" #'mc/keyboard-quit)

  (defvar +evil--mc-compat-evil-prev-state nil)
  (defvar +evil--mc-compat-mark-was-active nil)

  (defsubst +evil--visual-or-normal-p ()
    "True if evil mode is enabled, and we are in normal or visual mode."
    (and (bound-and-true-p evil-mode)
         (not (memq evil-state '(insert emacs)))))

  (defun +evil|mc-compat-switch-to-emacs-state ()
    (when (+evil--visual-or-normal-p)
      (setq +evil--mc-compat-evil-prev-state evil-state)
      (when (region-active-p)
        (setq +evil--mc-compat-mark-was-active t))
      (let ((mark-before (mark))
            (point-before (point)))
        (evil-emacs-state 1)
        (when (or +evil--mc-compat-mark-was-active (region-active-p))
          (goto-char point-before)
          (set-mark mark-before)))))

  (defun +evil|mc-compat-back-to-previous-state ()
    (when +evil--mc-compat-evil-prev-state
      (unwind-protect
          (case +evil--mc-compat-evil-prev-state
            ((normal visual) (evil-force-normal-state))
            (t (message "Don't know how to handle previous state: %S"
                        +evil--mc-compat-evil-prev-state)))
        (setq +evil--mc-compat-evil-prev-state nil)
        (setq +evil--mc-compat-mark-was-active nil))))

  (add-hook 'multiple-cursors-mode-enabled-hook '+evil|mc-compat-switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook '+evil|mc-compat-back-to-previous-state)

  (defun +evil|mc-evil-compat-rect-switch-state ()
    (if rectangular-region-mode
        (+evil|mc-compat-switch-to-emacs-state)
      (setq +evil--mc-compat-evil-prev-state nil)))

  ;; When running edit-lines, point will return (position + 1) as a
  ;; result of how evil deals with regions
  (defadvice mc/edit-lines (before change-point-by-1 activate)
    (when (+evil--visual-or-normal-p)
      (if (> (point) (mark))
          (goto-char (1- (point)))
        (push-mark (1- (mark))))))

  (add-hook 'rectangular-region-mode-hook '+evil|mc-evil-compat-rect-switch-state)

  (defvar mc--default-cmds-to-run-once nil))

(autoload-dir! "autoload")
;;; features/evil/configure.el ends here
