;;; core-popups.el -*- lexical-binding: t; -*-
;; https://github.com/hlissner/doom-emacs/blob/master/core/core-popups.el

(defvar zen-popup-history nil
  "A list of popups that were last closed. Used by `zen/popup-restore' and
`zen*popups-save'.")

(defvar zen-popup-other-window nil
  "The last window selected before a popup was opened.")

(defvar zen-popup-no-fringes t
  "If non-nil, disable fringes in popup windows.")

(defvar zen-popup-windows ()
  "A list of open popup windows.")

(defvar-local zen-popup-rules nil
  "The shackle rule that caused this buffer to be recognized as a popup. Don't
edit this directly.")
(put 'zen-popup-rules 'permanent-local t)

(defvar zen-popup-window-parameters
  '(:noesc :modeline :autokill :autoclose :autofit :static)
  "A list of window parameters that are set (and cleared) when `zen-popup-mode
is enabled/disabled.'")

(defvar zen-popup-remember-history t
  "Don't modify this directly. If non-nil, zen will remember the last popup(s)
that was/were open in `zen-popup-history'.")

(defvar zen-popup-inhibit-autokill nil
  "Don't modify this directly. When it is non-nil, no buffers will be killed
when their associated popup windows are closed, despite their :autokill
property.")

(defvar zen-popup-mode-map (make-sparse-keymap)
  "Active keymap in popup windows.")


(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules' (see for format details).
Several custom properties have been added that are not part of shackle, but are
recognized by zen's popup system. They are:
:noesc      If non-nil, the popup won't be closed if you press ESC from *inside*
            its window. Used by `zen/popup-close-maybe'.
:modeline   By default, mode-lines are hidden in popups unless this is non-nil.
            If it is a symbol, it'll use `zen-modeline' to fetch a modeline
            config (in `zen-popup-mode').
:autokill   If non-nil, the popup's buffer will be killed when the popup is
            closed. Used by `zen*delete-popup-window'. NOTE
            `zen/popup-restore' can't restore non-file popups that have an
            :autokill property.
:autoclose  If non-nil, close popup if ESC is pressed from outside the popup
            window.
:autofit    If non-nil, resize the popup to fit its content. Uses the value of
            the :size property as the maximum height/width. This will not work
            if the popup has no content when displayed.
:static     If non-nil, don't treat this window like a popup. This makes it
            impervious to being automatically closed or tracked in popup
            history. Excellent for permanent sidebars."
  (if (cl-every #'listp (mapcar #'zen-unquote rules))
      `(setq shackle-rules (nconc (list ,@rules) shackle-rules))
    `(push (list ,@rules) shackle-rules)))


;;
;;
;;

;; (defvar zen-popup-parameters
;;   '(:esc :modeline :transient :fit :align :size)
;;   "TODO")

;; (defvar zen-popup-whitelist
;;   '(("^ ?\\*" :size 15 :noselect t :autokill t :autoclose t))
;;   "TODO")

(defvar zen-popup-blacklist
  '("^\\*magit")
  "TODO")

;;
;; Bootstrap
;;

(def-package! shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        shackle-rules
        '(("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
          ("^\\*ftp " :noselect t :autokill t :noesc t)
          ;; zen
          ("^\\*zen:" :regexp t :size 0.35 :noesc t :select t)
          ("^ ?\\*zen " :regexp t :noselect t :autokill t :autoclose t :autofit t)
          ;; built-in (emacs)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3 :autokill t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)))

  :config
  ;; NOTE This is a temporary fix while hlissner rewrites core-popups
  (defun zen-display-buffer-condition (buffer _action)
    (and (cl-loop for re in zen-popup-blacklist
                  when (string-match-p re buffer)
                  return nil
                  finally return t)
         (shackle-match buffer)))

  (defun zen-display-buffer-action (buffer alist)
    (shackle-display-buffer buffer alist (shackle-match buffer)))

  (defun zen|autokill-popups ()
    (or (not (zen-popup-p))
        (prog1 (when (and (not zen-popup-inhibit-autokill)
                          (plist-get zen-popup-rules :autokill))
                 (zen-popup-mode -1)
                 (when-let* ((process (get-buffer-process (current-buffer))))
                   (set-process-query-on-exit-flag process nil))
                 t))))

  (add-hook! zen-post-init
    (setq display-buffer-alist
          (cons '(zen-display-buffer-condition zen-display-buffer-action)
                display-buffer-alist))
    (add-hook 'kill-buffer-query-functions #'zen|autokill-popups))

  ;; no modeline in popups
  (add-hook 'zen-popup-mode-hook #'zen|hide-modeline-in-popup)
  ;; ensure every rule without an :align, :same or :frame property has an
  ;; implicit :align (see `shackle-default-alignment')
  (advice-add #'shackle--match :filter-return #'zen*shackle-always-align)

  ;; bootstrap popup system
  (advice-add #'shackle-display-buffer :around #'zen*popup-init)
  (advice-add #'balance-windows :around #'zen*popups-save)
  (advice-add #'delete-window :before #'zen*delete-popup-window)

  ;; Tell `window-state-get' and `current-window-configuration' to recognize
  ;; these custom parameters. Helpful for `persp-mode' and persisting window
  ;; configs that have popups in them.
  (dolist (param `(popup ,@zen-popup-window-parameters))
    (push (cons param 'writable) window-persistent-parameters))

  (let ((map zen-popup-mode-map))
    (define-key map [escape]    #'zen/popup-close-maybe)
    (define-key map (kbd "ESC") #'zen/popup-close-maybe)
    (define-key map [remap quit-window] #'zen/popup-close-maybe)
    (define-key map [remap zen/kill-this-buffer] #'zen/popup-close-maybe)
    (define-key map [remap split-window-right]              #'ignore)
    (define-key map [remap split-window-below]              #'ignore)
    (define-key map [remap split-window-horizontally]       #'ignore)
    (define-key map [remap split-window-vertically]         #'ignore)
    (define-key map [remap mouse-split-window-horizontally] #'ignore)
    (define-key map [remap mouse-split-window-vertically]   #'ignore)))


;;
;; Hacks
;;

(progn ; hacks for built-in functions
  (defun zen*suppress-pop-to-buffer-same-window (orig-fn &rest args)
    (cl-letf (((symbol-function 'pop-to-buffer-same-window)
               (symbol-function 'pop-to-buffer)))
      (apply orig-fn args)))
  (advice-add #'info :around #'zen*suppress-pop-to-buffer-same-window)
  (advice-add #'eww :around #'zen*suppress-pop-to-buffer-same-window)
  (advice-add #'eww-browse-url :around #'zen*suppress-pop-to-buffer-same-window)

  (defun zen*popup-buffer-menu (&optional arg)
    "Open `buffer-menu' in a popup window."
    (interactive "P")
    (with-selected-window (zen-popup-buffer (list-buffers-noselect arg))
      (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %; q to quit; ? for help.")))
  (advice-add #'buffer-menu :override #'zen*popup-buffer-menu))


(after! comint
  (defun zen|popup-close-comint-buffer ()
    (when (and (zen-popup-p)
               (derived-mode-p 'comint-mode)
               (not (process-live-p (get-buffer-process (current-buffer)))))
      (delete-window)))
  (add-hook '+evil-esc-hook #'zen|popup-close-comint-buffer t))


(after! eshell
  ;; By tying buffer life to its process, we ensure that we land back in the
  ;; eshell buffer after term dies. May cause problems with short-lived
  ;; processes.
  ;; FIXME replace with a 'kill buffer' keybinding.
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defun zen*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (zen-popup-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
        (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'zen*eshell-undedicate-popup))


(after! evil
  (let ((map zen-popup-mode-map))
    (define-key map [remap evil-window-delete]           #'zen/popup-close-maybe)
    (define-key map [remap evil-save-modified-and-close] #'zen/popup-close-maybe)
    (define-key map [remap evil-window-move-very-bottom] #'zen/popup-move-bottom)
    (define-key map [remap evil-window-move-very-top]    #'zen/popup-move-top)
    (define-key map [remap evil-window-move-far-left]    #'zen/popup-move-left)
    (define-key map [remap evil-window-move-far-right]   #'zen/popup-move-right)
    (define-key map [remap evil-window-split]            #'ignore)
    (define-key map [remap evil-window-vsplit]           #'ignore))

  (defun zen|popup-close-maybe ()
    "If current window is a popup, close it. If minibuffer is open, close it. If
not in a popup, close all popups with an :autoclose property."
    (if (zen-popup-p)
        (unless (zen-popup-property :noesc)
          (delete-window))
      (zen/popup-close-all)))
  (add-hook '+evil-esc-hook #'zen|popup-close-maybe t)

  ;; Make evil-mode cooperate with popups
  (advice-add #'evil-command-window :override #'zen*popup-evil-command-window)
  (advice-add #'evil-command-window-execute :override #'zen*popup-evil-command-window-execute)

  (defun zen*popup-evil-command-window (hist cmd-key execute-fn)
    "The evil command window has a mind of its own (uses `switch-to-buffer'). We
monkey patch it to use pop-to-buffer, and to remember the previous window."
    (when (eq major-mode 'evil-command-window-mode)
      (user-error "Cannot recursively open command line window"))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win))
                   "*Command Line*")
        (kill-buffer (window-buffer win))
        (delete-window win)))
    (setq evil-command-window-current-buffer (current-buffer))
    (ignore-errors (kill-buffer "*Command Line*"))
    (with-current-buffer (pop-to-buffer "*Command Line*")
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil-command-window-cmd-key cmd-key)
      (evil-command-window-mode)
      (evil-command-window-insert-commands hist)))

  (defun zen*popup-evil-command-window-execute ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (popup (selected-window)))
      (select-window zen-popup-other-window)
      (unless (equal evil-command-window-current-buffer (current-buffer))
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (zen/popup-close popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Don't mess with popups
  (advice-add #'zen-evil-window-move        :around #'zen*popups-save)
  (advice-add #'evil-window-move-very-bottom :around #'zen*popups-save)
  (advice-add #'evil-window-move-very-top    :around #'zen*popups-save)
  (advice-add #'evil-window-move-far-left    :around #'zen*popups-save)
  (advice-add #'evil-window-move-far-right   :around #'zen*popups-save)

  ;; Don't block moving to/from popup windows
  (defun zen*ignore-window-parameters-in-popups (dir &optional arg window)
    (window-in-direction (cond ((eq dir 'up)   'above)
                               ((eq dir 'down) 'below)
                               (t dir))
                         window t arg windmove-wrap-around t))
  (advice-add #'windmove-find-other-window :override #'zen*ignore-window-parameters-in-popups))


(after! helm
  ;; Helm tries to clean up after itself, but shackle has already done this,
  ;; causing problems. This fixes that. To reproduce, add a helm rule in
  ;; `shackle-rules', open two splits side-by-side, move to the buffer on the
  ;; right and invoke helm. It will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t)

  (after! helm-swoop
    (setq helm-swoop-split-window-function #'pop-to-buffer))

  (after! helm-ag
    ;; This prevents helm-ag from switching between windows and buffers.
    (defun zen*helm-ag-edit-done (orig-fn &rest args)
      (cl-letf (((symbol-function 'select-window) #'ignore))
        (apply orig-fn args))
      (zen/popup-close))
    (advice-add #'helm-ag--edit-commit :around #'zen*helm-ag-edit-done)
    (advice-add #'helm-ag--edit-abort  :around #'zen*helm-ag-edit-done)

    (defun zen*helm-ag-edit (orig-fn &rest args)
      (cl-letf (((symbol-function 'other-window) #'ignore)
                ((symbol-function 'switch-to-buffer) #'zen-popup-buffer))
        (apply orig-fn args)
        (with-current-buffer (get-buffer "*helm-ag-edit*")
          (use-local-map helm-ag-edit-map))))
    (advice-add #'helm-ag--edit :around #'zen*helm-ag-edit)))


(defsubst zen--switch-from-popup (location)
  (zen/popup-close)
  (switch-to-buffer (car location) nil t)
  (if (not (cdr location))
      (message "Unable to find location in file")
    (goto-char (cdr location))
    (recenter)))

(after! help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (when (eq file 'C-source)
        (setq file (help-C-file-name (indirect-function fun) 'fun)))
      (zen--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (zen--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (zen--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))


(after! magit
  (add-hook 'magit-mode-hook #'zen-hide-modeline-mode))


(after! mu4e
  (defun zen*mu4e-popup-window (buf _height)
    (zen-popup-buffer buf '(:size 10 :noselect t))
    buf)
  (advice-add #'mu4e~temp-window :override #'zen*mu4e-popup-window))


(after! multi-term
  (setq multi-term-buffer-name "zen:terminal"))


(after! neotree
  ;; Neotree has its own window/popup management built-in, which is difficult to
  ;; police. For example, switching perspectives will cause neotree to forget it
  ;; is a neotree pane.
  ;;
  ;; By handing neotree over to shackle, which is better integrated into the
  ;; rest of my config (and persp-mode), this is no longer a problem.
  (set! :popup " *NeoTree*" :align neo-window-position :size neo-window-width :static t)

  (defun +evil-neotree-display-fn (buf _alist)
    "Hand neotree off to shackle."
    (let ((win (zen-popup-buffer buf)))
      (setq neo-global--buffer (window-buffer win)
            neo-global--window win)))
  (setq neo-display-action '(+evil-neotree-display-fn))

  (defun +evil|neotree-fix-popup ()
    "Repair neotree state whenever its popup state is restored. This ensures
that `zen*popup-save' won't break it."
    (when (equal (buffer-name) neo-buffer-name)
      (setq neo-global--window (selected-window))
      ;; Fix neotree shrinking when closing nearby vertical splits
      (when neo-window-fixed-size
        (zen-resize-window neo-global--window neo-window-width t t))))
  (add-hook 'zen-popup-mode-hook #'+evil|neotree-fix-popup))


(after! persp-mode
  (defun zen*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when-let* ((plist (zen-popup-properties window)))
        (with-selected-window window
          (unless zen-popup-mode
            (setq-local zen-popup-rules plist)
            (zen-popup-mode +1))))))
  (advice-add #'persp-load-state-from-file :after #'zen*persp-mode-restore-popups))


(after! quickrun
  ;; don't auto-focus quickrun windows, shackle handles that
  (setq quickrun-focus-p nil))


(after! twittering-mode
  (setq twittering-pop-to-buffer-function #'pop-to-buffer))


(after! wgrep
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'zen/popup-close)
  (advice-add #'wgrep-finish-edit   :after #'zen/popup-close))


(after! xref
  (defun zen*xref-follow-and-close (orig-fn &rest args)
    "Jump to the xref on the current line, select its window and close the popup
you came from."
    (interactive)
    (let ((popup-p (zen-popup-p))
          (window (selected-window)))
      (apply orig-fn args)
      (when popup-p (zen/popup-close window))))
  (advice-add #'xref-goto-xref :around #'zen*xref-follow-and-close))


;;
;; Major modes
;;

(after! plantuml-mode
  (defun zen*plantuml-preview-in-popup-window (orig-fn &rest args)
    (save-window-excursion
      (apply orig-fn args))
    (pop-to-buffer plantuml-preview-buffer))
  (advice-add #'plantuml-preview-string
              :around #'zen*plantuml-preview-in-popup-window))

;; Ensure these settings are loaded as late as possible, giving other modules a
;; chance to reconfigure org popup settings before the defaults kick in.
(defun zen|init-org-popups ()
  (add-hook! org-load
    (set! :popup
      '("*Calendar*"         :size 0.4 :noselect t)
      '(" *Org todo*"        :size 5   :noselect t)
      '("*Org Note*"         :size 10)
      '("*Org Select*"       :size 20  :noselect t)
      '("*Org Links*"        :size 5   :noselect t)
      '("*Org Export Dispatcher*" :noselect t)
      '(" *Agenda Commands*" :noselect t)
      '("^\\*Org Agenda"     :regexp t :size 20)
      '("*Org Clock*"        :noselect t)
      '("^\\*Org Src"        :regexp t :size 0.35 :noesc t)
      '("*Edit Formulas*"    :size 10)
      '("^\\*Org-Babel"      :regexp t :size 25 :noselect t)
      '("^CAPTURE.*\\.org$"  :regexp t :size 20))

    ;; Org has a scorched-earth window management system I'm not fond of. i.e.
    ;; it kills all windows and monopolizes the frame. No thanks. We can do
    ;; better with shackle's help.
    (defun zen*suppress-delete-other-windows (orig-fn &rest args)
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orig-fn args)))
    (advice-add #'org-add-log-note :around #'zen*suppress-delete-other-windows)
    (advice-add #'org-capture-place-template :around #'zen*suppress-delete-other-windows)
    (advice-add #'org-export--dispatch-ui :around #'zen*suppress-delete-other-windows)

    ;; Hand off the src-block window to a shackle popup window.
    (defun zen*org-src-pop-to-buffer (buffer _context)
      "Open the src-edit in a way that shackle can detect."
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (pop-to-buffer buffer)))
    (advice-add #'org-src-switch-to-buffer :override #'zen*org-src-pop-to-buffer)

    ;; Ensure todo, agenda, and other minor popups are delegated to shackle.
    (defun zen*org-pop-to-buffer (&rest args)
      "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
      (let ((buf (car args)))
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))))
    (advice-add #'org-switch-to-buffer-other-window :override #'zen*org-pop-to-buffer)

    ;; org-agenda
    (setq org-agenda-window-setup 'other-window
          org-agenda-restore-windows-after-quit nil)
      ;; Hide modeline in org-agenda
    (add-hook 'org-agenda-finalize-hook #'zen-hide-modeline-mode)
    (add-hook 'org-agenda-finalize-hook #'org-fit-window-to-buffer)
    ;; Don't monopolize frame!
    (advice-add #'org-agenda :around #'zen*suppress-delete-other-windows)
    ;; ensure quit keybindings work propertly
    (map! :map* org-agenda-mode-map
          :m [escape] 'org-agenda-Quit
          :m "ESC"    'org-agenda-Quit)))
(add-hook 'zen-init-hook #'zen|init-org-popups)

(provide 'zcore-popups)
;;; zcore-popups.el ends here
