;;; zcore-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored. (doom original)

(defvar zen-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar zen-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar zen-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")


;;
(def-package! which-key
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'zen-init-hook #'which-key-mode))


(def-package! hydra
  :init
  ;; In case I later need to wrap defhydra in any special functionality.
  (defalias 'def-hydra! 'defhydra)
  (defalias 'def-hydra-radio! 'defhydradio)
  :config
  (setq lv-use-seperator t)

  (def-hydra! zen@text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))

  (def-hydra! zen@window-nav (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu
"
    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" zen/previous-buffer)
    ("n" zen/next-buffer)
    ("b" switch-to-buffer)
    ("f" find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil)))


;;
(defun zen--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.
  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
  (if modes
      (dolist (mode modes)
        (which-key-add-major-mode-key-based-replacements mode key desc))
    (which-key-add-key-based-replacements key desc)))


(defun zen--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.
For example, :nvi will map to (list 'normal 'visual 'insert). See
`zen-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l zen-evil-state-alist)) collect it
           else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put ':after        'lisp-indent-function 'defun)
(put ':desc         'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':local        'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':mode         'lisp-indent-function 'defun)
(put ':prefix       'lisp-indent-function 'defun)
(put ':textobj      'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)

;; specials
(defvar zen--keymaps nil)
(defvar zen--prefix  nil)
(defvar zen--defer   nil)
(defvar zen--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags (and whether evil is loaded or not). It was designed to make
binding multiple keys more concise, like in vim.
If evil isn't loaded, it will ignore evil-specific bindings.
States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace
    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.
    If states are omitted the keybind will be global.
    This can be customized with `zen-evil-state-alist'.
    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.
Flags
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!
Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])
Example
    (map! :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind
          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (let ((zen--keymaps zen--keymaps)
        (zen--prefix  zen--prefix)
        (zen--defer   zen--defer)
        (zen--local   zen--local)
        key def states forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (cond ((eq key :leader)
               (push 'zen-leader-key rest)
               (setq key :prefix
                     desc "<leader>"))
              ((eq key :localleader)
               (push 'zen-localleader-key rest)
               (setq key :prefix
                     desc "<localleader>")))
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          (:map*    (setq zen--defer t) (push :map rest))
          (:map
            (setq zen--keymaps (zen-enlist (pop rest))))
          (:mode
            (setq modes (zen-enlist (pop rest)))
            (unless zen--keymaps
              (setq zen--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                        (:map evil-outer-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq zen--prefix `(vconcat ,zen--prefix (kbd ,def)))
              (when desc
                (push `(zen--keybind-register ,(key-description (eval zen--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (:local
           (setq zen--local t))
          (_ ; might be a state zen--prefix
           (setq states (zen--keyword-to-states key)))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when zen--prefix
                (setq key (append zen--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(zen--keybind-register ,(key-description (eval key))
                                               ,desc ',modes)
                      forms))
              (cond ((and zen--local zen--keymaps)
                     (push `(lwarn 'zen-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    ((and zen--keymaps states)
                     (unless (featurep 'evil)
                       (throw 'skip 'evil))
                     (dolist (keymap zen--keymaps)
                       (when (memq 'global states)
                         (push `(define-key ,keymap ,key ,def) forms))
                       (when-let* ((states (delq 'global states)))
                         (push `(,(if zen--defer 'evil-define-key 'evil-define-key*)
                                 ',states ,keymap ,key ,def)
                               forms))))
                    (states
                     (unless (featurep 'evil)
                       (throw 'skip 'evil))
                     (dolist (state states)
                       (push `(define-key
                                ,(if (eq state 'global)
                                     '(current-global-map)
                                   (intern (format "evil-%s-state-%smap" state (if zen--local "local-" ""))))
                                ,key ,def)
                             forms)))
                    (zen--keymaps
                     (dolist (keymap zen--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if zen--local 'local-set-key 'global-set-key) ,key ,def)
                           forms))))
          (setq states '()
                zen--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'zcore-keybinds)
;;; core-keybinds.el ends here