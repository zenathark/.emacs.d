;;; package --- Shortcuts for all modes based on evil/spacemacs
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;;                               Scala mode
;;------------------------------------------------------------------------------

(nmap :states '(normal)
      :keymaps '(evil-operator-state-map)
      "cs" 'evil-surround-change)
(vmap "cs" 'evil-surround-region)

(nmap :prefix evil-leader
      "a" 'evil-numbers/inc-at-pt
      "x" 'evil-numbers/dec-at-pt)

;;------------------------------------------------------------------------------
;;                               Scala mode
;;------------------------------------------------------------------------------
(declare-function nmap "general.el")
(declare-function vmap "general.el")
(declare-function imap "general.el")
(declare-function general-nvmap "general.el")
(declare-function general-mmap "general.el")

(defvar evil-command)
(defvar evil-leader)

(nmap :prefix evil-command
      :kepmaps 'scala-mode-map
      "vf" 'ensime-format-source
      "vr" 'ensime-show-uses-of-symbol-at-point
      "v5i" 'ensime-inspect-type-at-point-other-frame
      "vt" 'ensime-type-at-point
      "ve" 'ensime-print-errors-at-point
      "vp" 'ensime-inspect-package-at-point
      "vo" 'ensime-inspect-project-package
      "vu" 'ensime-undo-peek
      "cc" 'ensime-typecheck-current-buffer
      "ca" 'ensime-typecheck-all
      "cr" 'ensime-reload-open-files
      "ce" 'ensime-show-all-errors-and-warnings
      "tt" 'ensime-goto-test
      "ti" 'ensime-goto-impl
      "ra" 'ensime-refactor-add-type-annotation
      "ro" 'ensime-refactor-diff-organize-imports
      "rt" 'ensime-import-type-at-point
      "rr" 'ensime-refactor-diff-rename
      "rl" 'ensime-refactor-diff-extract-local
      "rm" 'ensime-refactor-diff-extract-method
      "ri" 'ensime-refactor-diff-inline-local
      "m." 'ensime-edit-definition
      "m," 'ensime-pop-find-definition-stack
      "mn" 'ensime-forward-note
      "mp" 'ensime-backward-note
      "v." 'ensime-expand-selection-command
      "vv" 'ensime-search
      "vd" 'ensime-show-doc-for-symbol-at-point
      "vz" 'ensime-inf-switch
      "vkz" 'ensime-inf-quit-interpreter
      "vb" 'ensime-inf-eval-buffer
      "vl" 'ensime-inf-load-file
      "bs" 'ensime-sbt-switch)
(vmap :prefix evil-command
      :keymaps 'scala-mode-map
      "vr" 'ensime-inf-eval-region)
(imap :keymaps 'ensime-inf-mode-map
      "C-c" 'evil-normal-state)
(vmap :keymaps 'ensime-inf-mode-map
      "C-c" 'evil-normal-state)

;;------------------------------------------------------------------------------
;;                               Clojure mode
;;------------------------------------------------------------------------------
(nmap :prefix evil-leader "wpb" 'popwin:popup-buffer  )
(general-define-key :states '(normal)
		    :keymaps 'clojure-mode-map
		    :prefix evil-command
		    "'" 'cider-jack-in)
(general-define-key :states '(normal)
		     :keymaps 'cider-mode-map
		     :prefix evil-command
		     "ha" 'cider-apropos
		     "hh" 'cider-doc
		     "hg" 'cider-grimoire
		     "hj" 'cider-javadoc
		     "hn" 'cider-browse-ns

		     "eb" 'cider-eval-buffer
		     "ee" 'cider-eval-last-sexp
		     "ef" 'cider-eval-defun-at-point
		     "em" 'cider-macroexpand-1
		     "eM" 'cider-macroexpand-all
		     "er" 'cider-eval-region
		     "ew" 'cider-eval-last-sexp-and-replace

		     "=" 'cider-format-buffer
		     "fb" 'cider-format-buffer

		     "gb" 'cider-pop-back
		     "gC" 'cider-classpath
		     "ge" 'cider-jump-to-compilation-error
		     "gr" 'cider-jump-to-resource
		     "gn" 'cider-browse-ns
		     "gN" 'cider-browse-ns-all

		     "'" 'cider-switch-to-repl-buffer
		     "\"" 'cider-jack-in-clojurescript
		     "sb" 'cider-load-buffer
		     "sB" '(if (eq major-mode 'cider-repl-mode)
			       'cider-repl-clear-buffer
			     'cider-connect)
		     "sC" 'cider-find-and-clear-repl-output
		     "si" 'cider-jack-in
		     "sI" 'cider-jack-in-clojurescript
		     "so" 'cider-repl-switch-to-other
		     "sq" 'cider-quit
		     "ss" (if (eq major-mode 'cider-repl-mode)
			      'cider-switch-to-last-clojure-buffer
			    'cider-switch-to-repl-buffer)
		     "sx" 'cider-refresh

		     "Te" 'cider-enlighten-mode
		     "Tt" 'cider-auto-test-mode

		     "tb" 'cider-test-show-report

		     "db" 'cider-debug-defun-at-point
		     "dv" 'cider-inspect

		     "rc{" 'clojure-convert-collection-to-map
		     "rc(" 'clojure-convert-collection-to-list
		     "rc'" 'clojure-convert-collection-to-quoted-list
		     "rc#" 'clojure-convert-collection-to-set
		     "rc[" 'clojure-convert-collection-to-vector
		     )

(general-define-key :states '(normal)
		    :keymap 'cider-repl-mode-map
		    :prefix evil-command
		    "," 'cider-repl-handle-shortcut
		    "cz" 'cider-switch-to-last-clojure-buffer)

(general-define-key :states '(normal)
		    :keymap 'cider-repl-mode-map
		    "C-j" 'cider-repl-next-input
		    "C-k" 'cider-repl-previous-input)

(general-define-key :states '(normal)
		    :mode 'cider-clojure-interaction-mode
		    "ep" 'cider-eval-print-last-sexp)

;;------------------------------------------------------------------------------
;;                               Org mode
;;------------------------------------------------------------------------------
(general-define-key :states '(normal)
		    :keymaps 'org-mode-map
		    :prefix evil-command
		    "aa" 'org-ref-add-acronym-entry
		    "gi" 'org-ref-insert-glossary-link
		    "tt" 'org-todo
		    "ti" 'org-insert-todo-heading
		    "tc" 'org-toggle-checkbox
		    "'"  'org-edit-special
		    )
(setq-default c-basic-offset 4)
(general-define-key :states '(normal)
		    :keymaps 'org-mode-map
		    :prefix evil-leader
		    "m'" 'org-table-edit-field
		    "m <SPC>" 'org-ctrl-c-ctrl-c)

(general-define-key :states '(insert)
		    :keymaps 'org-mode-map
		    :prefix "C-*"
		    "aa" 'org-ref-add-acronym-entry
		    "gi" 'org-ref-insert-glossary-link
		    "'" 'org-edit-special
		    "]" 'org-ref-helm-insert-cite-link
		    )

(general-define-key :states '(normal)
		    :keymaps 'org-src-mode-map
		    :prefix evil-command
		    "'" 'org-edit-src-exit
		    "k" 'org-edit-src-abort)

;;------------------------------------------------------------------------------
;;                               Elisp mode
;;------------------------------------------------------------------------------
(defun eval-last-sexp-and-replace ()
  "This function evaluate the last sexp and replace it.
After evaluating the last sexp, it is replaced by its result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(general-define-key :states '(normal)
		    :keymaps 'emacs-lisp-mode-map
		    :prefix evil-command
		    "ee" 'eval-last-sexp
		    "ef" 'eval-defun
		    "eb" 'eval-buffer
		    "em" 'macroexpand-1
		    "eM" 'macroexpand-all
		    "ew" 'eval-last-sexp-and-replace)

(general-define-key :states '(visual)
		    :keymaps 'emacs-lisp-mode-map
		    :prefix evil-command
		    "er" 'eval-region)

;;------------------------------------------------------------------------------
;;                               Julia mode
;;------------------------------------------------------------------------------
(general-define-key :states '(normal)
		     :keymaps 'julia-repl-mode-map
		     :prefix evil-command
		     "em" 'julia-repl-macroexpand
		     "ee" 'julia-repl-edit
		     "ed" 'julia-repl-doc
		     "el" 'julia-repl-send-line
		     "er" 'julia-repl-send-region-or-line
		     "eb" 'julia-repl-send-buffer
		     "jz" #'julia-repl)

(general-define-key :states '(visual)
		    :keymaps 'julia-repl-mode-map
		    :prefix evil-command
		    "em" 'julia-repl-macroexpand
		    "ee" 'julia-repl-edit
		    "ed" 'julia-repl-doc
		    "er" 'julia-repl-send-region-or-line
		    "eb" 'julia-repl-send-buffer
		    "jz" #'julia-repl)

;;------------------------------------------------------------------------------
;;                               Racket mode
;;------------------------------------------------------------------------------
(general-define-key :states '(normal visual)
		    :keymaps 'racket-mode-map
		    :prefix evil-command
		    "g`" 'geiser-unvisit
		    "gm" 'racket-visit-module
		    "gr" 'racket-open-require-path
		    "hd" 'racket-describe
		    "hh" 'racket-doc
		    "il" 'racket-insert-lambda
		    "'" 'racket-repl
		    "sb" 'racket-run
		    "gn" 'next-error
		    "gN" 'previous-error

		    "sB" 'racket-run-and-switch-to-repl
		    "se" 'racket-send-last-sexp
		    "sf" 'racket-send-definition
		    "sr" 'racket-send-region
		    "si" 'racket-run
		    "ss" 'racket-repl
		    "tb" 'racket-test)
;;; TODO create gtags keys
(general-define-key :states '(normal visual)
		    :keymaps '(racket-mode-map)
		    :prefix evil-leader
		    "gc" 'helm-gtags-create-tags
		    "gd" 'helm-gtags-find-tag
		    "gD" 'helm-gtags-find-tag-other-window
		    "gf" 'helm-gtags-select-path
		    "gG" 'helm-gtags-dwim
		    "gi" 'helm-gtags-tags-in-this-function
		    "gl" 'helm-gtags-parse-file
		    "gn" 'helm-gtags-next-history
		    "gN" 'helm-gtags-previous-history
		    "gn" 'helm-gtags-find-rtag
		    "gR" 'helm-gtags-resume
		    "gs" 'helm-gtags-select
		    "gS" 'helm-gtags-show-stack
		    "gu" 'helm-gtags-update-tags)

(general-define-key :states '(normal)
		    :keymaps '(go-mode-map)
		    :prefix evil-command
		    "ts" 'projectile-toggle-between-implementation-and-test)

(general-define-key :states '(insert normal)
		    :keymaps '(yas-minor-mode-map)
		    "C-\\" 'yas-expand)


;;------------------------------------------------------------------------------
;;                               Java Mode
;;------------------------------------------------------------------------------

(general-define-key :states '(normal)
		    :keymaps '(java-mode-map)
		    "," 'hydra-java/body
		    "gd" 'meghanada-jump-declaration
		    "gb" 'meghanada-back-jump)

;; (defhydra hydra-java (:hint nil :exit q)
;; "
;; ^Build^          ^Code Info^             ^Actions^
;; ^^^^^^-------------------------------------------------------
;; _c_: compile      _g_: Go to             _z_: leave
;; _t_: test         _G_: Magit             _q_: exit
;; _r_: run task

;; "
;;   ("c" zen:gradle-compile-java)
;;   ("t" hydra-java-test/body)
;;   ("r" gradle-run)
;;   ("g" hydra-java-go/body)
;;   ("G" magit-status)
;;   ("z" nil "leave")
;;   ("q" exit "quit" :color blue))

;; (defun zen:gradle-compile-java ()
;;   (interactive)
;;   (gradle-execute "compileJava"))

;; (defun zen:gradle-db-single-test ()
;;   (interactive)
;;   (let ((test-name (read-string "Test Class:")))
;;     (gradle-execute (concat "test --debug-jvm --tests " test-name))))

;; (defhydra hydra-java-go (:hint nil :exit q)
;; "
;; ^Find^
;; ^^^^^^-------------------------------------------------------
;; _t_: Tag             _s_: Symbol          _T_: Tag from here
;; _r_: Ref             _g_: Pattern         _P_: File
;; _o_: Tag on other    _f_: Parse           _z_: leave
;;		     _*_: Back		  _q_: exit
;; "
;;   ("T" helm-source-gtags-find-tag-from-here)
;;   ("*" helm-gtags-pop-stack)
;;   ("P" helm-gtags-find-files)
;;   ("f" helm-gtags-parse-file)
;;   ("g" helm-gtags-find-pattern)
;;   ("s" helm-gtags-find-symbol)
;;   ("r" helm-gtags-find-rtag)
;;   ("t" helm-gtags-find-tag)
;;   ("o" helm-gtags-find-tag-other-window)
;;   ("z" nil "leave")
;;   ("q" exit "quit" :color blue))

;; (defhydra hydra-java-compile (:hint nil :exit q)
;; "
;; ^Compile^
;; ^^^^^^-----------
;; _c_: file
;; _t_: project
;; _z_: leave
;; _q_: quit
;; "
;;   ("c" ensime-sbt-do-compile)
;;   ("z" nil "leave")
;;   ("q" exit "quit" :color blue))

;; (defhydra hydra-java-test (:hint nil :exit q)
;; "
;; ^Tests^
;; ^^^^^^-----------
;; _t_: Run single test
;; _T_: Run all tests
;; _z_: leave
;; _q_: quit
;; "
;;   ("T" gradle-test)
;;   ("t" gradle-single-test)
;;   ("z" nil "leave")
;;   ("q" exit "quit" :color blue))
(provide 'zen-shortcuts)
;;; zen-shortcuts ends here
