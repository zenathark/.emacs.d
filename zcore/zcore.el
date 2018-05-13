;;; zcore.el --- Default configuration
;;; Commentary:
;;; Code:


;;:: :: :: Defaults :: :: ::

(setq-default
 ;; Author info
 user-full-name "Juan Carlos Galán Hernández"
 user-nick-name "Zenathark"
 user-mail-address "jcgalanh@gmail.com"
 user-mail-academic "juan.galan@udlap.mx"
 user-mail-academic2 "juan.galan@itesm.mx")

(defvar zen-emacs-dir (file-truename user-emacs-directory)
  "Home directory for emacs config")

(defvar zen-local-dir (concat zen-emacs-dir ".local/")
  "Local emacs files, this directory will be added to a dropbox symlink")

(defvar zen-cache-dir (concat zen-local-dir "cache/")
  "Volatile folder")

(defvar zen-package-load-path (concat zen-emacs-dir "el-get/")
  "Packages folder")

(defvar zen-etc-dir (concat zen-local-dir "etc/")
  "Non-volatile folder")

(defvar zen-autoloads-dir (concat zen-emacs-dir "autoloads/"))
(defvar zen-autoload-file (concat zen-local-dir "autoloads.el")
  "Autoload files folder location")
(setq generated-autoload-file zen-autoload-file)

(defvar zen-core-dir (concat zen-emacs-dir "zcore/")
  "Configuration directory")

;; UTF-8 Settings
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system   'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment    'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq default-input-method "latin-1-prefix")


(setq-default
 ad-redefinition-action 'accept   ; Advised warnings off
 apropos-do-all t                     ;??
 compilation-always-kill t            ; kill compilation before starting another
 compilation-ask-about-save nil       ; save all on compilation
 compilation-scroll-output t          ; Keep scrolling when compiling
 confirm-nonexistent-file-or-buffer t ; If file doesn't exists, ask for open confirmation
 enable-recursive-minibuffers nil     ; Non buffer commands on minibuffer
 idle-update-delay 2                  ; Maybe for performance
;Don't focus minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 auto-save-default nil                ; I don't know, maybe true
 create-lockfiles  nil                ; Don't lock files on opening
 history-length 500
 make-backup-files nil               ; Again,... mmmm
 ;; files
 abbrev-file-name             (concat zen-local-dir "abbrev.el")
 auto-save-list-file-name     (concat zen-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat zen-cache-dir "backup/")))
 pcache-directory             (concat zen-cache-dir "pcache/")
 mc/list-file                 (concat zen-etc-dir "mc-list.el")
 server-auth-dir              (concat zen-etc-dir "server/")
 shared-game-score-directory  (concat zen-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat zen-cache-dir "trapm-persistency.el")
 url-cache-directory          (concat zen-cache-dir "url/")
 url-configuration-direcotry  (concat zen-cache-dir "url/")

 ; ZEN
 read-quoted-char-radix 16  ; Input utf char as hexa
 desktop-save-mode nil
 version-control nil
 vc-make-backup-file nil
 auto-save-file-name-transform '((".*" (concat zen-cache-dir "auto-save-list/")))
 )

(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
	inhibit-startup-echo-area-message user-login-name
	inhibit-default-init t
	initial-major-mode 'fundamental-mode
	initial-scratch-message nil
	mode-line-format nil))

(defvar zen-init-hook nil
  "List of hooks that run after initialization")

(defvar zen-post-init-hook nil
  "list of hooks that run after initialization its complete")

(defun zen-try-run-hook (fn hook)
  "Runs a hook wrapped in a `condition-case-unless-debug' block; its objective
is to include more information in the error message, without sacrificing your
ability to invoke the debugger in debug mode."
  (condition-case-unless-debug ex
      (if noninteractive
          (quiet! (funcall fn))
        (funcall fn))
    ('error
     (lwarn hook :error
          "%s in '%s' -> %s"
          (car ex) fn (error-message-string ex))))
  nil)

;; speed up load by increasing gc size -- nice tip!
(eval-and-compile
  (defvar zen--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    (setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6
	  file-name-handler-alist nil))

  (require 'cl-lib)
  (load (concat zen-core-dir "zcore-package") nil t)
  (add-to-list 'load-path zen-package-load-path)

  (load! zcore-lib)
  (load! zcore-os)
  

  (unless noninteractive
    (load! zcore-ui)
    (load! zcore-popups)
    (load! zcore-editor)
    (load! zcore-keybindings))

  (load! autoloads)
  
  (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
  (add-hook 'zen-init-hook 'new-empty-buffer)
  
  (defun zen|finalize ()
    (unless (or (not after-init-time) noninteractive)
      (dolist (hook '(zen-init-hook zen-post-init-hook))
	(run-hook-wrapped hook #'zen-try-run-hook hook)))
    (setq gc-cons-threshold 16777216
	  gc-cons-percentage 0.1
	  file-name-handler-alist zen--file-name-handler-alist)
    t)
  
   (add-hook 'emacs-startup-hook #'zen|finalize)
  )





;; Line display appearance
;(set-default 'truncate-lines nil)

;; Remove bell audio
;(setq visible-bell t)

;; cursor speed
;(setq auto-winwod-vscroll nil)

;; Input Method



;; Cache and backup

;(setq delete-old-versions -1)
;(setq version-control t)
;(setq vc-make-backup-files t)
;(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;(setq savehist-file "~/.emacs.d/cache/savehist")
;(savehist-mode 1)
;(setq history-delete-duplicates t)
;(setq savehist-save-minibuffer-history 1)
;(setq savehist-additional-variables
;      '(kill-ring;
;	search-ring;
;	regexp-search-ring))

;; https://github.com/jojojames/.emacs.d/blob/master/init.el
;; Set garbage collector 100MB
;(setq gc-cons-threshold 100000000)

;(scroll-bar-mode -1)
;(tool-bar-mode -1)
;(column-number-mode 1)
;(setq inhibit-splash-screen t)


(provide 'zcore)
;;; zcore.el ends here
