;;; package --- Main config file
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "light green")
 '(custom-safe-themes
   (quote
    ("c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" default)))
 '(org-preview-latex-process-alist
   (quote
    ((imagemagick :programs
		  ("latex" "convert")
		  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
		  (1.0 . 1.0)
		  :latex-compiler
		  ("latexmk -pdflatex=lualatex -quiet -shell-escape -pdf -outdir=%o %f")
		  :image-converter
		  ("convert -trim -density 300 -shave 1x1 %f -quality 100 %O")))))
 '(package-selected-packages
   (quote
    (meghanada realgud highlight-symbol google-c-style google-set-c-style autodisass-java-bytecode java-mode groovy-mode gradle-mode flycheck-rust racer cargo imenu-list flycheck-pos-tip company-ycmd company-ymcd eval-sexp-fu yaml-mode julia-mode company-go go-eldoc go-mode helm-mode-manager helm-make helm-projectile helm-gtags counsel-gtags ggtags company-quickhelp racket-mode quack geiser fontawesome haskell-mode web-beautify emmet-mode org-plus-contrib fill-column-indicator modeline-posn julia-repl ein tide pyenv-mode-auto pyenv-mode web-mode cider clojure-mode popup ivy org-ref helm-core company-flx flx ensime ivy-rich xcscope srefactor aggressive-indent auto-highlight-symbol clean-aindent-mode stickyfunc-enhance popwin counsel swiper orgit git-timemachine git-messenger git-link gitconfig-mode gitattributes-mode gitignore-mode evil-magit magit page-break-lines projectile whitespace-cleanup-mode company-statistics company yasnippet smartparens sublimity flycheck exec-path-from-shell highlight-parentheses highlight-numbers open-junk-file rainbow-delimiters vi-tilde-fringe evil-search-highlight-persist evil-matchit evil-exchange evil-anzu evil-visualstar evil-surround evil-nerd-commenter evil-org evil-numbers ace-window ace-link general powerline evil hlinum base16-theme s dash use-package)))
 '(safe-local-variable-values
   (quote
    ((eval
      (setq secret-ftp-password "secret"))
     (eval
      (setq test-variable "test"))
     (org-latex-caption-above table)
     (open-junk-file . "~/github/signalp/src/junk/scala/%Y-%m-%d-%H%M%S.sc")
     (open-junk-file . "~/github/signalp/src/junk/scala/%Y/%m/%d-%H%M%S.sc")
     (default-directory file-name-directory buffer-file-name)
     (default-directory parent-directory buffer-file-name)
     (default-directory . default-directory)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; UTF-8
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(set-default 'truncate-lines nil)
(setq visible-bell t)

(require 'package)
(setq
 package-archives '(("melpa" . "http://melpa.org/packages/")
		    ("gnu" . "http://elpa.gnu.org/packages/")
		    ("org" . "http://orgmode.org/elpa/")
		    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(setq package-enable-at-startup nil)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install package if not existing.
(setq use-package-always-ensure nil)

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/gists/")
(add-to-list 'load-path "~/.emacs.d/julia-emacs/")

(setq default-input-method "latin-1-prefix")

(desktop-change-dir "~/.emacs.d/cache")
(desktop-save-mode -1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/cache/savehist")
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

(setq user-full-name "Juan Carlos Galán Hernández"
      user-mail-address "juan.galan@udlap.mx")

(setq read-quoted-char-radix 16)

(setq config-dir (expand-file-name "settings/" user-emacs-directory))
(setq gists-dir (expand-file-name "gists/" user-emacs-directory))
(add-to-list 'load-path config-dir)

;; https://github.com/jojojames/.emacs.d/blob/master/init.el
;; Set garbage collector 100MB
(setq gc-cons-threshold 100000000)


(require 'global)
(require 'zen-general)
(require 'programming)
(require 'zen-evil)
(require 'zen-scala)
(require 'zen-clojure)
(require 'zen-org)
(require 'zen-elisp)
(require 'zen-web)
(require 'zen-python)
(require 'zen-typescript)
(require 'zen-julia)
(require 'zen-racket)
(require 'zen-go)
(require 'zen-rust)
(require 'zen-java)
(require 'zen-shortcuts)

;;; init.el ends here
