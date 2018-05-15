;;; packages.el --- list of packages for installation using quelpa
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))


(require 'package)
(setq package-archives
            '(("melpa" . "http://melpa.org/packages/")
              ("gnu" . "http://elpa.gnu.org/packages/")
              ("org" . "http://orgmode.org/elpa/")
              ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-refresh-contents)

(require 'el-get)

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "/el-get-user/recipes"))
;(el-get 'sync)

;; utilities
(el-get-bundle use-package)
(el-get-bundle edvorg/use-package-el-get)
(el-get-bundle dash)
(el-get-bundle s)

;; ui
(el-get-bundle all-the-icons)
(el-get-bundle fringe-helper)
(el-get-bundle highlight-indentation)
(el-get-bundle highlight-numbers)
(el-get-bundle rainbow-delimiters)
(el-get-bundle visual-fill-column)
(el-get-bundle shackle)
(el-get-bundle nlinum)
(el-get-bundle nlinum-hl)
(el-get-bundle nlinum-relative)
(el-get-bundle base16-theme
  :features nil)
(el-get-bundle eldoc-eval)
(el-get-bundle shrink-path
               :type git
               :url "https://gitlab.com/bennya/shrink-path.el.git")

;; editor
(el-get-bundle hideshow)
(el-get-bundle ace-link)
(el-get-bundle avy)
(el-get-bundle command-log-mode)
(el-get-bundle expand-region)
(el-get-bundle emacswiki:help-fns+)
(el-get-bundle pcre2el)
(el-get-bundle smart-forward)
(el-get-bundle wgrep)
(el-get-bundle editorconfig)
(el-get-bundle undo-tree)
(el-get-bundle smartparens
  :features nil)

;; keybinds
(el-get-bundle which-key)
(el-get-bundle hydra)

;; evil
(el-get-bundle general)
(el-get-bundle evil-anzu)
(el-get-bundle evil)
(el-get-bundle evil-args)
(el-get-bundle evil-commentary)
(el-get-bundle evil-easymotion)
(el-get-bundle evil-embrace)
(el-get-bundle evil-escape)
(el-get-bundle evil-exchange)
(el-get-bundle evil-indent-plus)
(el-get-bundle evil-matchit)
(el-get-bundle evil-mc)
(el-get-bundle evil-multiedit)
(el-get-bundle evil-numbers)
(el-get-bundle evil-textobj-anyblock)
(el-get-bundle evil-snipe)
(el-get-bundle evil-surround)
(el-get-bundle evil-vimish-fold)
(el-get-bundle evil-visualstar)

;; latex
(el-get-bundle auctex)
(el-get-bundle company-auctex)
;(el-get-bundle ivy-bibtex)
;(el-get-bundle helm-bibtex)

;; org
(el-get-bundle org)
(el-get-bundle hlissner/org-bullets)
(el-get-bundle toc-org)

(el-get-bundle ob-go)
(el-get-bundle krisajenkins/ob-mongo)
(el-get-bundle stardiviner/ob-redis)
(el-get-bundle pashky/restclient.el)
(el-get-bundle alf/ob-restclient.el)
(el-get-bundle zweifisch/ob-rust)
(el-get-bundle nikclayton/ob-sql-mode)
(el-get-bundle krisajenkins/ob-translate)

(el-get-bundle ox-pandoc)

(el-get-bundle centered-window-mode)
(el-get-bundle org-tree-slide)
(el-get-bundle ox-reveal)
