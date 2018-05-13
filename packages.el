;;; packages.el --- list of packages for installation using quelpa
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))


(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")
               ("gnu" . "http://elpa.gnu.org/packages/")
               ("org" . "http://orgmode.org/elpa/")
               ("melpa-stable" . "http://stable.melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize))

(require 'el-get)

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "/el-get-user/recipes"))
(el-get 'sync)

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
;(el-get-bundle )
;(el-get-bundle )

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

;; evil
(el-get-bundle general)
