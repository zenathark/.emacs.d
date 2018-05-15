;;; lang/latex/config.el

(defvar +latex-bibtex-dir "~/writing/biblio"
  "Bibtex files folder")

(defvar +latex-bibtex-default-file "default.bib"
  "Defalut bib file")

(load "auctex" nil t)

(push '("\\.[tT]e[xX]\\'" . Tex-latex-mode) auto-mode-alist)

(add-transient-hook! 'LaTeX-mode-hook
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-source-query nil
        TeX-source-correlate-start-server nil
        LaTeX-fill-break-at-separators nil
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
  
  (add-hook! (latex-mode LaTeX-mode) #'turn-on-auto-fill)
  (add-hook! 'LaTeX-mode-hook #'(LaTeX-math-mode TeX-source-correlate-mode))
  
  (set! :popup " output\\*$" :regexp t :size 15 :noselect t :autoclose t :autokill t)

  (map! :map LaTeX-mode-map "C-j" nil)

  (def-package! company-auctex
    :after (company)
    :init
    (set! :company-backend 'LaTeX-mode '(company-auctex))))

(def-package! reftex
  :commands (turn-on-reftex reftex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list +latex-bibtex-default-file)
        reftex-toc-split-windows-fraction 0.2)
  
  (add-hook! (latex-mode LaTeX-mode) #'turn-on-reftex)

  (add-hook! 'reftex-toc-mode-hook
    (reftex-toc-rescan)
    (zen-hide-modeline-mode +1)
    (map! :local
          :e "h" #'next-line
          :e "t" #'previous-line
          :e "q" #'kill-buffer-and-window
          :e "ESC" #'kill-buffer-and-window)))

(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20
        bitex-completion-bibilography (list +latex-bibtex-default-file))


  (map! :map bibtex-mode-map "C-c \\" #'bibtex-fill-entry))

(def-package! ivy-bibtex
  :after (ivy)
  :commands ivy-bibtex)

;(def-package! helm-bibtex
;  :when (featurep! :completion helm)
;  :commands helm-bibtex)
