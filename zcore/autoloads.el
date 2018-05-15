;;; autoloads.el --- require all files on autoload path



(Update-directory-autoloads zen-autoloads-dir)
(mapc 'load (seq-drop (directory-files zen-autoloads-dir t) 2))
(update-directory-autoloads zen-autoloads-dir)
(mapc 'load (seq-drop (directory-files zen-autoloads-dir t) 2))(update-directory-autoloads zen-autoloads-dir)
(mapc 'load (seq-drop (directory-files zen-autoloads-dir t) 2))
(provide 'autoloads)
;;; autoloads.el ends here
