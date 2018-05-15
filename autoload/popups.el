;;; popups.el
;;;###autoload
(defun zen-popup-p (&optional target)
  "Return t if TARGET (a window or buffer) is a popup. Uses current window if
omitted."
  (when-let* ((target (or target (selected-window))))
    (cond ((bufferp target)
           (and (buffer-live-p target)
                (buffer-local-value 'zen-popup-mode target)))
          ((windowp target)
           (and (window-live-p target)
                (window-parameter target 'popup))))))

;;;###autoload
(defun zen-popup-buffer (buffer &optional plist extend-p)
  "Display BUFFER in a shackle popup with PLIST rules. See `shackle-rules' for
possible rules. If EXTEND-P is non-nil, don't overwrite the original rules for
this popup, just the specified properties. Returns the new popup window."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (shackle-display-buffer
   buffer
   nil (or (if extend-p
               (append plist (shackle-match buffer))
             plist)
           (shackle-match buffer))))

;;;###autoload
(defun zen-popup-switch-to-buffer (buffer)
  "Switch the current (or closest) pop-up window to BUFFER."
  (unless (zen-popup-p)
    (if-let* ((popups (zen-popup-windows)))
        (select-window (car popups))
      (error "No popups to switch to")))
  (set-window-dedicated-p nil nil)
  (switch-to-buffer buffer nil t)
  (prog1 (selected-window)
    (set-window-dedicated-p nil t)))

;;;###autoload
(defun zen-popup-fit-to-buffer (&optional window max-size)
  "Fit WINDOW to the size of its content."
  (unless (string-empty-p (buffer-string))
    (let* ((window-size (zen-popup-size window))
           (max-size (or max-size (zen-popup-property :size window)))
           (size (+ 2 (if (floatp max-size) (truncate (* max-size window-size)) window-size))))
      (fit-window-to-buffer window size nil size))))

;;;###autoload
(defun zen-popup-move (direction)
  "Move a popup window to another side of the frame, in DIRECTION, which can be
one of the following: 'left 'right 'above 'below"
  (when (zen-popup-p)
    (let ((buffer (current-buffer))
          (zen-popup-inhibit-autokill t))
      (zen/popup-close)
      (zen-popup-buffer buffer `(:align ,direction) 'extend))))

;;;###autoload
(defun zen-popup-file (file &optional plist extend-p)
  "Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules."
  (unless (file-exists-p file)
    (user-error "Can't display file in popup, it doesn't exist: %s" file))
  (zen-popup-buffer (find-file-noselect file t) plist extend-p))

;;;###autoload
(defun zen-popup-windows (&optional filter-static-p)
  "Get a list of open pop up windows."
  (cl-loop for window in zen-popup-windows
           if (and (zen-popup-p window)
                   (not (and filter-static-p
                             (zen-popup-property :static window))))
           collect window))

;;;###autoload
(defun zen-popup-properties (window-or-buffer)
  "Returns a window's popup property list, if possible. The buffer-local
`zen-popup-rules' always takes priority, but this will fall back to the popup
window parameter."
  (cond ((windowp window-or-buffer)
         (or (window-parameter window-or-buffer 'popup)
             (zen-popup-properties (window-buffer window-or-buffer))))
        ((bufferp window-or-buffer)
         (buffer-local-value 'zen-popup-rules window-or-buffer))))

;;;###autoload
(defun zen-popup-property (prop &optional window)
  "Returns a `zen-popup-rules' PROPerty from WINDOW."
  (or (plist-get (zen-popup-properties (or window (selected-window)))
                 prop)
      (pcase prop
        (:size  shackle-default-size)
        (:align shackle-default-alignment))))

;;;###autoload
(defun zen-popup-side (&optional window)
  "Return what side a popup WINDOW came from ('left 'right 'above or 'below)."
  (let ((align (zen-popup-property :align window)))
    (when (eq align t)
      (setq align shackle-default-alignment))
    (when (functionp align)
      (setq align (funcall align)))
    align))

;;;###autoload
(defun zen-popup-size (&optional window)
  "Return the size of a popup WINDOW."
  (pcase (zen-popup-side window)
    ((or 'left 'right)  (window-width window))
    ((or 'above 'below) (window-height window))))

(defun zen--popup-data (window)
  (when-let* ((buffer (window-buffer window)))
    `(,(buffer-name buffer)
      :file  ,(buffer-file-name buffer)
      :rules ,(window-parameter window 'popup)
      :size  ,(zen-popup-size window))))

;;;###autoload
(defmacro with-popup-rules! (rules &rest body)
  "TODO"
  (declare (indent defun))
  `(let (shackle-rules)
     ,@(cl-loop for rule in rules
                collect `(set! :popup ,@rule))
     ,@body))

;;;###autoload
(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let ((in-popup-p (zen-popup-p))
         (popups (zen-popup-windows))
         (zen-popup-remember-history t)
         (zen-popup-inhibit-autokill t))
     (when popups
       (mapc #'zen/popup-close popups))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (zen/popup-restore)
           (unless in-popup-p
             (select-window origin)))))))


;; --- Commands ---------------------------

;;;###autoload
(defun zen/popup-restore ()
  "Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Dead special buffers or buffers
with non-nil :autokill properties will not be.
Returns t if popups were restored, nil otherwise."
  (interactive)
  (unless zen-popup-history
    (error "No popups to restore"))
  (let (any-p)
    (dolist (spec zen-popup-history)
      (let ((buffer (get-buffer (car spec)))
            (file   (plist-get (cdr spec) :file))
            (rules  (plist-get (cdr spec) :rules))
            (size   (plist-get (cdr spec) :size)))
        (when (and (not buffer) file)
          (setq buffer
                (if-let* ((buf (get-file-buffer file)))
                    (clone-indirect-buffer (buffer-name buf) nil t)
                  (find-file-noselect file t))))
        (when size
          (setq rules (plist-put rules :size size)))
        (when (and buffer (zen-popup-buffer buffer rules) (not any-p))
          (setq any-p t))))
    (when any-p
      (setq zen-popup-history '()))
    any-p))

;;;###autoload
(defun zen/popup-toggle ()
  "Toggle popups on and off. If used outside of popups (and popups are
available), it will select the nearest popup window."
  (interactive)
  (when (zen-popup-p)
    (if zen-popup-other-window
        (select-window zen-popup-other-window)
      (other-window 1)))
  (if (zen-popup-windows t)
      (let ((zen-popup-inhibit-autokill t))
        (zen/popup-close-all t))
    (zen/popup-restore)))

;;;###autoload
(defun zen/popup-close (&optional window)
  "Find and close WINDOW if it's a popup. If WINDOW is omitted, defaults to
`selected-window'. The contained buffer is buried, unless it has an :autokill
property."
  (interactive)
  (when (zen-popup-p window)
    (delete-window (or window (selected-window)))))

;;;###autoload
(defun zen/popup-close-all (&optional force-p)
  "Closes most open popups.
Does not close popups that are :static or don't have an :autoclose property (see
`shackle-rules').
If FORCE-P is non-nil (or this function is called interactively), ignore popups'
:autoclose property. This command will never close :static popups."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when-let* ((popups (zen-popup-windows t)))
    (let (success zen-popup-remember-history)
      (setq zen-popup-history (delq nil (mapcar #'zen--popup-data popups)))
      (dolist (window popups success)
        (when (or force-p (zen-popup-property :autoclose window))
          (delete-window window)
          (setq success t))))))

;;;###autoload
(defun zen/popup-kill-all ()
  "Like `zen/popup-close-all', but kill *all* popups, including :static ones,
without leaving any trace behind (muahaha)."
  (interactive)
  (when-let* ((popups (zen-popup-windows)))
    (let (zen-popup-remember-history)
      (setq zen-popup-history nil)
      (mapc #'delete-window popups))))

;;;###autoload
(defun zen/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (if (zen-popup-property :noesc)
      (call-interactively
       (if (featurep 'evil)
           #'evil-force-normal-state
         #'keyboard-quit))
    (quit-restore-window nil 'kill)))

;;;###autoload
(defun zen/popup-this-buffer ()
  "Display currently selected buffer in a popup window."
  (interactive)
  (zen-popup-buffer (current-buffer) '(:align t :autokill t)))

;;;###autoload
(defun zen/popup-toggle-messages ()
  "Toggle *Messages* buffer."
  (interactive)
  (if-let* ((win (get-buffer-window "*Messages*")))
      (zen/popup-close win)
    (zen-popup-buffer (get-buffer "*Messages*"))))

;;;###autoload
(defun zen/other-popup (count)
  "Cycle through popup windows. Like `other-window', but for popups."
  (interactive "p")
  (if-let* ((popups (if (zen-popup-p)
                        (cdr (memq (selected-window) zen-popup-windows))
                      (setq zen-popup-other-window (selected-window))
                      zen-popup-windows)))
      (ignore-errors (select-window (nth (mod (1- count) (length popups)) popups)))
    (unless (eq (selected-window) zen-popup-other-window)
      (when zen-popup-other-window
        (select-window zen-popup-other-window t)
        (cl-decf count))
      (when (/= count 0)
        (other-window count)))))

;;;###autoload
(defalias 'other-popup #'zen/other-popup)

;;;###autoload
(defun zen/popup-raise (&optional window)
  "Turn a popup window into a normal window."
  (interactive)
  (let ((window (or window (selected-window))))
    (unless (zen-popup-p window)
      (user-error "Not a valid popup to raise"))
    (with-selected-window window
      (zen-popup-mode -1))))

;;;###autoload
(defun zen/popup-move-top () "See `zen-popup-move'." (interactive) (zen-popup-move 'above))
;;;###autoload
(defun zen/popup-move-bottom () "See `zen-popup-move'." (interactive) (zen-popup-move 'below))
;;;###autoload
(defun zen/popup-move-left () "See `zen-popup-move'." (interactive) (zen-popup-move 'left))
;;;###autoload
(defun zen/popup-move-right () "See `zen-popup-move'." (interactive) (zen-popup-move 'right))


;; --- zen-popup-mode --------------------

;;;###autoload
(define-minor-mode zen-popup-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap zen-popup-mode-map
  (let ((window (selected-window)))
    ;; If `zen-popup-rules' isn't set for some reason, try to set it
    (setq-local zen-popup-rules (zen-popup-properties window))
    ;; Ensure that buffer-opening functions/commands (like
    ;; `switch-to-buffer-other-window' won't use this window).
    (set-window-parameter window 'no-other-window zen-popup-mode)
    ;; Makes popup window resist interactively changing its buffer.
    (set-window-dedicated-p window zen-popup-mode)
    (cond (zen-popup-mode
           (when zen-popup-no-fringes
             (set-window-fringes window 0 0 fringes-outside-margins))
           ;; Save metadata into window parameters so it can be saved by window
           ;; config persisting plugins like workgroups or persp-mode.
           (set-window-parameter window 'popup (or zen-popup-rules t))
           (when zen-popup-rules
             (cl-loop for param in zen-popup-window-parameters
                      when (plist-get zen-popup-rules param)
                      do (set-window-parameter window param it))))

          (t
           (when zen-popup-no-fringes
             (set-window-fringes window
                                 zen-fringe-size zen-fringe-size
                                 fringes-outside-margins))
           ;; Ensure window parameters are cleaned up
           (set-window-parameter window 'popup nil)
           (dolist (param zen-popup-window-parameters)
             (set-window-parameter window param nil))))))
(put 'zen-popup-mode 'permanent-local t)

;;;###autoload
(defun zen|hide-modeline-in-popup ()
  "Don't show modeline in popup windows without a :modeline rule. If one exists
and it's a symbol, use `zen-modeline' to grab the format. If non-nil, show the
mode-line as normal. If nil (or omitted, by default), then hide the modeline
entirely."
  (if zen-popup-mode
      (let ((modeline (plist-get zen-popup-rules :modeline)))
        (cond ((or (eq modeline 'nil)
                   (not modeline))
               (zen-hide-modeline-mode +1))
              ((and (symbolp modeline)
                    (not (eq modeline 't)))
               (setq-local zen--modeline-format (zen-modeline modeline))
               (when zen--modeline-format
                 (zen-hide-modeline-mode +1)))))
    (when zen-hide-modeline-mode
      (zen-hide-modeline-mode -1))))


;; --- Advice functions -------------------

;;;###autoload
(defun zen*shackle-always-align (plist)
  "Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule."
  (when plist
    (unless (or (plist-member plist :align)
                (plist-member plist :same)
                (plist-member plist :frame))
      (plist-put plist :align t))
    (unless (or (plist-member plist :select)
                (plist-member plist :noselect))
      (plist-put plist :select t)))
  plist)

;;;###autoload
(defun zen*popup-init (orig-fn &rest args)
  "Initializes a window as a popup window by enabling `zen-popup-mode' in it
and setting `zen-popup-rules' within it. Returns the window."
  (unless (zen-popup-p)
    (setq zen-popup-other-window (selected-window)))
  (let* ((target (car args))
         (plist (or (nth 2 args)
                    (cond ((windowp target)
                           (and (window-live-p target)
                                (shackle-match (window-buffer target))))
                          ((bufferp target)
                           (and (buffer-live-p target)
                                (shackle-match target))))))
         (buffer (get-buffer target))
         (window-min-height (if (plist-get plist :modeline) 4 2))
         window)
    (when (and (zen-real-buffer-p buffer)
               (get-buffer-window-list buffer nil t))
      (setq plist (append (list :autokill t) plist))
      (setcar args (clone-indirect-buffer (buffer-name target) nil t)))
    (unless (setq window (apply orig-fn args))
      (error "No popup window was found for %s: %s" target plist))
    (cl-pushnew window zen-popup-windows :test #'eq)
    (with-selected-window window
      (unless (eq plist t)
        (setq-local zen-popup-rules plist))
      (zen-popup-mode +1)
      (when (plist-get plist :autofit)
        (zen-popup-fit-to-buffer window)))
    window))

;;;###autoload
(defun zen*popups-save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply orig-fn args)))

;;;###autoload
(defun zen*delete-popup-window (&optional window)
  "Ensure that popups are deleted properly, and killed if they have :autokill
properties."
  (or window (setq window (selected-window)))
  (when (zen-popup-p window)
    (setq zen-popup-windows (delq window zen-popup-windows))
    (when zen-popup-remember-history
      (setq zen-popup-history (list (zen--popup-data window))))))
