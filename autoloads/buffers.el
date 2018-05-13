;;; buffers.el --- buffer helper functions

(defvar-local zen-buffer--narrowed-origin nil)

;;;###autoload
(defvar zen-real-buffer-functions '()
  "A list of predicate functions run to determine if a buffer is real. These
functions are iterated over with one argument, the buffer in question. If any
function returns non-nil, the procession stops and the buffer is qualified as
real.")

;;;###autoload
(defvar-local zen-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what.")

;;;###autoload
(defvar zen-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions
;;

;;;###autoload
(defun zen-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer."
  (get-buffer-create zen-fallback-buffer))

;;;###autoload
(defalias 'zen-buffer-list #'buffer-list)

;;;###autoload
(defun zen-project-buffer-list ()
  "Return a list of buffers belonging to the current project.
If no project is active, return all buffers."
  (let ((buffers (zen-buffer-list)))
    (if-let* ((project-root (if (zen-project-p) (zen-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun zen-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `zen-real-buffer-p'."
  (cl-loop for buf in (or buffer-list (zen-buffer-list))
           if (zen-real-buffer-p buf)
           collect buf))

;;;###autoload
(defun zen-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. The complete criteria for a
real buffer is:
  1. The buffer-local value of `zen-real-buffer-p' (variable) is non-nil OR
  2. Any function in `zen-real-buffer-functions' must return non-nil when
     passed this buffer OR
  3. The current buffer:
     a) has a `buffer-file-name' defined AND
     b) is not in a popup window (see `zen-popup-p') AND
     c) is not a special buffer (its name isn't something like *Help*)
If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (when-let* ((buf (ignore-errors (window-normalize-buffer buffer-or-name))))
    (or (buffer-local-value 'zen-real-buffer-p buf)
        (run-hook-with-args-until-success 'zen-real-buffer-functions buf)
        (not (or (zen-popup-p buf)
                 (minibufferp buf)
                 (string-match-p "^\\s-*\\*" (buffer-name buf))
                 (not (buffer-file-name buf)))))))

;;;###autoload
(defun zen-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (zen-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (zen-buffer-list)))))

;;;###autoload
(defun zen-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for win in (or window-list (window-list))
           unless (zen-popup-p win)
           collect win))

;;;###autoload
(defun zen-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (zen-buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun zen-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-loop for buf in (or buffer-list (zen-buffer-list))
           unless (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun zen-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (zen-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

(defun zen--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over unreal
buffers. If there's nothing left, switch to `zen-fallback-buffer'. See
`zen-real-buffer-p' for what 'real' means."
  (let ((buffers (delq (current-buffer) (zen-real-buffer-list))))
    (cond ((or (not buffers)
               (zerop (% n (1+ (length buffers)))))
           (switch-to-buffer (zen-fallback-buffer) nil t))
          ((= (length buffers) 1)
           (switch-to-buffer (car buffers) nil t))
          (t
           ;; Why this instead of switching straight to the Nth buffer in
           ;; BUFFERS? Because `switch-to-next-buffer' and
           ;; `switch-to-prev-buffer' properly update buffer list order.
           (cl-loop with move-func =
                    (if (> n 0) #'switch-to-next-buffer #'switch-to-prev-buffer)
                    for i to 20
                    while (not (memq (current-buffer) buffers))
                    do
                    (dotimes (_i (abs n))
                      (funcall move-func)))))
    (force-mode-line-update)
    (current-buffer)))

;;;###autoload
(defun zen-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq zen-real-buffer-p flag)))

;;;###autoload
(defun zen-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER (defaults to current buffer), but make sure we land on a real
buffer. Bury the buffer if the buffer is present in another window.
Will prompt to save unsaved buffers when attempting to kill them, unless
DONT-SAVE is non-nil.
See `zen-real-buffer-p' for what 'real' means."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (and (bufferp buffer)
             (buffer-live-p buffer))
    (let ((buffer-win (get-buffer-window buffer)))
      ;; deal with modified buffers
      (when (and (buffer-file-name buffer)
                 (buffer-modified-p buffer))
        (with-current-buffer buffer
          (if (and (not dont-save)
                   (yes-or-no-p "Buffer is unsaved, save it?"))
              (save-buffer)
            (set-buffer-modified-p nil))))
      ;; kill the buffer (or close dedicated window)
      (cond ((not buffer-win)
             (kill-buffer buffer))
            ((window-dedicated-p buffer-win)
             (unless (window--delete buffer-win t t)
               (split-window buffer-win)
               (window--delete buffer-win t t)))
            (t ; cycle to a real buffer
             (with-selected-window buffer-win
               (zen--cycle-real-buffers -1)
               (kill-buffer buffer)))))
    (not (eq (current-buffer) buffer))))

;;;###autoload
(defun zen-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun zen-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (zen-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (zen-kill-buffer buf t))))


;;
;; Interactive commands
;;

;;;###autoload
(defun zen/kill-this-buffer (&optional interactive-p)
  "Use `zen-kill-buffer' on the current buffer."
  (interactive (list 'interactive))
  (when (and (not (zen-kill-buffer)) interactive-p)
    (message "Nowhere left to go!")))

;;;###autoload
(defun zen/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer.
If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (zen-kill-buffer buffer dont-save)
    (cl-loop for win in windows
             if (zen-real-buffer-p (window-buffer win))
             do (with-selected-window win (zen/previous-buffer)))))

;;;###autoload
(defun zen/kill-all-buffers (&optional project-p)
  "Kill all buffers and closes their windows.
If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (zen/popup-kill-all)
  (let ((buffers (if project-p (zen-project-buffer-list) (zen-buffer-list))))
    (mapc #'zen-kill-buffer-and-windows buffers)
    (unless (zen-real-buffer-p)
      (switch-to-buffer (zen-fallback-buffer)))
    (message "Killed %s buffers" (length buffers))))

;;;###autoload
(defun zen/kill-other-buffers (&optional project-p)
  "Kill all other buffers (besides the current one).
If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (let ((buffers (if project-p (zen-project-buffer-list) (zen-buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (zen-kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun zen/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If PROJECT-P (universal argument), only kill matching buffers in the current
project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (zen-project-buffer-list) (zen-buffer-list)))
         (n (zen-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun zen/cleanup-session (&optional all-p)
  "Clean up buried buries and orphaned processes in the current workspace. If
ALL-P (universal argument), clean them up globally."
  (interactive "P")
  (run-hooks 'zen-cleanup-hook)
  (let ((buffers (zen-buried-buffers (if all-p (buffer-list))))
        (n 0)
        kill-buffer-query-functions)
    (mapc #'kill-buffer buffers)
    (setq n (+ n (length buffers) (zen/cleanup-processes)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

;;;###autoload
(defun zen/cleanup-processes ()
  "Kill all processes that have no visible associated buffers. Return number of
processes killed."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (delete-process p)
          (cl-incf n))))
    n))

;;;###autoload
(defun zen/next-buffer ()
  "Switch to the next real buffer, skipping non-real buffers. See
`zen-real-buffer-p' for what 'real' means."
  (interactive)
  (zen--cycle-real-buffers +1))

;;;###autoload
(defun zen/previous-buffer ()
  "Switch to the previous real buffer, skipping non-real buffers. See
`zen-real-buffer-p' for what 'real' means."
  (interactive)
  (zen--cycle-real-buffers -1))
