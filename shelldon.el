;;; shelldon.el --- An enhanced shell interface -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne <scmorris.dev@gmail.com>
;; Version: 1.0
;; URL: https://github.com/Overdr0ne/shelldon
;; Package-Requires: ((emacs "27.1") (exec-path-from-shell))
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; https://github.com/Overdr0ne/shelldon
;; Shelldon is largely just a modification of async-shell-command that
;; provides a more complete minibuffer workflow by separating command
;; outputs into searchable, separate buffers among other things.

;;; Code:

(require 'cl-lib)
(require 'shell)
(require 'dired)
(require 'notifications)

;;; ============================================================================
;;; Core Configuration
;;; ============================================================================

(defgroup shelldon nil
  "A shell command interface that keeps track of output buffers."
  :group 'convenience
  :prefix "shelldon-")

(defcustom shelldon-prompt-str ">> "
  "A string prepending the shelldon prompt, much like the PS1 EV in BASH."
  :type 'editable-field)

(defcustom shelldon-autohistory-p t
  "Set to t to automatically complete history for shelldon commands."
  :type 'toggle)

(defcustom shelldon-ansi-colors nil
  "Toggle ANSI color output on shelldon's output."
  :type 'toggle)

(defcustom shelldon-desktop-notify-p t
  "Set to t to use desktop notifications to notify when async commands complete."
  :type 'toggle)

(defcustom shelldon-command-auto-load-history-p t
  "Set to t to automatically complete history for shelldon commands."
  :type 'toggle)

;;; ============================================================================
;;; History Management
;;; ============================================================================

(defvar shelldon--hist '()
  "History of executed shelldon commands and their buffers.")

(defun shelldon-slurp (file)
  "Read contents of FILE into a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun shelldon-command-history-to-list ()
  "Convert shell history file contents to a list of commands."
  (seq-filter (lambda (str)
                (string-match-p "[A-z].*" str))
              (split-string (shelldon-slurp (getenv "HISTFILE")) "\n")))

(defun shelldon-command-auto-history ()
  "Load shell command history if enabled."
  (if shelldon-command-auto-load-history-p
      (shelldon-command-history-to-list)))

(defvar shelldon-command-history (shelldon-command-auto-history)
  "History list of shell commands.")

(defun shelldon--output-buffer-name (command)
  (concat "*shelldon:" (number-to-string (length shelldon--hist)) ":" command "*"))

;;; ============================================================================
;;; Minibuffer and Input Handling
;;; ============================================================================

(defun shelldon-cd ()
  "Change directories without leaving shelldon context.
Get the workdir, then throw it back for the shelldon command to set it in that context."
  (interactive)
  (let (shelldon-wd)
    (setq shelldon-wd (call-interactively #'cd))
    (throw 'shelldon-cwd shelldon-wd)))

(defvar shelldon-minibuffer-local-command-map
  (let ((map (copy-keymap minibuffer-local-shell-command-map)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-x C-f") #'shelldon-cd)
    map)
  "Keymap used for completing shell commands in minibuffer.")

(defun shelldon--get-command ()
  "Get command string from the user with appropriate completion."
  (minibuffer-with-setup-hook
      (lambda ()
        (shell-completion-vars)
        (set (make-local-variable 'minibuffer-default-add-function)
             'minibuffer-default-add-shell-commands))
    (let* ((prompt (format-message "%s%s"
                                   (abbreviate-file-name default-directory)
                                   shelldon-prompt-str))
           (initial-contents nil)
           (filename-default (when buffer-file-name
                               (file-relative-name buffer-file-name)))
           (dired-default (when (eq major-mode 'dired-mode)
                            (dired-get-filename nil t)))
           (file-default (or filename-default dired-default))
           (defaults (when file-default (list (list file-default))))
           (command
            (if shelldon-autohistory-p
                ;; Use completing-read with history
                (completing-read prompt shelldon-command-history
                                 nil nil initial-contents 'shelldon-command-history)
              ;; Use standard read-from-minibuffer
              (read-from-minibuffer prompt initial-contents
                                    shelldon-minibuffer-local-command-map
                                    nil
                                    'shelldon-command-history
                                    defaults))))
      command)))

;;; ============================================================================
;;; Buffer Management
;;; ============================================================================

(defun shelldon--generate-buffer-name (command)
  "Generate the buffer name for COMMAND."
  (concat "*shelldon:" (number-to-string (length shelldon--hist)) ":" command "*"))

(defun shelldon--track-buffer (command buffer-name)
  "Add COMMAND and BUFFER-NAME to shelldon history."
  (add-to-list 'shelldon--hist
               `(,(concat (number-to-string (length shelldon--hist)) ":" command)
                 . ,buffer-name)))

(defun shelldon-command-set-point-to-bob (&optional buffer)
  "Set point to beginning of BUFFER after command completion."
  (let* ((buf (or buffer (current-buffer)))
         (pos (alist-get buf shell-command-saved-pos)))
    ;; Remove the saved position
    (setq shell-command-saved-pos
          (assq-delete-all buf shell-command-saved-pos))

    (when (buffer-live-p buf)
      (let ((win (car (get-buffer-window-list buf))))
        (if win
            ;; Set point in the existing window
            (progn
              (set-window-point win 0)
              (with-selected-window win (fit-window-to-buffer)))
          ;; No window showing buffer - display it temporarily
          (when pos
            (with-current-buffer buf (goto-char 0))
            (with-selected-window win (fit-window-to-buffer)))
          (save-window-excursion
            (let ((win (display-buffer
                        buf
                        '(nil (inhibit-switch-frame . t)))))
              (set-window-point win 0)
              (with-selected-window win (fit-window-to-buffer)))))))))

(defun shell-command-save-pos-or-erase (&optional output-to-current-buffer)
  "Save position or erase buffer based on settings.
With OUTPUT-TO-CURRENT-BUFFER, handle current buffer case."
  (if (and (not output-to-current-buffer)
           (not (eq shell-command-dont-erase-buffer 'noncontiguous)))
      ;; We are not inserting into the current buffer,
      ;; and we are recreating the buffer, so erase it.
      (erase-buffer)
    ;; We are going to keep the existing buffer, including its
    ;; contents, so record its current size to use as the beginning of
    ;; the inserted output.
    (push (cons (current-buffer) (point-max))
          shell-command-saved-pos)))

;;; ============================================================================
;;; Notification and Process Handling
;;; ============================================================================

(defun shelldon-on-action-function (_ key)
  "Jump to buffer KEY when notification action clicked."
  (pop-to-buffer key))

(defun shelldon-on-close-function (_ _)
  "Handle notification close action."
  nil)

(defun shelldon-command-sentinel (process signal)
  "Handle shell command PROCESS completion with SIGNAL.
Shows notification and updates mode line."
  (when (memq (process-status process) '(exit signal))
    (shelldon-command-set-point-to-bob (process-buffer process))
    (when shelldon-desktop-notify-p
      (let ((status-string (format "%s: %s."
                                   (car (cdr (cdr (process-command process))))
                                   (substring signal 0 -1)))
            (bname (buffer-name (process-buffer process))))
        ;; Create desktop notification
        (notifications-notify
         :title status-string
         :body "Open the output buffer?"
         :actions `(,bname "Jump to output")
         :on-action 'shelldon-on-action-function
         :on-close 'shelldon-on-close-function)
        (message status-string)))))

;;; ============================================================================
;;; Region Handling Functions
;;; ============================================================================

(defun shelldon--command-on-region-noncontiguous (start end command
                                                        &optional output-buffer replace)
  "Execute COMMAND on noncontiguous region from START to END.
Optionally use OUTPUT-BUFFER and REPLACE the region."
  (let ((input (concat (funcall region-extract-function
                                (when replace 'delete))
                       "\n"))
        (shell-command-switch "-c") ;; synchronous commands can’t be run interactively
        output)
    ;; Process the extracted region
    (with-temp-buffer
      (insert input)
      (call-process-region (point-min) (point-max)
                           shell-file-name t t
                           nil shell-command-switch
                           command)
      (setq output (split-string (buffer-substring
                                  (point-min)
                                  ;; Trim the trailing newline
                                  (if (eq (char-before (point-max)) ?\n)
                                      (1- (point-max))
                                    (point-max)))
                                 "\n")))
    ;; Handle output appropriately
    (cond
     (replace
      (goto-char start)
      (funcall region-insert-function output))
     (t
      (let ((buffer (get-buffer-create
                     (or output-buffer shell-command-buffer-name))))
        (with-current-buffer buffer
          (erase-buffer)
          (funcall region-insert-function output))
        (display-message-or-buffer buffer))))))

(defun shelldon--command-replace-region-contiguous (start end command replace error-file)
  "Replace region from START to END with COMMAND output.
Uses REPLACE mode and ERROR-FILE for error handling."
  (let ((swap (and replace (< start end)))
        (shell-command-switch "-c") ;; synchronous commands can’t be run interactively
        exit-status)
    ;; Don't modify mark unless REPLACE says we should
    (goto-char start)
    (when (and replace
               (not (eq replace 'no-mark)))
      (push-mark (point) 'nomsg))

    ;; Execute the command and capture exit status
    (setq exit-status
          (call-shell-region start end command replace
                             (if error-file
                                 (list t error-file)
                               t)))

    ;; Handle mark position if replacing and swapping
    (when (and replace swap
               (not (eq replace 'no-mark)))
      (exchange-point-and-mark))
    exit-status))

(defun shelldon--command-output-to-buffer (start end command
                                                 &optional output-buffer replace
                                                 error-file)
  "Send COMMAND output on region from START to END to OUTPUT-BUFFER.
Handles REPLACE mode and ERROR-FILE for error handling."
  (let ((buffer (get-buffer-create
                 (or output-buffer shell-command-buffer-name)))
        (shell-command-switch "-c") ;; synchronous commands can’t be run interactively
        exit-status)
    ;; Enable modes that should be global
    (set-buffer-major-mode buffer)

    (unwind-protect
        (if (and (eq buffer (current-buffer))
                 (or (memq shell-command-dont-erase-buffer '(nil erase))
                     (and (not (eq buffer (get-buffer
                                           shell-command-buffer-name)))
                          (not (region-active-p)))))
            ;; Input is same as output buffer - handle specially
            (progn
              (setq buffer-read-only nil)
              (delete-region (max start end) (point-max))
              (delete-region (point-min) (min start end))
              (setq exit-status
                    (call-process-region (point-min) (point-max)
                                         shell-file-name t
                                         (if error-file
                                             (list t error-file)
                                           t)
                                         nil shell-command-switch
                                         command)))
          ;; Standard case - clear output buffer, then run command
          (let ((directory default-directory))
            (with-current-buffer buffer
              (if (not output-buffer)
                  (setq default-directory directory))
              (shell-command-save-pos-or-erase)))
          (setq exit-status
                (call-shell-region start end command nil
                                   (if error-file
                                       (list buffer error-file)
                                     buffer))))

      ;; Process complete - report output
      (with-current-buffer buffer
        ;; Setup revert functionality
        (setq-local revert-buffer-function
                    (lambda (&rest _)
                      (shelldon-command command)))
        ;; Update mode line with process status
        (setq mode-line-process
              (cond ((null exit-status)
                     " - Error")
                    ((stringp exit-status)
                     (format " - Signal [%s]" exit-status))
                    ((not (equal 0 exit-status))
                     (format " - Exit [%d]" exit-status)))))

      ;; Display output appropriately
      (if (with-current-buffer buffer (> (point-max) (point-min)))
          ;; There's output, display it
          (progn
            (display-message-or-buffer buffer)
            (shell-command-set-point-after-cmd buffer))
        ;; No output - maybe error?
        (let ((error-output
               (if (and error-file
                        (< 0 (file-attribute-size
                              (file-attributes error-file))))
                   (format "some error output%s"
                           (if shell-command-default-error-buffer
                               (format " to the \"%s\" buffer"
                                       shell-command-default-error-buffer)
                             ""))
                 "no output")))
          ;; Show appropriate message
          (cond ((null exit-status)
                 (message "(Shell command failed with error)"))
                ((equal 0 exit-status)
                 (message "(Shell command succeeded with %s)"
                          error-output))
                ((stringp exit-status)
                 (message "(Shell command killed by signal %s)"
                          exit-status))
                (t
                 (message "(Shell command failed with code %d and %s)"
                          exit-status error-output))))))))

(defun shelldon--output-current-buffer-p (output-buffer)
  "Check if OUTPUT-BUFFER is the current buffer."
  (and output-buffer
       (or (eq output-buffer (current-buffer))
           (and (stringp output-buffer) (eq (get-buffer output-buffer) (current-buffer)))
           (not (or (bufferp output-buffer) (stringp output-buffer))))))

(defun shelldon--output-current-buffer (command output-buffer error-buffer)
  "Execute COMMAND with output to current buffer.
Handles ERROR-BUFFER for error output."
  (let ((error-file
         (and error-buffer
              (make-temp-file
               (expand-file-name "scor"
                                 (or small-temporary-file-directory
                                     temporary-file-directory))))))
    ;; Validate buffer is writable
    (barf-if-buffer-read-only)
    (push-mark nil t)

    ;; Prepare buffer for output
    (shell-command-save-pos-or-erase 'output-to-current-buffer)

    ;; Execute the command
    (call-process-shell-command command nil
                                (if error-file
                                    (list t error-file)
                                  t))

    ;; Process error output if any
    (when (and error-file (file-exists-p error-file))
      (when (< 0 (file-attribute-size (file-attributes error-file)))
        (with-current-buffer (get-buffer-create error-buffer)
          (let ((pos-from-end (- (point-max) (point))))
            (or (bobp)
                (insert "\f\n"))
            ;; Insert error file contents
            (format-insert-file error-file nil)
            ;; Position point appropriately
            (goto-char (- (point-max) pos-from-end)))
          (display-buffer (current-buffer))))
      (delete-file error-file))

    ;; Exchange point and mark without activating mark
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point)
                             (current-buffer))))))

;;; ============================================================================
;;; Command Execution Core
;;; ============================================================================

(defun shelldon-command-on-region (start end command
                                         &optional output-buffer replace
                                         error-buffer display-error-buffer
                                         region-noncontiguous-p)
  "Execute string COMMAND in inferior shell with region as input.
Executes on region from START to END, optionally sending to OUTPUT-BUFFER.
With REPLACE, replaces the region with command output.
ERROR-BUFFER specifies where to send error output.
With DISPLAY-ERROR-BUFFER, shows error buffer if there are errors.
REGION-NONCONTIGUOUS-P indicates a non-contiguous region."
  (interactive (let (string)
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 ;; Read command before getting region bounds to avoid
                 ;; issues with subprocess output moving them
                 (setq string (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end)
                       string
                       current-prefix-arg
                       current-prefix-arg
                       shell-command-default-error-buffer
                       t
                       (region-noncontiguous-p))))
  (let ((error-file
         (if error-buffer
             (make-temp-file
              (expand-file-name "scor"
                                (or small-temporary-file-directory
                                    temporary-file-directory)))
           nil))
        exit-status)

    ;; Handle different region types appropriately
    (cond
     ;; Non-contiguous region (e.g., rectangle)
     (region-noncontiguous-p
      (shelldon--command-on-region-noncontiguous start end command
                                                 output-buffer replace))

     ;; Replace region with output
     ((or replace
          (shelldon--output-current-buffer-p output-buffer))
      (setq exit-status
            (shelldon--command-replace-region-contiguous
             start end command replace error-file)))

     ;; Output to separate buffer
     (t
      (shelldon--command-output-to-buffer
       start end command output-buffer replace error-file)))

    ;; Process error file if it exists
    (when (and error-file (file-exists-p error-file))
      (if (< 0 (file-attribute-size (file-attributes error-file)))
          (with-current-buffer (get-buffer-create error-buffer)
            (goto-char (point-max))
            ;; Insert a separator if there's already text
            (unless (bobp)
              (insert "\f\n"))
            ;; Insert error file contents without formatting
            (format-insert-file error-file nil)
            (when display-error-buffer
              (display-buffer (current-buffer)))))
      (delete-file error-file))

    exit-status))

(defun shelldon--tramp-file-name-for-operation (orig-fun &rest args)
  (let ((operation (nth 0 args)))
    (if (equal operation 'shelldon-command)
        default-directory
      (let ((res (apply orig-fun args)))
        res))))
(advice-add 'tramp-file-name-for-operation :around #'shelldon--tramp-file-name-for-operation)
(setopt tramp-sh-file-name-handler-alist
        (append tramp-sh-file-name-handler-alist
                '((shelldon-command . tramp-handle-shell-command))))
(defun shelldon-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.
OUTPUT-BUFFER specifies where to send command output.
ERROR-BUFFER specifies where to send error output."
  ;; Check for remote directory handler
  (let* ((output-buffer (shelldon--output-buffer-name command))
         (hidden-output-buffer (concat " " output-buffer))
         (error-buffer shell-command-default-error-buffer)
         (shell-command-switch "-c") ;; synchronous commands can’t be run
         ;; interactively
         (handler
          (find-file-name-handler (directory-file-name default-directory)
                                  'shell-command)))

    (if handler
        ;; Use the appropriate handler for remote files
        (funcall handler 'shelldon-command command output-buffer error-buffer)
      ;; (funcall handler 'shelldon-command command hidden-output-buffer error-buffer)

      ;; Handle based on output destination
      (if (shelldon--output-current-buffer-p output-buffer)
          (shelldon--output-current-buffer command output-buffer error-buffer)
        (shelldon-command-on-region (point) (point) command
                                    output-buffer nil error-buffer)))

    (with-current-buffer output-buffer (rename-buffer hidden-output-buffer))
    ;; Track this command in history
    (shelldon--track-buffer command hidden-output-buffer)))
;; (advice-add 'shell-command :override #'shelldon-command)

(defun shelldon-async-command (command)
  "Execute string COMMAND in inferior shell asynchronously.
Displays output in a separate buffer with process monitoring."
  (interactive
   (list (shelldon--get-command)))

  ;; Setup output buffers
  (let* ((output-buffer (shelldon--output-buffer-name command))
         (hidden-output-buffer (concat " " output-buffer))
         (error-buffer shell-command-default-error-buffer)
         (handler
          (find-file-name-handler (directory-file-name default-directory)
                                  'async-shell-command)))

    ;; Track this command in history
    (shelldon--track-buffer command hidden-output-buffer)

    (if handler
        ;; Use the appropriate handler for remote files
        (funcall handler 'shelldon-async-command command output-buffer error-buffer)

      ;; Standard case - output to separate buffer
      (save-match-data
        (let* ((buffer (get-buffer-create output-buffer))
               (proc (get-buffer-process buffer)))
          (with-current-buffer buffer
            ;; Prepare buffer for command output
            (shell-command-save-pos-or-erase)

            ;; Setup environment with appropriate terminal settings
            (let* ((process-environment
                    (nconc
                     (list
                      (format "TERM=%s" (if shelldon-ansi-colors "eterm-color" "dumb"))
                      (format "TERMINFO=%s" data-directory)
                      (format "INSIDE_EMACS=%s" emacs-version))
                     process-environment)))

              ;; Start the process
              (setq proc
                    (start-process-shell-command "Shell" buffer command)))

            ;; Setup process display
            (setq mode-line-process '(":%s"))
            (shelldon-mode)

            ;; Configure process monitoring
            (set-process-sentinel proc #'shelldon-command-sentinel)
            (set-process-filter proc #'comint-output-filter)

            ;; Handle buffer display
            (if async-shell-command-display-buffer
                ;; Display buffer immediately
                (display-buffer buffer '(nil (allow-no-window . t)))
              ;; Defer display until first output
              (let ((nonce (make-symbol "nonce")))
                (add-function :before (process-filter proc)
                              (lambda (proc _string)
                                (let ((buf (process-buffer proc)))
                                  (when (buffer-live-p buf)
                                    (remove-function (process-filter proc)
                                                     nonce)
                                    (display-buffer buf))))
                              `((name . ,nonce)))))

            ;; Hide buffer initially with space prefix
            (rename-buffer hidden-output-buffer))))))
  nil)

;;; ============================================================================
;;; User Interface Commands
;;; ============================================================================

(define-derived-mode shelldon-mode shell-mode "Shelldon"
  "Mode for displaying shelldon output."
  (view-mode +1))

(defun shelldon--execute-with-directory-catch (command-function &rest args)
  "Execute COMMAND-FUNCTION with ARGS, handling directory changes.
Catches directory change requests and re-executes in the new context."
  (let ((rtn t))
    (while rtn
      (setq rtn (catch 'shelldon-cwd
                  (apply command-function args)))
      (when rtn
        (setq default-directory rtn)
        (setq list-buffers-directory rtn)))))

;;;###autoload
(defun shelldon (command &optional output-buffer error-buffer)
  "Execute COMMAND synchronously with output history tracking.
Sends output to OUTPUT-BUFFER and errors to ERROR-BUFFER.
Handles directory changes during command execution."
  (interactive
   (list
    (shelldon--get-command)
    current-prefix-arg
    shell-command-default-error-buffer))

  (shelldon--execute-with-directory-catch
   #'shelldon-command command
   (if output-buffer output-buffer nil)
   (if error-buffer error-buffer nil)))

;;;###autoload
(defun shelldon-kill-output (command &optional output-buffer error-buffer)
  "Execute COMMAND and copy output to kill ring.
Also sends output to OUTPUT-BUFFER and errors to ERROR-BUFFER."
  (interactive
   (list
    (shelldon--get-command)
    current-prefix-arg
    shell-command-default-error-buffer))

  ;; Execute the command
  (shelldon-command command output-buffer error-buffer)

  ;; Copy output to kill ring
  ;; (kill-new (with-current-buffer (string-trim (cdr (car shelldon--hist)))
  ;;             (buffer-string)))
  (kill-new (with-current-buffer (cdr (car shelldon--hist))
              (buffer-string))))

;;;###autoload
(defun shelldon-async ()
  "Execute command asynchronously with output history tracking.
Handles directory changes during command execution."
  (interactive)
  (shelldon--execute-with-directory-catch
   (lambda () (call-interactively #'shelldon-async-command))))

;;;###autoload
(defun shelldon-loop ()
  "Loops the shelldon command to more closely emulate a terminal."
  (interactive)
  (cl-loop (call-interactively #'shelldon-async)))

;;;###autoload
(defun shelldon-output-history ()
  "Displays the output of the selected command from the shelldon history."
  (interactive)
  (let* ((command-key (completing-read shelldon-prompt-str shelldon--hist))
         (buffer-name (cdr (assoc command-key shelldon--hist))))
    (pop-to-buffer buffer-name)))

;; Backwards compatibility
(defalias 'shelldon--hist #'shelldon-output-history
  "shelldon--hist is deprecated, use shelldon-output-history")

;;;###autoload
(defun shelldon-send-line-at-point ()
  "Send the current line to shelldon and display the result."
  (interactive)
  (let ((cmd (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position))))
    (shelldon-async-command cmd)))

;;;###autoload
(defun shelldon-send-region (start end)
  "Send region from START to END to shelldon and display the result."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (let ((cmd (buffer-substring-no-properties start end)))
    (shelldon-async-command cmd)))

;;; ============================================================================
;;; Buffer Display Configuration
;;; ============================================================================

;; Configure how shelldon buffers are displayed
(add-to-list 'display-buffer-alist
             `("*\\(shelldon.*\\)"
               (display-buffer-reuse-window
                display-buffer-in-previous-window
                display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (reusable-frames . visible)))

(provide 'shelldon)
;;; shelldon.el ends here
