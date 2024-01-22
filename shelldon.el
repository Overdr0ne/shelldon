;;; shelldon.el --- An enhanced shell interface -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne <scmorris.dev@gmail.com>
;; Version: 1.0
;; URL: https://github.com/Overdr0ne/shelldon
;; Package-Requires: ((emacs "27.1"))
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

(defgroup shelldon nil
  "A shell command interface that keeps track of output buffers."
  :group 'convenience
  :prefix "shelldon-")

(defcustom shelldon-prompt-str ">> "
  "A string prepending the shelldon prompt, much like the PS1 EV in BASH."
  :type 'editable-field)

(defcustom shelldon-ansi-colors nil
  "Toggle ANSI color output on shelldon’s output."
  :type 'toggle)

(defun shelldon-cd ()
  "Change directories without leaving shelldon context.

Get the workdir, then throw it back for the shelldon command to set it in that
context."
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

(defvar shelldon--hist '())
(defun shelldon--get-command ()
  "Get command string from the user."
  (minibuffer-with-setup-hook
      (lambda ()
        (shell-completion-vars)
        (set (make-local-variable 'minibuffer-default-add-function)
             'minibuffer-default-add-shell-commands))
    (let ((prompt (format-message "%s%s"
                                  (abbreviate-file-name
                                   default-directory)
                                  shelldon-prompt-str))
          (initial-contents nil))
      (read-from-minibuffer prompt initial-contents
                            shelldon-minibuffer-local-command-map
                            nil
                            'shell-command-history
                            (list
                             (list
                              (let ((filename
                                     (cond
                                      (buffer-file-name)
                                      ((eq major-mode 'dired-mode)
                                       (dired-get-filename nil t)))))
                                (and filename (file-relative-name filename)))))))))
(defvar shelldon--kill-output nil)

(defun shelldon-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND in inferior shell; display output, if any.
With prefix argument, insert the COMMAND's output at point.

This is function is largely just copy-pasted from the built-in
`shell-command’, with some minor modifications.

Interactively, prompt for COMMAND in the minibuffer.
If `shell-command-prompt-show-cwd' is non-nil, show the current
directory in the prompt.

Otherwise, COMMAND is executed synchronously.  The output appears in
the buffer named by `shell-command-buffer-name'.  If the output is
short enough to display in the echo area (which is determined by the
variables `resize-mini-windows' and `max-mini-window-height'), it is
shown there, but it is nonetheless available in buffer named by
`shell-command-buffer-name' even though that buffer is not
automatically displayed.

To specify a coding system for converting non-ASCII characters
in the shell command output, use \\[universal-coding-system-argument] \
before this command.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

The optional second argument OUTPUT-BUFFER, if non-nil,
says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, erase that buffer
and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevents the buffer from being
erased.  If OUTPUT-BUFFER is not a buffer and not nil (which happens
interactively when the prefix argument is given), insert the
output in current buffer after point leaving mark after it.  This
cannot be done asynchronously.

If OUTPUT-BUFFER is a buffer or buffer name different from the
current buffer, instead of outputting at point in that buffer,
the output will be appended at the end of that buffer.

The user option `shell-command-dont-erase-buffer', which see, controls
whether the output buffer is erased and where to put point after
the shell command.

If the command terminates without error, but generates output,
and you did not specify \"insert it in the current buffer\",
the output can be displayed in the echo area or in its buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise, the buffer containing the output is displayed.
Note that if `shell-command-dont-erase-buffer' is non-nil,
the echo area could display more than just the output of the
last command.

If there is output and an error, and you did not specify \"insert it
in the current buffer\", a message about the error goes at the end
of the output.

If the optional third argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER.

In Elisp, you will often be better served by calling `call-process' or
`start-process' directly, since they offer more control and do not
impose the use of a shell (with its need to quote arguments)."

  ;; Look for a handler in case default-directory is a remote file name.
  (let* ((output-buffer (concat "*shelldon:" (number-to-string (length shelldon--hist)) ":" command "*"))
         (hidden-output-buffer (concat " " output-buffer))
         (error-buffer shell-command-default-error-buffer)
         (handler
	        (find-file-name-handler (directory-file-name default-directory)
				                          'shell-command)))
    (add-to-list 'shelldon--hist `(,(concat (number-to-string (length shelldon--hist)) ":" command) . ,hidden-output-buffer))
    (if handler
	      (funcall handler 'shell-command command output-buffer error-buffer)
      (if (and output-buffer
               (or (eq output-buffer (current-buffer))
                   (and (stringp output-buffer) (eq (get-buffer output-buffer) (current-buffer)))
	                 (not (or (bufferp output-buffer) (stringp output-buffer))))) ; Bug#39067
	        ;; Synchronous command with output in current buffer.
	        (let ((error-file
                 (and error-buffer
                      (make-temp-file
                       (expand-file-name "scor"
                                         (or small-temporary-file-directory
                                             temporary-file-directory))))))
	          (barf-if-buffer-read-only)
	          (push-mark nil t)
            (shell-command-save-pos-or-erase 'output-to-current-buffer)
	          ;; We do not use -f for csh; we will not support broken use of
	          ;; .cshrcs.  Even the BSD csh manual says to use
	          ;; "if ($?prompt) exit" before things that are not useful
	          ;; non-interactively.  Besides, if someone wants their other
	          ;; aliases for shell commands then they can still have them.
            (call-process-shell-command command nil (if error-file
                                                        (list t error-file)
                                                      t))
	          (when (and error-file (file-exists-p error-file))
              (when (< 0 (file-attribute-size (file-attributes error-file)))
                (with-current-buffer (get-buffer-create error-buffer)
                  (let ((pos-from-end (- (point-max) (point))))
                    (or (bobp)
                        (insert "\f\n"))
                    ;; Do no formatting while reading error file,
                    ;; because that can run a shell command, and we
                    ;; don't want that to cause an infinite recursion.
                    (format-insert-file error-file nil)
                    ;; Put point after the inserted errors.
                    (goto-char (- (point-max) pos-from-end)))
                  (display-buffer (current-buffer))
                  ))
	            (delete-file error-file))
	          ;; This is like exchange-point-and-mark, but doesn't
	          ;; activate the mark.  It is cleaner to avoid activation,
	          ;; even though the command loop would deactivate the mark
	          ;; because we inserted text.
	          (goto-char (prog1 (mark t)
			                   (set-marker (mark-marker) (point)
				                             (current-buffer)))))
	      ;; Otherwise, command is executed synchronously.
	      (shell-command-on-region (point) (point) command
				                         output-buffer nil error-buffer)))))

(defun shelldon-async-command (command)
  "Execute string COMMAND in inferior shell; display output, if any.
With prefix argument, insert the COMMAND's output at point.

This is just a copy-pasta of the `async-shell-command' that has all the
superfluous stuff ripped out.  It also sets up a process environment for
the command that optionally enables ANSI colors.

In Elisp, you will often be better served by calling `call-process' or
`start-process' directly, since they offer more control and do not
impose the use of a shell (with its need to quote arguments)."
  (interactive
   (list (shelldon--get-command)))
  ;; (when current-prefix-arg (setq output-buffer current-prefix-arg))
  ;; Look for a handler in case default-directory is a remote file name.
  (let* ((output-buffer (concat "*shelldon:" (number-to-string (length shelldon--hist)) ":" command "*"))
         (hidden-output-buffer (concat " " output-buffer))
         (error-buffer shell-command-default-error-buffer)
         (handler
	      (find-file-name-handler (directory-file-name default-directory)
				                  'shelldon-async-command)))
    (add-to-list 'shelldon--hist `(,(concat (number-to-string (length shelldon--hist)) ":" command) . ,hidden-output-buffer))
    (if handler
	    (funcall handler 'shelldon-async-command command output-buffer error-buffer)
      ;; Output goes in a separate buffer.
      ;; Preserve the match data in case called from a program.
      ;; FIXME: It'd be ridiculous for an Elisp function to call
      ;; shell-command and assume that it won't mess the match-data!
      (save-match-data
        (let* ((buffer (get-buffer-create output-buffer))
               (proc (get-buffer-process buffer)))
          (with-current-buffer buffer
            (shell-command-save-pos-or-erase)
	        (let* ((process-environment
                    (nconc
                     (list
                      (format "TERM=%s" (if shelldon-ansi-colors "eterm-color" "dumb"))
                      (format "TERMINFO=%s" data-directory)
                      (format "INSIDE_EMACS=%s" emacs-version))
                     process-environment)))
	          (setq proc
		            (start-process-shell-command "Shell" buffer command)))
	        (setq mode-line-process '(":%s"))
	        (shelldon-mode)
            (set-process-sentinel proc #'shell-command-sentinel)
	        ;; Use the comint filter for proper handling of
	        ;; carriage motion (see comint-inhibit-carriage-motion).
            (set-process-filter proc #'comint-output-filter)
            (if async-shell-command-display-buffer
                ;; Display buffer immediately.
                (display-buffer buffer '(nil (allow-no-window . t)))
              ;; Defer displaying buffer until first process output.
              ;; Use disposable named advice so that the buffer is
              ;; displayed at most once per process lifetime.
              (let ((nonce (make-symbol "nonce")))
                (add-function :before (process-filter proc)
                              (lambda (proc _string)
                                (let ((buf (process-buffer proc)))
                                  (when (buffer-live-p buf)
                                    (remove-function (process-filter proc)
                                                     nonce)
                                    (display-buffer buf))))
                              `((name . ,nonce)))))
            ;; FIXME: When the output buffer is hidden before the shell process is started,
            ;; ANSI colors are not displayed. I have no idea why.
            (rename-buffer hidden-output-buffer))))))
  nil)

(define-derived-mode shelldon-mode shell-mode "Shelldon"
  "Mode for displaying shelldon output."
  (view-mode +1))

;;;###autoload
(defun shelldon (command &optional output-buffer error-buffer)
  "Execute given asynchronously in the minibuffer with output history.

If the user tries to change the workdir while the command is executing, catch
the change and re-execute in the new context."
  (interactive
   (list
    (shelldon--get-command)
    current-prefix-arg
    shell-command-default-error-buffer))
  (let ((rtn t))
    (while rtn
      (setq rtn (catch 'shelldon-cwd
                  (shelldon-command command
                                    (if output-buffer output-buffer nil)
                                    (if error-buffer error-buffer nil))))
      (when rtn
        (setq default-directory rtn)
        (setq list-buffers-directory rtn)))))

(defun shelldon-kill-output (command &optional output-buffer error-buffer)
  "Execute COMMAND and copy output as kill.

Also send output to OUTPUT-BUFFER and ERROR-BUFFER."
  (interactive
   (list
    (shelldon--get-command)
    current-prefix-arg
    shell-command-default-error-buffer))
  (shelldon-command command
                    (if output-buffer output-buffer nil)
                    (if error-buffer error-buffer nil))
  (kill-new (with-current-buffer (string-trim (cdr (car shelldon--hist)))
              (buffer-string ))))

(defun shelldon-async ()
  "Execute given asynchronously in the minibuffer with output history.

If the user tries to change the workdir while the command is executing, catch
the change and re-execute in the new context."
  (interactive)
  (let ((rtn t))
    (while rtn
      (setq rtn (catch 'shelldon-cwd (call-interactively #'shelldon-async-command)))
      (when rtn
        (setq default-directory rtn)
        (setq list-buffers-directory rtn)))))

;;;###autoload
(defun shelldon-loop ()
  "Loops the shelldon command to more closely emulate a terminal."
  (interactive)
  (cl-loop (call-interactively #'shelldon-async)))

;;;###autoload
(defun shelldon-output-history ()
  "Displays the output of the selected command from the shelldon history."
  (interactive)
  (pop-to-buffer (cdr (assoc (completing-read shelldon-prompt-str shelldon--hist) shelldon--hist))))
(defalias 'shelldon--hist #'shelldon-output-history
  "shelldon--hist is deprecated, use shelldon-output-history")

(add-to-list 'display-buffer-alist
	         `("*\\(shelldon.*\\)"
	           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
	           (side . right)
	           (slot . 0)
	           (window-width . 80)
	           (reusable-frames . visible)))

;;;###autoload
(defun shelldon-send-line-at-point ()
  "Send the current line to shelldon and display the result."
  (interactive)
  (let ((cmd (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (shelldon-async-command cmd)))

;;;###autoload
(defun shelldon-send-region (start end)
  "Send region from START to END to shelldon and display the result."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (let ((cmd (buffer-substring-no-properties start end)))
    (shelldon-async-command cmd)))

(provide 'shelldon)
;;; shelldon.el ends here
