;;; shelldon.el --- a friendly little shell in the minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne <scmorris.dev@gmail.com>
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
;; It is basically just a simple wrapper around async-shell-command that
;; primarily allows you to store and navigate separate command outputs among
;; other things.

;;; Code:

(require 'cl)

(defun shelldon-cd ()
  "Change directories without leaving shelldon context.

Get the workdir, then throw it back for the shelldon command to set it in that
context."
  (interactive)
  (let (shelldon-wd)
    (setq shelldon-wd (call-interactively #'cd))
    (throw 'shelldon-cwd shelldon-wd)))

(define-key minibuffer-local-shell-command-map (kbd "C-x C-f") #'shelldon-cd)

(defvar shelldon-hist '())
(defvar shelldon-prompt-str ">> ")
(setq shelldon-prompt-str ">> ")
(defun shelldon-internal (command &optional output-buffer error-buffer)
  "Execute COMMAND asynchronously in the minibuffer with output history.

Keep track of each command output in a separate buffer.  Optionally send stdout
to OUTPUT-BUFFER and stderr to ERROR-BUFFER, just like the raw
’async-shell-command’."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "%s%s"
                                            (abbreviate-file-name
                                             default-directory)
                                            shelldon-prompt-str)
                          shelldon-prompt-str)
                        nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (setq output-buffer (concat " *shelldon:" (number-to-string (length shelldon-hist)) ":" command "*"))
  (add-to-list 'shelldon-hist `(,(concat (number-to-string (length shelldon-hist)) ":" command) .
                                ,output-buffer))
  (shell-command command output-buffer error-buffer)
  (with-current-buffer output-buffer (buffer-string))
  nil)
(defun shelldon ()
  "Execute given asynchronously in the minibuffer with output history.

If the user tries to change the workdir while the command is executing, catch
the change and re-execute in the new context."
  (interactive)
  (let ((rtn t))
    (while rtn
      (setq rtn (catch 'shelldon-cwd (call-interactively #'shelldon-internal)))
      (when rtn
        (setq default-directory rtn)
        (setq list-buffers-directory rtn)))))

(defun shelldon-loop ()
  "Loops the shelldon command to more closely emulate a terminal."
  (interactive)
  (loop (call-interactively #'shelldon)))

(defun shelldon-output-history ()
  "Displays the output of the selected command from the shelldon history."
  (interactive)
  (switch-to-buffer (cdr (assoc (completing-read shelldon-prompt-str shelldon-hist) shelldon-hist))))
(defalias 'shelldon-hist 'shelldon-output-history
  "shelldon-hist is deprecated, use shelldon-output-history")

(provide 'shelldon)

;;; shelldon.el ends here
