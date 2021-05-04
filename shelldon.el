(defvar shelldon-hist '())
(defun shelldon (command &optional output-buffer error-buffer)
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message ">> `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          ">> ")
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
  (setq output-buffer (concat " *" (number-to-string shelldon-nth) ":" command "*"))
  (add-to-list 'shelldon-hist `(,(concat (number-to-string (length shelldon-hist)) ":" command) .
				,output-buffer))
  (async-shell-command command output-buffer error-buffer)
  (with-current-buffer output-buffer (buffer-string)))

(defun shelldon-loop ()
  (interactive)
  (call-interactively 'shelldon)
  (call-interactively 'shelldon-loop))

(defun shelldon-hist ()
  (interactive)
  (switch-to-buffer (cdr (assoc (completing-read ">> " shelldon-hist) shelldon-hist))))

(provide 'shelldon)
