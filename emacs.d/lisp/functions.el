(defun reload-init ()
  "Reload the init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (spaceline-compile))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))
