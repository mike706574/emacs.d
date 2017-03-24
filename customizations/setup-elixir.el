(require 'alchemist)

(add-hook 'elixir-mode-hook 'alchemist-mode)

(defun alchemist-switch-to-iex-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*Alchemist-IEx*")))

(defun alchemist-switch-from-iex-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun alchemist-mode-cider-keys ()
  "My keys for `alchemist-mode' mode."
  (interactive)
  (local-set-key (kbd "C-c C-c") #'print-message)
  (local-set-key (kbd "C-c M-j") #'alchemist-iex-project-run)
  (local-set-key (kbd "C-x e") #'alchemist-iex-send-current-line-and-go)
  (local-set-key (kbd "C-x C-e") #'alchemist-iex-send-current-line-and-go)
  (local-set-key (kbd "C-c C-k") #'alchemist-iex-reload-module)
  (local-set-key (kbd "C-c C-z") #'alchemist-switch-to-iex-buffer))

(add-hook 'alchemist-mode-hook 'alchemist-mode-cider-keys)

(defun alchemist-iex-mode-cider-keys ()
  "My keys for `alchemist-iex-mode'."
  (interactive)
  (local-set-key (kbd "C-c C-z") #'alchemist-switch-from-iex-buffer))

(add-hook 'alchemist-iex-mode-hook 'alchemist-iex-mode-cider-keys)
