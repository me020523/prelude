;;; prelude-go.el --- Emacs Prelude: Dired mode support.

;;; Code;

(prelude-require-packages '(all-the-icons-dired dired-hide-dotfiles))

(add-hook 'dired-mode-hook (lambda ()
                             (all-the-icons-dired-mode)
                             (dired-hide-dotfiles-mode)))
(provide 'prelude-dired)
