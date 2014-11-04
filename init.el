
;; Set up the package manager.
;; Use elpa, melpa, and marmalade
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )

;; Set the path for the emacs process so things like running .bashrc
;; when opening a terminal work
(setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))

;; Show the menu bar
(menu-bar-mode t)

;; Line numbers on the left
(global-linum-mode t)

;; Show column number in status bar
(column-number-mode)

;; Put a little space after the line numbers so as not to crowd the
;; text.
(setq linum-format "%d  ")

;; Set the mode to python-mode when the filename ends in ".py"
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Save session and restore files when editor closes, if we are using
;; a GUI.  (i.e. don't do it when invoking on the command line)

(when (window-system) (desktop-save-mode t))
(setq desktop-dirname         "~/.emacs.d/desktop"
      desktop-base-file-name  "emacs.desktop"
      desktop-base-lock-name  "lock"
      desktop-path            (list desktop-dirname)
      desktop-save            t
      ;desktop-files-not-to-save nil
      desktop-load-locked-desktop nil)

;; Set a proper color theme.

(require 'color-theme)
(color-theme-initialize)
(color-theme-hober)

;; Don't highlight the current line.  It's annoying!

(global-hl-line-mode 0)
