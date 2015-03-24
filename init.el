;; Save backup and autosaves to ~/.emacs.d/.tmp

(setq temporary-file-directory "~/.emacs.d/tmp")

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `((".*" . ,temporary-file-directory))    ; don't litter my fs tree
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t)) ; ditto
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Set up the package manager.
;; Use elpa, melpa, and marmalade
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )


;; Set up pyflakes/PEP8 checking
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake")))
					; Load Python Flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
					;                       'flymake-create-temp-inplace))
		       'flymake-create-temp-in-system-tempdir))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pyflymake.sh" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))

;; Configure Espresso for JS editing
;; Emacs 22 comes with css-mode.el and espresso.el - if you don't have it, then you have to copy the respective
;; .el files from the ~/.emacs.d dir

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))

; Load JavaScript Flymake
; May need to package-install flymake-jslint
(require 'flymake-jslint)
(add-hook 'js2-mode-hook 'flymake-mode)

;; Modify the way emacs displays buffer names with the same filename
;; but different paths
(require 'uniquify)

;; Nuke trailing whitespace on lines I've modifed
;; may need to package-install ws-trim
(require 'ws-trim)
(global-ws-trim-mode t)
(set-default 'ws-trim-level 1) ; only modified lines are trimmed

;; We need the indent-tabs-mode to be set to nil to convert tabs
;; into spaces, which is needed to pass JSLint.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set the Flymake highlight colors -- the default ones are impossible to read.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "Red" :bold :foreground "Yellow"))))
  '(flymake-warnline ((((class color)) (:background "DarkBlue")))))
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
