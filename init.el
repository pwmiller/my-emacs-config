;; init.el
;;
;; TODO:
;;
;; Make uniqify, and color-theme work again.
;;

;; Save backup and autosaves to ~/.emacs.d/.tmp

(setq temporary-file-directory "~/.emacs.d/tmp")

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `((".*" . ,temporary-file-directory))    ; don't litter my fs tree
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t)) ; ditto
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   inhibit-startup-screen t
   initial-scratch-message nil)

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; Lazily install add-on packages.

(leaf
  flymake-jslint
  vterm
  cl-lib
  ws-butler
  :ensure t)

;; General utility functions

(defun symbol-components (symbol)
  `(
     ("symbol-name"     . ,(symbol-name symbol))
     ("symbol-value"    . ,(ignore-errors (symbol-value symbol)))
     ("symbol-function" . ,(ignore-errors (symbol-function symbol)))
     ("symbol-plist"    . ,(ignore-errors (symbol-plist symbol)))
  ))

(defun lisp ()
  (interactive)
  (switch-to-buffer "*ielm*"))

(setq init-el-path
  (concat user-emacs-directory "init.el"))

(defun reload-init-el ()
  (interactive)
  (load init-el-path))

(defun edit-init-el ()
  (interactive)
  (find-file init-el-path))

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

;; Configure ws-butler to trim witespace at EOL on save in programming modes
;; derived from prog-mode

(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Configure ws-butler to ignore multiline strings

(setq ws-butler-trim-predicate
      (lambda (beg end)
        (not (eq 'font-lock-string-face
                 (get-text-property end 'face)))))

;; Configure Espresso for JS editing
;; Emacs 22 comes with css-mode.el and espresso.el - if you don't have it, then
;; you have to copy the respective .el files from the ~/.emacs.d dir

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))

;; Configure JavaScript Flymake

(add-hook 'js2-mode-hook 'flymake-mode)

;; Modify the way emacs displays buffer names with the same filename
;; but different paths
;;(leaf uniquify
;;      :ensure t)

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
 '(flymake-error ((((class color)) (:background "Red" :bold :foreground "Yellow"))))
 '(flymake-warning ((((class color)) (:background "DarkBlue")))))
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

;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-hober)

;; Don't highlight the current line.  It's annoying!

(global-hl-line-mode 0)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(leaf-keywords hydra flymake-jslint el-get color-theme blackout))
 '(safe-local-variable-values
   '((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-style face tabs trailing lines-tail))))

;; Configure autocomplete for ielm

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)
