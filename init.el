;; init.el

;; Disable all byte compile warnings if ignore-byte-compile-warnings
;; is set

(setq ignore-byte-compile-warnings t)

(when (not ignore-byte-compile-warnings)
  (setq byte-compile-warnings
        '(not nresolved
              free-vars
              callargs
              redefine
              obsolete
              noruntime
              cl-functions
              interactive-only
              )))

;; Toggle this on for debugging purposes

(setq debug-on-error t)

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

;; Define list of packages to install

(leaf ac-slime :ensure t)
(leaf better-defaults :ensure t)
(leaf cl-lib :ensure t)
(leaf elpy :ensure t)
(leaf flymake-jslint :ensure t)
(leaf company-lean :ensure t)
(leaf pyenv-mode :ensure t)
(leaf vterm :ensure t)
(leaf ws-butler :ensure t)

;; Consider all custom themes "safe," and load them from
;; ~/.emacs.d/themes

(setq custom-safe-themes t) 
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Set color theme
;;
;; Behavior here is still a little wonky, but good enough for now.

(load-theme 'hober t t)

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
  (get-buffer-create "elisp REPL")
  (switch-to-buffer "elisp REPL")
  (ielm))

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

;; Configure ws-butler to ignore multiline strings

(setq ws-butler-trim-predicate
      (lambda (beg end)
        (not (eq 'font-lock-string-face
                 (get-text-property end 'face)))))

;; Configure Espresso for JS editing
;; Emacs 22 comes with css-mode.el and espresso.el - if you don't have it, then
;; you have to copy the respective .el files from the ~/.emacs.d dir

(autoload 'css-mode "css-mode")
(append auto-mode-alist
        `(
          ("\\.js$" . js-mode)
          ("\\.json\\'" . js-mode)
          ))

;; Configure JavaScript Flymake

(add-hook 'js2-mode-hook 'flymake-mode)

;; We need the indent-tabs-mode to be set to nil to convert tabs
;; into spaces, which is needed to pass JSLint.

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set the Flymake highlight colors -- the default ones are impossible to read.

(custom-set-faces
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

(setq linum-format "%5d | ")

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

;; Don't highlight the current line.  It's annoying!

(global-hl-line-mode 0)

(elpy-enable)

(defun ipython ()
  (interactive)
  (elpy-shell-switch-to-shell)
  (delete-other-windows))

;; You must install pyenv or this will error

(pyenv-mode)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Enable autocomplete using company

(add-hook 'after-init-hook #'global-company-mode)

;; Save custom variables in a separate file, so we don't clutter up init.el

(setq custom-file "~/.emacs.d/emacs-custom-variables.el")
(load custom-file)

;; Prompt to safe any unsaved customizations on exit.

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)
