
(require 'cl-lib)

;; Setup Emacs PATH
(defun my-set-path-from-shell ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -l -c 'echo -n $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator))))
  
(my-set-path-from-shell)

;; Setup load-path
(add-to-list 'load-path (expand-file-name user-emacs-directory))

;; Add marmalade package archive
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(defvar my-packages
  '(magit haskell-mode solarized-theme yasnippet markdown-mode expand-region)
  "A list of the packages I want to ensure are installed")

;; Based off of preludes prelude-install-packages
(defun my-ensure-packages-installed ()
  (unless (cl-every 'package-installed-p my-packages)
    (message "%s" "Some packages are missing. Refreshing package database...")
    (package-refresh-contents)
    (message "%s" "Done refreshing package database")
    ;;install them
    (dolist (pack my-packages)
      (unless (package-installed-p pack)
        (package-install pack)))))
  
(my-ensure-packages-installed)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "bsd") (awk-mode . "awk") (other . "bsd"))))
 '(c-echo-syntactic-information-p t)
 '(c-electric-pound-behavior (quote (alignleft)))
 '(c-hanging-braces-alist (quote ((defun-close) (class-close) (inline-close) (block-close) (statement-cont) (substatement-open after) (brace-list-open) (brace-list-close) (brace-entry-open) (extern-lang-open after) (namespace-open after) (namespace-close) (module-open after) (module-close) (composition-open after) (composition-close) (inexpr-class-open after) (inexpr-class-close before) (arglist-cont-nonempty))))
 '(c-offsets-alist (quote ((access-label . /) (arglist-close . 0) (inextern-lang . 0) (innamespace . 0))))
 '(c-tab-always-indent nil)
 '(c-toggle-auto-hungry-state nil)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(custom-safe-themes (quote ("501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars) (width . 100) (height . 66))))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-doc-mode)))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.DS_Store")))
 '(indent-tabs-mode nil)
 '(inverse-video t)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote super))
 '(python-guess-indent nil)
 '(python-indent-guess-indent-offset nil)
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tab-width 4)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco")))))


;; Load theme
(require 'solarized-theme)
(load-theme 'solarized-dark t)


;; Format title to show file path or buffer name
(defun my-frame-title-format ()
  (let ((buf-name (buffer-file-name)))
    (if buf-name
        (abbreviate-file-name buf-name)
      "%b")))

(setq frame-title-format 
      '("" (:eval (my-frame-title-format))))

(show-paren-mode 1)


;; Setup python stuff
(defun my-python-clear-buffer ()
  "Clear the python buffer, if it is running"
  (interactive)
  (when python-buffer
    (with-current-buffer python-buffer
      (erase-buffer)
      (python-send-string "\n"))))
    
;; Modify some keys in python mode
(add-hook 'python-mode-hook
          '(lambda ()
             (setq  python-remove-cwd-from-path nil)
             ;;(define-key python-mode-map "\C-c\C-v" 'virtualenv-run-python)
             (define-key python-mode-map "\C-c\C-k" 'my-python-clear-buffer)))

(require 'virtualenv)


;; better window movement
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)


;; put the autosave and backup files in my emacs directory
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;; Remove .DS_Store files from dired
(require 'dired-x)
(setq dired-omit-files "\\`\\.DS_Store")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Setup ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)

;; yasnippet setup
(require 'yasnippet)
(yas-global-mode)
(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

;; Setup markdown mode
(setq auto-mode-alist (cons '("\\.md\\'" . markdown-mode) auto-mode-alist))

;; Setup expand-region
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)

;; Run Emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Disable upcase-region warning
(put 'upcase-region 'disabled nil)

;; Enable Rust
(setq auto-mode-alist (cons '("\\.rs\\'" . rust-mode) auto-mode-alist))

