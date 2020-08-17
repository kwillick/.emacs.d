;; Add the melpa package archives
(require 'package)

(defvar archive-gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar archive-melpa '("melpa" . "https://melpa.org/packages/"))
(defvar archive-melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives archive-melpa t)
(add-to-list 'package-archives archive-melpa-stable t)
(package-initialize)

;; Prefer packages from melpa-stable, then melpa, then gnu
(setq package-archive-priorities
      '(("melpa-stable" . 2)
	    ("melpa" . 1)
	    ("gnu" . 0)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; Setup builtin things

(setq inhibit-splash-screen t)

;; Setup Emacs PATH
(defun my-set-path-from-shell ()
  (let ((path-from-shell
         (shell-command-to-string "EMACS=1 $SHELL -l -c 'echo -n $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator))))
  
;; (my-set-path-from-shell)

;; Setup load-path with "lisp" directory
;; (add-to-list 'load-path (concat (expand-file-name user-emacs-directory) "lisp"))

;; Setup ido-mode
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer t)

(global-set-key (kbd "C-x C-d") 'ido-dired)

;; MacOS specific
(when (eq system-type 'darwin)
  ;; Change to ~/
  (cd "~/"))

;; Windows specific
(when (eq system-type 'windows-nt)
  (setq w32-apps-modifier 'super)
  (global-set-key (kbd "s-l") 'goto-line)

  (cd "c:/Users/kipp/"))

(defun my-frame-title-format ()
  "Format title to show file path or buffer name"
  (let ((buf-name (buffer-file-name)))
    (if buf-name
        (abbreviate-file-name buf-name)
      "%b")))

(setq frame-title-format 
      '("" (:eval (my-frame-title-format))))

;; Stuff for a smaller less cluttered mode line
(defvar clean-mode-line-clean-alist
  '((abbrev-mode . "")
    (auto-revert-mode . "")
    (magit-auto-revert-mode . "")
    (company-mode . " comp")
    ;; major modes
    (python-mode . "py")
    (emacs-lisp-mode . "el")
    (js-mode . "js"))
  "List of (mode . rename) pairs. The mode can be minor or major.")

(defun clean-mode-line ()
  "Clean up the mode-line. 
For each (mode . name) in `clean-mode-line-clean-alist', 
if mode is active rename it to name."
  (interactive)
  (dolist (mode-pair clean-mode-line-clean-alist)
    (let* ((key (car mode-pair))
           (value (cdr mode-pair))
           (cur-mode-str (cdr (assq key minor-mode-alist))))
      (when cur-mode-str
        (setcar cur-mode-str value))
      (when (eq key major-mode)
        (setq mode-name value)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Add objective-c++ file detection
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(setq confirm-nonexistent-file-or-buffer nil)

(show-paren-mode 1)
;;(which-function-mode 1)

;; Open init file
(defun goto-init-file ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))

;; Nicer window movement
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

;; Get rid of annoying insert key
(global-set-key (kbd "<insert>") nil)

;; Disable upcase-region warning
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Better indent function
(defun indent-region-or-buffer ()
  "Indent a region, if none selected indent the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; Line movement functions
(defun my-move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my-move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
  
(global-set-key (kbd "M-S-<up>") 'my-move-line-up)
(global-set-key (kbd "M-S-<down>") 'my-move-line-down)

(defun my-backward-kill-line ()
  "Kill text from point to beginning-of-line"
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (kill-region (point) pos)))

(global-set-key (kbd "M-k") 'my-backward-kill-line)

(defun my-emacs-quit ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit?")
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'my-emacs-quit)

;; byte-compile the current buffer
(defun byte-compile-this-file ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

;; Disable erase-buffer
(put 'erase-buffer 'disabled nil)

;;; Setup external packages

;; Add dash package here since I use it in my-ask-first-advice
(use-package dash
  :ensure t
  :pin "melpa-stable"
  :demand)

(defmacro my-ask-first-advice (message &rest funs)
  "Add a y-or-n-p prompt of MESSAGE to all functions in funs"
  (let ((ask `(lambda (&rest args)
                (y-or-n-p ,message))))
    (cons 'progn
          (-map (lambda (fn)
                  `(advice-add ,fn :before-while ,ask '((name . "my-ask-first-advice"))))
                funs))))

;; Theme
(use-package solarized-theme
  :ensure t
  :pin "melpa-stable"
  :demand
  :config (load-theme 'solarized-dark t))

;; Programming language modes
(use-package lsp-mode
  :ensure t
  :pin "melpa-stable"
  :after (spinner lsp-ui)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t
  :pin "melpa-stable")

(use-package d-mode
  :ensure t
  :pin "melpa-stable")

(use-package glsl-mode
  :ensure t
  :pin "melpa"
  :mode ("\\.vsh\\'" "\\.fsh\\'" "\\.gsh\\'"))

(defun my-gofmt-before-save (&optional arg)
  (interactive "p")
  (gofmt)
  (save-buffer arg))

(use-package go-mode
  :ensure t
  :pin "melpa-stable"
  :bind (:map go-mode-map
              (("C-x C-s" . my-gofmt-before-save))))

(use-package markdown-mode
  :ensure t
  :pin "melpa-stable"
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	     ("\\.md\\'" . markdown-mode)
	     ("\\.markdown\\'" . markdown-mode)))

(defun my-rust-mode-hook ()
  ;; Turn on flycheck-rust
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  ;; Turn on company-mode
  (company-mode 1)

  ;; Key binding to auto complete and indent
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common)

  ;; Enable cargo
  (cargo-minor-mode 1)

  ;; Enable backtraces on panics
  (setenv "RUST_BACKTRACE" "1")

  (electric-pair-mode 1)

  ;; Enable lsp
  (lsp-deferred))

(use-package rust-mode
  :ensure t
  :pin "melpa-stable"
  :hook (rust-mode . my-rust-mode-hook))

(use-package cargo
  :ensure t
  :pin "melpa-stable"
  :commands cargo-minor-mode)

(use-package web-mode
  :ensure t
  :pin "melpa-stable"
  :mode (("\\.js\\'" . web-mode)
	     ("\\.jsx\\'" . web-mode)))

;; (require 'prettier-js)
;; (setq prettier-target-mode "web-mode")
;; (setq prettier-width-mode nil)
;; (setq prettier-args '("--bracket-spacing" "--trailing-comma" "--single-quote"))

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (rainbow-delimiters-mode)
;;             (add-hook 'before-save-hook 'prettier-before-save)))

;; Misc packages
(use-package company
  :ensure t
  :pin "melpa-stable"
  :commands company-mode
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package expand-region
  :ensure t
  :pin "melpa"
  :demand
  :bind ("C-," . er/expand-region))

(use-package flycheck
  :ensure t
  :pin "melpa-stable")

(use-package flycheck-rust
  :ensure t
  :pin "melpa-stable")

(use-package rainbow-delimiters
  :ensure t
  :pin "melpa-stable")

(use-package smex
  :ensure t
  :pin "melpa-stable"
  :demand
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package spinner
  :ensure t
  :pin "gnu")

;; Magit
;; Magit uses this for magit-completing-read-function
(use-package ido-completing-read+
  :ensure t
  :pin "melpa-stable")

(defun my-magit-post-display-buffer-hook ()
  "After displaying magit, make it the only window showing"
  (when (eq major-mode 'magit-status-mode)
    (delete-other-windows)))

(defun my-git-rebase-mode-hook ()
  "Disable read-only-mode in git-rebase-mode."
  (read-only-mode -1))

(use-package magit
  :ensure t
  :pin "melpa-stable"
  :hook ((magit-post-display-buffer . my-magit-post-display-buffer-hook)
	     (git-rebase-mode . my-git-rebase-mode-hook))

  ;; Add y or n prompt to push related functions
  :config (my-ask-first-advice "Are you sure you want to push? "
			                   'magit-push-current
			                   'magit-push-elsewhere
			                   'magit-push-implicitly
			                   'magit-push-quickly))

(use-package rainbow-delimiters
  :ensure t
  :pin "melpa-stable"
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-backslash-max-column 90)
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "bsd")
     (c++-mode . "bsd")
     (java-mode . "bsd")
     (awk-mode . "awk")
     (other . "bsd")))
 '(c-echo-syntactic-information-p t)
 '(c-electric-pound-behavior '(alignleft))
 '(c-hanging-braces-alist
   '((defun-close)
     (class-close)
     (inline-close)
     (block-close)
     (statement-cont)
     (substatement-open after)
     (brace-list-open)
     (brace-list-close)
     (brace-entry-open)
     (extern-lang-open after)
     (namespace-open after)
     (namespace-close)
     (module-open after)
     (module-close)
     (composition-open after)
     (composition-close)
     (inexpr-class-open after)
     (inexpr-class-close before)
     (arglist-cont-nonempty)))
 '(c-offsets-alist
   '((access-label . /)
     (arglist-close . 0)
     (inextern-lang . 0)
     (innamespace . 0)))
 '(c-tab-always-indent nil)
 '(c-toggle-auto-hungry-state nil)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(default-frame-alist '((vertical-scroll-bars) (width . 100) (height . 62)))
 '(grep-command "egrep -nH -e ")
 '(grep-find-command '("find . -type f -exec egrep -nH -e \"\" {} +" . 36))
 '(ido-ignore-files
   '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.DS_Store"))
 '(indent-tabs-mode nil)
 '(inverse-video t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(magit-completing-read-function 'magit-ido-completing-read)
 '(magit-use-overlays nil)
 '(ns-alternate-modifier 'meta)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'super)
 '(package-selected-packages
   '(lsp-ui magit ido-completing-read+ smex rainbow-delimiters flycheck-rust flycheck expand-region company web-mode cargo rust-mode markdown-mode go-mode glsl-mode d-mode solarized-theme dash use-package))
 '(python-indent-guess-indent-offset nil)
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tab-width 4)
 '(tool-bar-mode nil))

;; put the autosave and backup files in my emacs directory
;; (defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
;; (defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
;; (setq backup-directory-alist (list (cons ".*" backup-dir)))
;; (setq auto-save-list-file-prefix autosave-dir)
;; (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;; Remove .DS_Store files from dired
(require 'dired-x)
(setq dired-omit-files "\\`\\.DS_Store")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 102 :width medium :foundry "outline" :family "Consolas"))))
 '(magit-blame-date ((t (:foreground "#F2804F" :background nil))))
 '(magit-blame-heading ((t (:foreground "#073642" :background nil))))
 '(magit-blame-name ((t (:foreground "#F2804F" :background nil))))
 '(magit-blame-summary ((t (:foreground "#d33682" :background nil :weight bold)))))
