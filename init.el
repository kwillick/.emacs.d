(require 'cl-lib)

(setq inhibit-splash-screen t)

;; Setup Emacs PATH
(defun my-set-path-from-shell ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -l -c 'echo -n $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator))))
  
(my-set-path-from-shell)

;; Setup load-path
;;(add-to-list 'load-path (expand-file-name user-emacs-directory))

;; Add marmalade and melpa package archives
(require 'package)
(defvar archive-marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar archive-gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar archive-melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives archive-marmalade t)
(add-to-list 'package-archives archive-melpa t)
(package-initialize)

(defun my-install-packages (&rest packages)
  (mapc (lambda (package)
          (let ((name (car package))
                (src (cdr package)))
            (unless (package-installed-p name)
              (let ((package-archives (list src)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(defun my-install-packages-perform ()
  (my-install-packages
   (cons 'cmake-mode archive-marmalade)
   (cons 'company archive-melpa)
   (cons 'd-mode archive-melpa)
   (cons 'expand-region archive-marmalade)
   (cons 'glsl-mode archive-melpa)
   (cons 'go-mode archive-melpa)
   (cons 'markdown-mode archive-marmalade)
   (cons 'rainbow-delimiters archive-melpa)
   (cons 'rust-mode archive-melpa)
   (cons 'smex archive-marmalade)
   (cons 'solarized-theme archive-melpa)
   (cons 'yasnippet archive-melpa)
   (cons 'magit archive-melpa)))

(condition-case nil
    (my-install-packages-perform)
  (error
   (package-refresh-contents)
   (my-install-packages-perform)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-backslash-max-column 90)
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (c++-mode . "bsd")
     (java-mode . "bsd")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(c-echo-syntactic-information-p t)
 '(c-electric-pound-behavior (quote (alignleft)))
 '(c-hanging-braces-alist
   (quote
    ((defun-close)
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
     (arglist-cont-nonempty))))
 '(c-offsets-alist
   (quote
    ((access-label . /)
     (arglist-close . 0)
     (inextern-lang . 0)
     (innamespace . 0))))
 '(c-tab-always-indent nil)
 '(c-toggle-auto-hungry-state nil)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (width . 100)
      (height . 62))))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-doc-mode)))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.DS_Store")))
 '(indent-tabs-mode nil)
 '(inverse-video t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient")
 '(magit-use-overlays nil)
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
 '(default ((t (:inherit nil :stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco")))))


;; Load theme
(require 'solarized)
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
(which-function-mode 1)

;; Stuff for a smaller less cluttered mode line
(defvar clean-mode-line-clean-alist
  '((yas-minor-mode . " y")
    (abbrev-mode . "")
    (auto-revert-mode . "")
    (magit-auto-revert-mode . "")
    (company-mode . " comp")
    ;; major modes
    (python-mode . "py")
    (emacs-lisp-mode . "el")
    (js-mode . "js")))

(defun clean-mode-line ()
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

;; shell stuff
(defun get-first-shell ()
  "Get first shell listed in /etc/shells"
  (with-temp-buffer
    (insert-file-contents "/etc/shells")
    (forward-line 4)
    (let ((p (point)))
      (end-of-line)
      (buffer-substring p (point)))))

(with-eval-after-load 'shell
  ;; (defadvice shell (before shell-check-remote (&optional buffer) activate)
  ;;   (setq explicit-shell-file-name (if (file-remote-p default-directory)
  ;;                                      nil
  ;;                                    (get-first-shell))))

  (add-to-list 'explicit-bash-args "--login"))

(require 'pcomplete)
(defun my-shell-mode-hook ()
  "set pcomplete-ignore-case to t. shell-dynamic-complete-functions uses pcomplete."
  (make-local-variable 'pcomplete-ignore-case)
  (setq pcomplete-ignore-case t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; Open init file
(defun goto-init-file ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))


;; Setup ido-mode
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      confirm-nonexistent-file-or-buffer nil)
(global-set-key (kbd "C-x C-d") 'ido-dired) ;; Switch list directory to dired

;; Setup smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun find-file-as-root ()
  "Call ido-find-file using tramp/sudo"
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))


;; Setup python stuff
(with-eval-after-load 'python
  (defun python-set-virtualenv-path (dir)
    "Set the python-shell-virtualenv-path to dir. If already set, set it to nil."
    (interactive (if (null python-shell-virtualenv-path)
                     (list (expand-file-name 
                            (ido-read-directory-name "virtualenv dir: " nil nil t)))
                   (list nil)))
    (setq python-shell-virtualenv-path dir))

  (defun my-python-fixup-shift-region (start end)
    "Given start and end return start moved to beginning-of-line and end moved to end-of-line"
    (when mark-active
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (beginning-of-line)
        (setq beg (point))
        (goto-char end)
        (end-of-line)
        (cons beg (point)))))
  
  (defadvice python-indent-shift-right (around python-indent-shift-right-around
                                               (start end &optional count)
                                               activate)
    (save-excursion
      (let ((new-region (my-python-fixup-shift-region start end)))
        (setq start (car new-region))
        (setq end (cdr new-region))
        ad-do-it)))
  
  (defadvice python-indent-shift-left (around python-indent-shift-left-around
                                              (start end &optional count)
                                              activate)
    (save-excursion
      (let ((new-region (my-python-fixup-shift-region start end)))
        (setq start (car new-region))
        (setq end (cdr new-region))
        ad-do-it)))
)

;; javscript stuff
;; which-function-mode does not work with js very well
(add-hook 'js-mode-hook
          (lambda ()
            (which-function-mode -1)
            (rainbow-delimiters-mode)))

;; company-mode stuff
(require 'company)

;; emacs-lisp stuff
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; go-mode stuff
(with-eval-after-load 'go-mode
  (defun my-gofmt-before-save (&optional arg)
    (interactive "p")
    (gofmt)
    (save-buffer arg))

  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-s") 'my-gofmt-before-save))))
  

;; prog-mode hook
(add-hook 'prog-mode-hook
          (lambda ()
            (imenu-add-menubar-index)))

;; magit stuff
(require 'magit)

(defun my-magit-status-buffer-switch-function (buffer)
  (pop-to-buffer buffer)
  (delete-other-windows))

(setq magit-status-buffer-switch-function 'my-magit-status-buffer-switch-function)

(defun my-magit-push-dwim (arg)
  "Slight modification of magit-push-dwim to ask for confirmation"
  (interactive "P")
  (when (y-or-n-p "Are you sure you want to push? ")
    (magit-push-dwim arg)))

(setq magit-push-hook '(my-magit-push-dwim))

;; ediff
;; hack because ediff-control-frame-parameters is weird (top and left)
(require 'ediff-wind)
(setcdr (assoc 'left ediff-control-frame-parameters) 1)
(setcdr (assoc 'top ediff-control-frame-parameters) 1)

;; Nicer window movement
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

;; yasnippet setup
(require 'yasnippet)
(yas-global-mode)
(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

;; Setup expand-region
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)

;; Run Emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

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

;; backward line killing
(defun my-backward-kill-line ()
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (kill-region (point) pos)))

(global-set-key (kbd "M-k") 'my-backward-kill-line)

;; indirect buffer + narrowing
(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (switch-to-buffer buf))))

;; byte-compile the current buffer
(defun byte-compile-this-file ()
  (interactive)
  (byte-compile-file (buffer-file-name)))


;; cmake-mode
(require 'cmake-mode)


;; Modify auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.gsh\\'" . glsl-mode))


;; Change to ~/
(cd "~/")
