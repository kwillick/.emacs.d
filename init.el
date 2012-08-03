;;
;; init.el - My .emacs files
;;
;; Copyright (c) 2012, Kipp Hickman
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(add-to-list 'exec-path "/usr/local/bin/")

(add-to-list 'load-path (expand-file-name user-emacs-directory))


(require 'cl)

;; Add melpa package archive
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(defvar my-packages
  '(magit haskell-mode solarized-theme yasnippet)
  "A list of the packages I want to ensure are installed")

;; Based off of preludes prelude-install-packages
(defun my-ensure-packages-installed ()
  (unless (every 'package-installed-p my-packages))
    (message "%s" "Some packages are missing. Refreshing package database...")
    (package-refresh-contents)
    (message "%s" "Done refreshing package database")
    ;;install them
    (dolist (pack my-packages)
      (unless (package-installed-p pack)
        (package-install pack))))
  
(my-ensure-packages-installed)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "bsd") (awk-mode . "awk") (other . "bsd"))))
 '(c-echo-syntactic-information-p t)
 '(c-electric-pound-behavior nil)
 '(c-hanging-braces-alist (quote ((defun-close) (class-close) (inline-close) (block-close) (statement-cont) (substatement-open after) (brace-list-open) (brace-list-close) (brace-entry-open) (extern-lang-open after) (namespace-open after) (namespace-close) (module-open after) (module-close) (composition-open after) (composition-close) (inexpr-class-open after) (inexpr-class-close before) (arglist-cont-nonempty))))
 '(c-offsets-alist (quote ((access-label . /) (arglist-close . 0) (inextern-lang . 0) (innamespace . 0))))
 '(c-tab-always-indent nil)
 '(c-toggle-auto-hungry-state nil)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(custom-safe-themes (quote ("501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-doc-mode)))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.DS_Store")))
 '(indent-tabs-mode nil)
 '(inverse-video t)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote meta))
 '(ns-right-alternate-modifier (quote super))
 '(python-guess-indent nil)
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))


(require 'setup-appearance)


;; Setup python stuff
(require 'virtualenv)

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
             (define-key python-mode-map "\C-c\C-v" 'virtualenv-run-python)
             (define-key python-mode-map "\C-c\C-k" 'my-python-clear-buffer)))


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
(yas/initialize)

;; Found at http://blog.iany.me/2012/03/use-popup-isearch-for-yasnippet-prompt/
;; add some shotcuts in popup menu mode
;; (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
;; (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

;; (defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
;;   (when (featurep 'popup)
;;     (popup-menu*
;;      (mapcar
;;       (lambda (choice)
;;         (popup-make-item
;;          (or (and display-fn (funcall display-fn choice))
;;              choice)
;;          :value choice))
;;       choices)
;;      :prompt prompt
;;      ;; start isearch mode immediately
;;      :isearch t
;;      )))

(setq yas/prompt-functions '(yas/ido-prompt yas/no-prompt))

;; Make C-c c do comment-region
(define-key global-map (kbd "C-c c") 'comment-region)


;; Run Emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

