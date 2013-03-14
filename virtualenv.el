(require 'python)

(defvar virtualenv-mode-name-format " [venv:%s]")

(defvar virtualenv-mode-name "Virtualenv")

(defun virtualenv-set-mode-name (name)
  (let ((formatted-name (format virtualenv-mode-name-format name)))
    (make-local-variable 'virtualenv-mode-name)
    (setq virtualenv-mode-name formatted-name)))

(define-minor-mode virtualenv-minor-mode
  "minor mode for showing virtualenv name in mode line"
  :init-value nil
  :lighter virtualenv-mode-name)


(defun virtualenv-verify-p (env-path)
  (and (file-exists-p env-path)
       (file-exists-p (concat (file-name-directory env-path)
                              "bin/activate_this.py"))))


(defun virtualenv-activate (venv)
  (interactive "DVirtualenv Directory: ")
  (let ((deactivate-choice))
    (when virtualenv-minor-mode
        (setq deactivate-choice
              (y-or-n-p (format "Virtualenv %s already active. Deactivate it?"
                                virtualenv-mode-name))))
    (cond
     ((and virtualenv-minor-mode (not deactivate-choice)) nil)
     ((and virtualenv-minor-mode deactivate-choice)
      (virtualenv-deactivate)
      (virtualenv--activate-internal venv)
      t)
     (t (virtualenv--activate-internal venv) t))))

(defun virtualenv--activate-internal (venv)
  (unless (virtualenv-verify-p venv)
    (error "Not a virtualenv directory: %s" venv))
  (let* ((bin-path (expand-file-name (concat (file-name-directory venv) "bin")))
         (old-path (getenv "PATH"))
         (new-path (concat bin-path path-separator old-path)))
    (virtualenv-set-mode-name 
     (file-name-nondirectory (directory-file-name venv)))
    (make-local-variable 'virtualenv--new-path)
    (setq virtualenv--new-path new-path)
    (make-local-variable 'virtualenv--venv)
    (setq virtualenv--venv venv)
    (virtualenv-minor-mode t)))


(defun virtualenv-deactivate ()
  (interactive)
  (if (not virtualenv-minor-mode)
      (error "A virtualenv is in not active in this buffer")
    (kill-local-variable 'virtualenv--new-path)
    (kill-local-variable 'virtualenv--venv)
    (virtualenv-minor-mode 0)))


(defun virtualenv-run-python ()
  (interactive)
  (if (not virtualenv-minor-mode)
      (if (call-interactively 'virtualenv-activate)
          (let ((process-environment 
               (cons (concat "VIRTUAL_ENV=" virtualenv--venv)
                     (cons (concat "PATH=" virtualenv--new-path)
                           process-environment))))
            (run-python))
        (message "Virtualenv not activated"))
    (let ((process-environment 
               (cons (concat "VIRTUAL_ENV=" virtualenv--venv)
                     (cons (concat "PATH=" virtualenv--new-path)
                           process-environment))))
      (run-python))))


;; (defun virtualenv-run-python (venv)
;;   (interactive "DVirtualenv Directory: ")
;;   (unless (virtualenv-verify-p venv)
;;     (error "Not a virtualenv directory: %s" venv))
;;   (let* ((activate-this-py (expand-file-name
;;                             (concat (file-name-directory venv)
;;                                     "bin/activate_this.py")))
;;          (execfile-cmd (format "execfile('%s', dict(__file__ = '%s'))"
;;                                activate-this-py
;;                                activate-this-py)))
;;     (run-python)
;;     (python-send-string execfile-cmd)
;;     (virtualenv-set-mode-name
;;      (car (last (split-string (directory-file-name venv) "/"))))
;;     (virtualenv-minor-mode t)))


(provide 'virtualenv)
