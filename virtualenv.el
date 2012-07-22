;; Use a virtualenv for python shell
(require 'python)

(defvar virtualenv-mode-name-format " [venv:%s]")

(defvar virtualenv-mode-name "Virtualenv")

(defun virtualenv-set-mode-name (name)
  (let ((formatted-name (format virtualenv-mode-name-format name)))
    (make-local-variable 'virtualenv-mode-name)
    (setq virtualenv-mode-name formatted-name)))

;;;###autoload
(define-minor-mode virtualenv-minor-mode
  "minor mode for showing virtualenv name in mode line"
  :init-value nil
  :lighter virtualenv-mode-name)


(defun virtualenv-verify-p (env-path)
  (and (file-exists-p env-path)
       (file-exists-p (concat (file-name-directory env-path)
                              "bin/activate_this.py"))))
                
  
(defun virtualenv-run-python (activate)
  (interactive "DVirtualenv Directory: ")
  (unless (virtualenv-verify-p activate)
    (error "Not a virtualenv directory: %s" activate))
  (let* ((activate-this-py (expand-file-name
                            (concat (file-name-directory activate)
                                    "bin/activate_this.py")))
         (execfile-cmd (format "execfile('%s', dict(__file__ = '%s'))"
                               activate-this-py
                               activate-this-py)))
    (run-python)
    (python-send-string execfile-cmd)
    (virtualenv-set-mode-name
     (car (last (split-string (directory-file-name activate) "/"))))
    (virtualenv-minor-mode t)))

(provide 'virtualenv)
