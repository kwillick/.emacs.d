;;
;; virtualenv.el - Use a virtualenv for python shell
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
