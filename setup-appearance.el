
;; Load theme
(require 'solarized-theme)
(load-theme 'solarized-dark t)


;; Format title to show file path or buffer name
(defun kipps-frame-title-format ()
  (let ((buf-name (buffer-file-name)))
    (if buf-name
        (abbreviate-file-name buf-name)
      "%b")))

(setq frame-title-format 
      '("" (:eval (kipps-frame-title-format))))
