
;; Add melpa package archive
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(defvar my-packages
  '(magit haskell-mode solarized-theme yasnippet popup)
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
        (package-install pack)))))
  
(my-ensure-packages-installed)
