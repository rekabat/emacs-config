;; initialize the package manager "melpa"
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; refresh contents if need be
(when (not package-archive-contents)
  (package-refresh-contents))

;; define a list of all the packages of interest
(defvar packages-to-get
  '(;; themes
    zenburn-theme
   )
)

;; install any uninstalled packages
(dolist (package packages-to-get)
  (when (not (package-installed-p package))
    (package-install package)))

;; load the appropriate imported package theme
(load-theme 'zenburn t)

;; turn off the blinking cursor
(blink-cursor-mode 0)
