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
    solarized-theme
   )
)

;; install any uninstalled packages
(dolist (package packages-to-get)
  (when (not (package-installed-p package))
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration
;;;;;;;;;;;;;;;;;;;;;;;;

;; load the appropriate imported package theme
(load-theme 'solarized-light t)


;;;;;;;;;;;;;;;;;;;;;
;; Disable annoyances
;;;;;;;;;;;;;;;;;;;;;

;; turn off the blinking cursor
(blink-cursor-mode 0)
;; turn off the useless toolbar in GUI mode
(if window-system (tool-bar-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable helpful features
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display the column number
(setq column-number-mode t)
;; display line numbers in left column
(global-linum-mode 1)
;; set GOTO line to C-t. This overwrites the default function of transpose which frankly seems mostly useless. Generally you'll just delete the switched letters
(global-set-key "\C-t" 'goto-line)
