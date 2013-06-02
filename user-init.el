;;;;;;;;;;;;;;;;;;;;;;;;
;; Load package managers
;; Install packaged
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; refresh contents if need be
(when (not package-archive-contents)
  (package-refresh-contents))

;; define a list of all the packages of interest
(defvar packages-to-get
  '(;; themes
    solarized-theme
    ;;monokai-theme
    ;;zenburn-theme

    ;; Highlight nested parens, brackets, braces a different color at each depth.
    rainbow-delimiters
    
    ;; M-x interface with Ido-style fuzzy matching.
    smex

    ;; All the packages necessary for auto-complete
    ;; Visual Popup User Interface
    popup
    ;; Auto Completion for GNU Emacs
    auto-complete

    ;; switch to other buffers and files via popup.
;    popup-switcher  ; also requires package "popup"
   )
)

;; install any uninstalled packages
(dolist (package packages-to-get)
  (when (not (package-installed-p package))
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration
;;;;;;;;;;;;;;;;;;;;;;;;

;; load the appropriate imported package theme. I like: Misterioso, Whiteboard, wombat, solarized-light, solarized-dark, zenburn
(load-theme 'solarized-dark t)

;; activate rainbow delimiters in all modes
(global-rainbow-delimiters-mode)

;; activate smex and assign it to M-x instead of the default
(smex-initialize) ; initializes at startup (not first use)
(global-set-key "\M-x" 'smex)
(global-set-key "\M-X" 'smex-major-mode-commands)
(global-set-key "\C-c \C-c \M-x" 'execute-extended-command) ; the old M-x

;; activate auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; set the popup switcher to be in the middle of the screen and set shortcut
;(setq psw-in-window-center t)
;(global-set-key "\C-;" 'psw-switch)


;;;;;;;;;;;;;;;;;;;;;
;; Disable annoyances
;;;;;;;;;;;;;;;;;;;;;

;; turn off the blinking cursor
(blink-cursor-mode 0)
;; turn off the alert bell and make it display a square on the screen
(setq visible-bell t)
;; turn off the useless toolbar in GUI mode
(if window-system (tool-bar-mode 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable helpful features
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display the column number
(setq column-number-mode t)
;; display line numbers in left column
(global-linum-mode 1)
;; highlight the line the cursor is currently on
(global-hl-line-mode 1)
;; start in an empty buffer instead of the help screen
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "empty"))
;;(delete-other-windows)
;; set GOTO line to C-t. This overwrites the default function of transpose which frankly seems mostly useless. Generally you'll just delete the switched letters
(global-set-key "\C-t" 'goto-line)
;; add keyboard shortcuts for easily cycling between buffers
;(global-set-key (kbd "C-;") 'previous-buffer)
;(global-set-key (kbd "C-'") 'next-buffer)

;;;;;;;;;;;;;;;;
;; OS X specific
;;;;;;;;;;;;;;;;

(cond
 ((eq system-type 'darwin)
    (setq ns-alternate-modifier 'none)
    (setq ns-command-modifier 'meta)))


;;;;;;;;;;;;;;;;
;; Personal libs
;;;;;;;;;;;;;;;;

(load "~/.emacs.d/lib/buffer-cycle/buffer-cycle.el")
(bc-set-next-prev-keybinds (kbd "C-;") (kbd "C-'"))
;(bc-set-next-prev-keybinds '(control \;) '(control \') )
;(bc-set-next-prev-keybinds [?\C-\;] [?\C-\'] )