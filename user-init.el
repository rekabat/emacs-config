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

    ;; minor mode for editing parentheses
    paredit

    ;; switch to other buffers and files via popup.
;    popup-switcher  ; also requires package "popup"

    ;;All of the clojure stuff
    ;;Major mode for Clojure code
    clojure-mode
    ;;Client for Clojure nREPL
    nrepl
    ;;auto-complete sources for Clojure using nrepl completions
    ac-nrepl

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

;; paredit minor-mode hooks
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)

;; enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; enable autocomplete in nrepl buffers (ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
 (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Disable annoyances
;;;;;;;;;;;;;;;;;;;;;

;; turn off the blinking cursor
(blink-cursor-mode 0)
;; turn off the alert bell and make it display a square on the screen
(setq visible-bell t)
;; turn off the useless toolbar in GUI mode
(if window-system (tool-bar-mode 0))
;; I hate C-x f, I don't want to change the width....
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'find-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable helpful features
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display the column number
(setq column-number-mode t)
;; display line numbers in left column
(global-linum-mode 1)
;; highlight the line the cursor is currently on
(global-hl-line-mode 1)
;; highlight the matching delimeter that the cursor is on
(show-paren-mode 1)
;; start in an empty buffer instead of the help screen
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "empty"))
;;(delete-other-windows)
;; set GOTO line to C-t. This overwrites the default function of transpose which frankly seems mostly useless. Generally you'll just delete the switched letters
(global-set-key "\C-t" 'goto-line)
;; set enter to be new line and indent by default in prog modes
(add-hook 'prog-mode-hook (global-set-key (kbd "<RET>") 'newline-and-indent))

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


;;;;;;;;;;;;;;;;;;;;;;
;; Other libs (online)
;;;;;;;;;;;;;;;;;;;;;;

; (require 'dirtree)
