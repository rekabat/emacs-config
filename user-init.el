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
  '(
    ;; themes
    base16-theme
    solarized-theme
    subatomic-theme
    ;; subatomic256-theme
    ;; monokai-theme
    ;; zenburn-theme

    ;; ;; Sublime-like scrolling
    ;; sublimity

    ;; directory browsing
    neotree

    ;; tabs for buffers
    tabbar

    ;; better scrollbars
    yascroll

    ;; improve the bar on the bottom
    telephone-line

    ;; ;; nyan-cat doc position
    ;; nyan-mode

    ;; ;; project management
    ;; projectile

    ;; session management
    workgroups2

    ;; Highlight nested parens, brackets, braces a different color at each depth.
    rainbow-delimiters

    ;; M-x interface with Ido-style fuzzy matching.
    smex

    ;; Visual Popup User Interface
    popup

    ;; auto-complete framework
    company

    ;; for fuzzy matching
    company-flx

    ;; Auto Completion for GNU Emacs
    ;; auto-complete
    ;; for fuzzy completion in auto-complete
    ;; fuzzy

    ;; minor mode for editing parentheses
    paredit

    ;; Treat undo history as a tree
    undo-tree

    ;; Window switching
    switch-window

    ;; switch to other buffers and files via popup.
    ;; popup-switcher  ; also requires package "popup"

    ;;All of the clojure stuff
    ;;Major mode for Clojure code
    ;; clojure-mode
    ;;Client for Clojure nREPL
    ; nrepl
    ;;auto-complete sources for Clojure using nrepl completions
					; ac-nrepl


    ;; Entertainment
    ;; xkcd
    2048-game

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
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (load-theme 'sanityinc-tomorrow-eighties t)
;; (load-theme 'subatomic256 t)
;; (load-theme 'subatomic t)
(load-theme 'base16-eighties t)

;; tabbar
(add-hook 'after-init-hook #'tabbar-mode) ;; open on startup
(load "~/.emacs.d/customize-tabbar.el")

;; scrollbar
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)

;; telephone-line
(require 'telephone-line)
(telephone-line-mode 1)

;; neotree on startup
;; (add-hook 'after-init-hook #'neotree-toggle)

;; ;; sublimity
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (sublimity-mode 1)

;; workgroups
(require 'workgroups2)
(workgroups-mode 1)

;; activate rainbow delimiters in all modes
; (global-rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; activate smex and assign it to M-x instead of the default
(smex-initialize) ; initializes at startup (not first use)

;; activate auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)

;; company-flx
(with-eval-after-load 'company
  (company-flx-mode +1))

;; paredit minor-mode hooks
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook       'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)

;; globally enable undo-tree-mode
(global-undo-tree-mode)

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

;; ;; enable eldoc in clojure buffers
;; (add-hook 'nrepl-interaction-mode-hook
;;   'nrepl-turn-on-eldoc-mode)

;; ;; enable autocomplete in nrepl buffers (ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))


;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#d3d0c8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Ubuntu Mono")))))



;;;;;;;;;;;;;;;;;;;;;
;; Disable annoyances
;;;;;;;;;;;;;;;;;;;;;

;; turn off the blinking cursor
(blink-cursor-mode 0)

;; turn off the alert bell and make it display a square on the screen
(setq visible-bell t)

;; turn off the useless toolbar and menubar in GUI mode
(if window-system (tool-bar-mode -1))
(if window-system (menu-bar-mode -1))
(if window-system (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable helpful features
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximize the window on open
; (defun toggle-fullscreen ()
;   "Toggle full screen"
;   (interactive)
;   (set-frame-parameter
;      nil 'fullscreen
;      (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
; (add-hook 'after-init-hook 'toggle-fullscreen)

;; start in an empty buffer instead of the help screen
(defun create-empty-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "empty")))
(setf inhibit-splash-screen t)
;; no longer want to start in empty buffer because I load desktops
;;(switch-to-buffer (get-buffer-create "empty"))
;;(delete-other-windows)

;; display the column number
(setq column-number-mode t)

;; display line numbers in left column
(global-linum-mode 1)

;; highlight the line the cursor is currently on
(global-hl-line-mode 1)

;; highlight the matching delimeter that the cursor is on
(show-paren-mode 1)

;; Ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; set enter to be new line and indent by default in prog modes
(add-hook 'prog-mode-hook (lambda ()  (local-set-key (kbd "RET") 'newline-and-indent)))

;; automatically delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; line wrap on long lines
;(set-fill-column 80)
;(global-visual-line-mode t)

;; scroll constant number of lines at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))) ;; five lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; store all backup files "*~" in a central location
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; ;; highlight all TODOs
;; (add-hook 'prog-mode-hook 'highlight-todos)

;;;;;;;;;;;;;;;;
;; Functions to be keybound
;;;;;;;;;;;;;;;

;; auto-indent yanked text
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(defun yank-pop-and-indent ()
  "Yank-pop and then indent the newly formed regiona according to mode."
  (interactive)
  (yank-pop)
  (call-interactively 'indent-region))
(global-set-key (kbd "C-y") 'yank-and-indent)
(global-set-key (kbd "M-y") 'yank-pop-and-indent)

;; comment block or line
(defun comment-or-uncomment-region-or-line (begin end)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (line-beginning-position) (line-end-position))))
  (comment-or-uncomment-region begin end)
  ;; (next-line)
  )
(global-set-key (kbd "C-,") 'comment-or-uncomment-region-or-line)

;; kill block or line
(defun kill-region-or-line (begin end)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (line-beginning-position) (line-end-position))))
  (kill-region begin end))
(global-set-key (kbd "C-M-<backspace>") 'kill-region-or-line)

;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;

;; smex
(global-set-key "\M-x" 'smex)
(global-set-key "\M-X" 'smex-major-mode-commands)
(global-set-key "\C-c \C-c \M-x" 'execute-extended-command) ; the old M-x

;; undo-tree redo
(global-set-key (kbd "C-.") 'undo-tree-redo)

;; set GOTO line to C-t. This overwrites the default function of transpose which frankly seems mostly useless. Generally you'll just delete the switched letters
(global-set-key (kbd "C-t") 'goto-line)

;; I hate C-x f, I don't want to change the width....
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'find-file)

(global-set-key (kbd "M-n") 'create-empty-buffer)

;; add keyboard shortcuts for easily cycling between buffers
;;(global-set-key (kbd "C-;") 'previous-buffer)
;;(global-set-key (kbd "C-'") 'next-buffer)


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

; (load "~/.emacs.d/lib/buffer-cycle/buffer-cycle.el")
; (setq bc-use-popup nil)
; (bc-set-next-prev-keybinds (kbd "C-;") (kbd "C-'"))
; (bc-set-kill-keybind (kbd "C-k"))
; ;(bc-set-next-prev-keybinds '(control \;) '(control \') )
; ;(bc-set-next-prev-keybinds [?\C-\;] [?\C-\'] )


;; (load "~/.emacs.d/lib/session-manager/session-manager.el")


;;;;;;;;;;;;;;;;;;;;;;
;; Other libs (online)
;;;;;;;;;;;;;;;;;;;;;;

; (require 'dirtree)
(load "~/.emacs.d/ex-lib/dircolors.el")
