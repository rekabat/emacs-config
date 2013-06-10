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

;; maximize the window on open
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(add-hook 'after-init-hook 'toggle-fullscreen)

;; start in an empty buffer instead of the help screen
(defun create-empty-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "empty")))
(setf inhibit-splash-screen t)
;; no longer want to start in empty buffer because I load desktops
;;(switch-to-buffer (get-buffer-create "empty"))
(global-set-key (kbd "M-n") 'create-empty-buffer)
;;(delete-other-windows)

;; display the column number
(setq column-number-mode t)

;; display line numbers in left column
(global-linum-mode 1)

;; highlight the line the cursor is currently on
(global-hl-line-mode 1)

;; highlight the matching delimeter that the cursor is on
(show-paren-mode 1)

;; set GOTO line to C-t. This overwrites the default function of transpose which frankly seems mostly useless. Generally you'll just delete the switched letters
(global-set-key "\C-t" 'goto-line)

;; set enter to be new line and indent by default in prog modes
(add-hook 'prog-mode-hook (lambda ()  (local-set-key (kbd "RET") 'newline-and-indent)))

;; save all current buffers for next time, if you want to clear desktop (for whatever reason) use desktop-clear
;(desktop-change-dir "~/.emacs.d/desktop/")
;(add-hook 'after-init-hook 'desktop-read)
;(add-hook 'kill-emacs-hook 'desktop-save-in-desktop-dir)
;(desktop-save-mode t)
;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
;      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

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

;; line wrap on long lines
;(set-fill-column 80)
;(global-visual-line-mode t)

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
;; (setq bc-use-popup nil)
(bc-set-next-prev-keybinds (kbd "C-;") (kbd "C-'"))
(bc-set-kill-keybind (kbd "C-k"))
;(bc-set-next-prev-keybinds '(control \;) '(control \') )
;(bc-set-next-prev-keybinds [?\C-\;] [?\C-\'] )


;;;;;;;;;;;;;;;;;;;;;;
;; Other libs (online)
;;;;;;;;;;;;;;;;;;;;;;

; (require 'dirtree)
