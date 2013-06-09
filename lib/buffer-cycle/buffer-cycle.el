;; known issues:
;;    1: Killing the last buffer causes a "#<killed buffer>" to appear in the list
;;    2: It can't open in the elpa directory (dired), it says that it's read-only... But it works in .emacs.d

;; to implement:
;;    1: (defvar bc-hide-star nil)   ; Hide all buffers that start and end with '*'
;;    2: defvar bc-hide-list ())     ; a user defined list of buffers to hide
;;    3: a keybind to kill the selected buffer
;;    4: hop to another window if the selected buffer is already open
;;    5: automatically go to the second or last item depending on the button pressed.

;; design questions:
;;


;; for the sake of extending the functionality of this package, 
;; I've included all the default keybinds of the popup system.

; (defvar popup-menu-keymap
;   (let ((map (make-sparse-keymap)))
;     (define-key map "\r"        'popup-select)
;     (define-key map "\C-f"      'popup-open)
;     (define-key map [right]     'popup-open)
;     (define-key map "\C-b"      'popup-close)
;     (define-key map [left]      'popup-close)

;     (define-key map "\C-n"      'popup-next)
;     (define-key map [down]      'popup-next)
;     (define-key map "\C-p"      'popup-previous)
;     (define-key map [up]        'popup-previous)

;     (define-key map [f1]        'popup-help)
;     (define-key map (kbd "\C-?") 'popup-help)

;     (define-key map "\C-s"      'popup-isearch)

;     (define-key map [mouse-1]   'popup-select)
;     (define-key map [mouse-4]   'popup-previous)
;     (define-key map [mouse-5]   'popup-next)
;     map))

(require 'popup)
(require 'artist)

(defvar bc-use-popup t "Default t. Set to nil to use the minibuf prompt.")
(defvar bc-max-height 15 "Max number of rows in the popup. Will use scrollbar if in excess of max.")
(defvar bc-include-search nil  "Default nil. Set to t to allow searching of buffers when using popoup.")

(defvar bc-next-key nil "Set using bc-set-next-prev-keybinds.")
(defvar bc-prev-key nil "Set using bc-set-next-prev-keybinds.")
(defvar bc-select-key (kbd "RET"))
(defvar bc-kill-key nil "Set using bc-set-kill-keybind.")

(defvar bc-keybinds popup-menu-keymap)
;(define-key bc-keybinds bc-kill-selected-buf 'bc-kill-selected-buffer)
(define-key bc-keybinds bc-select-key 'popup-select)

(defun bc-set-next-prev-keybinds (next prev)
  "Set all the appropriate buffer switching functions to be bound o the provided keybinds."
  (setq bc-next-key next)
  (setq bc-prev-key prev)
  (global-set-key next 'bc-launch-next)
  (global-set-key prev 'bc-launch-previous)
  (define-key bc-keybinds next 'popup-next)
  (define-key bc-keybinds prev 'popup-previous))

(defun bc-set-kill-keybind (kill)
  "Set all the appropriate buffer killing functions to be bound to the provided keybind."
  (setq bc-kill-key kill)
  (define-key bc-keybinds kill '(lambda (thepopup)
				 (message "hello")
				 (let ((buff (popup-selected-item thepopup)))
				   (popup-close thepopup)
				   (bc-kill-selected-buffer buff)))))

(setq bc-list-of-buffers ())   ; how do I make this private? And other functions



(defun bc-update-buffer-list ()
  "Called whenever buffer-list is updated. Uses the buffer-list-update-hook."
  (interactive)
  ;; more effecient to create the list once
  ;; reverse it for the sake of the first call, so things go in in order
  (setq bl (reverse (buffer-list)))
  ;; adds any buffers added to the list (after filtering)
  (dolist (b bl)
   (when (and 
   	      (not (member b 'bc-list-of-buffers))
   	      (buffer-live-p b)
          (not (minibufferp b))
          (not (equal (substring (buffer-name b) 0 2) " *")))
    (add-to-list 'bc-list-of-buffers b)))
  ;; removes any buffers that had been killed
  (setq bc-list-of-buffers (member-if 'buffer-live-p bc-list-of-buffers)))

;; update every time there is a change to the buffer list
(add-hook 'buffer-list-update-hook 'bc-update-buffer-list)


(defun bc-remove-buffer-from-list (buff)
  "Removes a buffer from bc-list-of-buffers."
  (let ((i (position buff bc-list-of-buffers)))
    (defun remove-nth (n list)
      (if (or (zerop n) (null list))
	  (cdr list)
	(cons (car list) (remove-nth (1- n) (cdr list)))))
    (setq bc-list-of-buffers (remove-nth i bc-list-of-buffers))))

(defun bc-kill-selected-buffer (buff)
  "Asks if you're sure you want to kill, then calls kill-buffer on the buffer and removes it from bc-list-of-buffers."
  (if (y-or-n-p (format "Kill buffer \"%s\"?" buff))
      (progn
	(bc-remove-buffer-from-list buff)
	(kill-buffer buff))))

(defun bc-switch-to-buffer-and-put-on-top (buff)
  "Bumps the selected buffer to the top of bc-list-of-buffers."
  (bc-remove-buffer-from-list buff)
  (push buff bc-list-of-buffers)
  (switch-to-buffer buff))  

(defun bc-launch-popup ()
  "Called by the two main functions of buffer-cycle: bc-launch-next & bc-launch-previous."
  (let* ((menu-height (min bc-max-height (length bc-list-of-buffers) (- (window-height) 4)))
	 (menu-width (apply 'max (mapcar
				  (lambda (a) (length (buffer-name a)))
                                    bc-list-of-buffers)))
         (x (/ (- (window-width) menu-width) 2))
         (y (+ (- (save-excursion (goto-char (window-start)) (line-number-at-pos)) 2)
            (/ (- (window-height) menu-height) 2)))
         (modified (buffer-modified-p))
         (saved-text (buffer-substring (window-start) (window-end)))
         (old-pos (point))
         (menu-pos (save-excursion (artist-move-to-xy x y) (point))))
    (unwind-protect
         (let* ((inhibit-read-only t)
         	    (thepopup (popup-menu*
         	          bc-list-of-buffers
         	          :point menu-pos
         	          :width menu-width
         	          :height menu-height
         	          :keymap bc-keybinds
         	          :scroll-bar t
         	          :isearch bc-include-search)))
           (when (buffer-modified-p)
           	  (delete-region (window-start) (window-end))
              (insert saved-text)
              (goto-char old-pos)
              (set-buffer-modified-p modified))
           (bc-switch-to-buffer-and-put-on-top thepopup)))))

(defun bc-launch-prompt (start-position)
  "Opens the minibuf prompt for changing buffers. Called recursively by subsequent keystrokes."
  (interactive)
  (cond ((>= start-position (length bc-list-of-buffers)) (setq start-position 0))
        ((<= start-position -1) (setq start-position (- (length bc-list-of-buffers) 1))))
  (message "Switch to buffer %d: \"%s\"" start-position (nth start-position bc-list-of-buffers))
  (let ((the-event (read-char)))
    (cond ((eq the-event (elt bc-select-key 0)) (bc-switch-to-buffer-and-put-on-top (elt bc-list-of-buffers start-position)))
	  ((eq the-event (elt bc-kill-key 0)) (bc-kill-selected-buffer (elt bc-list-of-buffers start-position)))
          ((eq the-event (elt bc-next-key 0)) (bc-launch-prompt (+ start-position 1)))
          ((eq the-event (elt bc-prev-key 0)) (bc-launch-prompt (- start-position 1))))))

(defun bc-launch-next ()
  "One of two ways to launch the buffer-cycle popup window. Recommended that it's bound to C-;"
  (interactive)
  (if bc-use-popup
    (bc-launch-popup)
    (bc-launch-prompt 1)))

(defun bc-launch-previous ()
  "One of two ways to launch the buffer-cycle popup window. Recommended that it's bound to C-'"
  (interactive)
  (if bc-use-popup
    (bc-launch-popup)
    (bc-launch-prompt (- (length bc-list-of-buffers) 1))))

(provide 'buffer-cycle)
