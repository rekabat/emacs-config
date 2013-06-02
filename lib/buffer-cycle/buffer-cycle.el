;; known issues:
;;    1: Killing the last buffer causes a "#<killed buffer>" to appear in the list
;;    2: It can't open in the elpa directory (dired), it says that it's read-only... But it works in .emacs.d

;; to implement:
;;    1: (defvar bc-hide-star nil)   ; Hide all buffers that start and end with '*'
;;    2: defvar bc-hide-list ())     ; a user defined list of buffers to hide
;;    3: a keybind to kill the selected buffer
;;    4: hop to another window if the selected buffer is already open
;;    5: automatically go to the second or last item depending on the button pressed.


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

(defvar bc-use-popup nil)
(defvar bc-max-height 15)
(defvar bc-include-search nil)
(defvar bc-keybinds popup-menu-keymap)
(define-key bc-keybinds (kbd "C-k") 'bc-kill-selected-buffer)

(defvar bc-next-key nil)
(defvar bc-prev-key nil)
(defvar bc-select-buf (kbd "RET"))

(defun bc-set-next-prev-keybinds (next prev)
  (setq bc-next-key next)
  (setq bc-prev-key prev)
  ; (global-set-key next 'bc-launch-next)
  ; (global-set-key (event-convert-list prev) 'bc-launch-previous)
  ; (define-key bc-keybinds (event-convert-list next) 'popup-next)
  ; (define-key bc-keybinds (event-convert-list prev) 'popup-previous)

  (global-set-key next 'bc-launch-next)
  (global-set-key prev 'bc-launch-previous)
  (define-key bc-keybinds next 'popup-next)
  (define-key bc-keybinds prev 'popup-previous)


  ; (global-set-key (kbd next) 'bc-launch-next)
  ; (global-set-key (kbd prev) 'bc-launch-previous)
  ; (define-key bc-keybinds (kbd next) 'popup-next)
  ; (define-key bc-keybinds (kbd prev) 'popup-previous)
)

(setq bc-list-of-buffers ())   ; how do I make this private? And other functions
(setq bc-current-buffer 0)

(defun bc-kill-selected-buffer (thepopup)
  ; (let ((i (position (get-selected-item thepopup) bc-list-of-buffers)))
  ;        (defun remove-nth (n list)
  ;          (if (or (zerop n) (null list))
  ;          	(cdr list)
  ;          	(cons (car list) (remove-nth (1- n) (cdr list)))))
  ;        (setq bc-list-of-buffers (remove-nth i bc-list-of-buffers)))
  (let ((height (thepopup-height thepopup))
        (cursor (1+ (thepopup-cursor thepopup)))
        (scroll-top (thepopup-scroll-top thepopup))
        (length (length (thepopup-list thepopup))))
    (cond
     ((>= cursor length)
      ;; Back to first page
      (setq cursor 0
            scroll-top 0))
     ((= cursor (+ scroll-top height))
      ;; Go to next page
      (setq scroll-top (min (1+ scroll-top) (max (- length height) 0)))))
    (setf (thepopup-cursor thepopup) cursor
          (thepopup-scroll-top thepopup) scroll-top)
    (thepopup-draw thepopup)))

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

(add-hook 'buffer-list-update-hook 'bc-update-buffer-list)

(defun bc-switch-to-buffer-and-put-on-top (buff)
  (let ((i (position buff bc-list-of-buffers)))
       (defun remove-nth (n list)
         (if (or (zerop n) (null list))
         	(cdr list)
         	(cons (car list) (remove-nth (1- n) (cdr list)))))
       (setq bc-list-of-buffers (remove-nth i bc-list-of-buffers))
       (if (eq i 0)
           (message "Already selected buffer: \"%s\"" buff)
           (message "Opened buffer: \"%s\"" buff)))
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
  (interactive)
  (cond ((>= start-position (length bc-list-of-buffers)) (setq start-position 0))
        ((<= start-position -1) (setq start-position (- (length bc-list-of-buffers) 1))))
  (message "Switch to buffer %d: \"%s\"" start-position (nth start-position bc-list-of-buffers))
  (let ((the-event (read-char)))
    (cond ((eq the-event (elt bc-select-buf 0)) (bc-switch-to-buffer-and-put-on-top (elt bc-list-of-buffers start-position)))
          ((eq the-event (elt bc-next-key 0)) (bc-launch-prompt (+ start-position 1)))
          ((eq the-event (elt bc-prev-key 0)) (bc-launch-prompt (- start-position 1)))
          (t (message "%s %s" the-event bc-select-buf)))))

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
