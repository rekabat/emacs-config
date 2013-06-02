(require 'popup)
(require 'artist)

;; unimplemented
(defvar bc-hide-star nil)  ;Hide all buffers that start and end with '*'
; potentially hop to other window if it contains the buffer you want to switch to. Maybe that's a separate keybind that only lists open ones.
; a key to close a selected buffer
;;(defvar bc-hide-list ())
;;(add-to-list 'bc-hide-list '"*Messages*")


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


(defvar bc-max-height 15)
(defvar bc-include-search nil)
(defvar bc-keybinds popup-menu-keymap)
(define-key bc-keybinds (kbd "C-k") 'bc-kill-selected-buffer)

(defun bc-set-next-prev-keybinds (next prev)
  (global-set-key next 'bc-launch-next)
  (global-set-key prev 'bc-launch-previous)
  (define-key bc-keybinds next 'popup-next)
  (define-key bc-keybinds prev 'popup-previous)
)

(setq bc-list-of-buffers ())
(setq bc-cycling -1)


(defun bc-update-buffer-list ()
  (interactive)
  ;; more effecient to create the list once, reverse it for the sake of the first call, so things go in in order
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

(defun bc-window-line-number ()
  (save-excursion
    (goto-char (window-start))
    (line-number-at-pos)))

(defun bc-launch-popup ()
  (let* ((menu-height (min bc-max-height (length bc-list-of-buffers) (- (window-height) 4)))
         (menu-width (apply 'max (mapcar
         	                        (lambda (a) (length (buffer-name a)))
                                    bc-list-of-buffers)))
         (x (/ (- (window-width) menu-width) 2))
         (y (+ (- (bc-window-line-number) 2) (/ (- (window-height) menu-height) 2)))
         (modified (buffer-modified-p))
         (saved-text (buffer-substring (window-start) (window-end)))
         (old-pos (point))
         (menu-pos (save-excursion (artist-move-to-xy x y) (point))))
    (unwind-protect
         (let ((thepopup (popup-menu*
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
           (message "Opened buffer: %s" thepopup)
           (let ((i (position thepopup bc-list-of-buffers)))
                (defun remove-nth (n list)
                  (if (or (zerop n) (null list))
                  	(cdr list)
                  	(cons (car list) (remove-nth (1- n) (cdr list)))))
                (setq bc-list-of-buffers (remove-nth i bc-list-of-buffers)))
           (push thepopup bc-list-of-buffers)
           thepopup))))

(defun bc-launch-next ()
  (interactive)
  (switch-to-buffer
   (bc-launch-popup)))

(defun bc-launch-previous ()
  (interactive)
  (switch-to-buffer
   (bc-launch-popup)))

(provide 'buffer-cycle)
