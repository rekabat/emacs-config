(require 'popup)
(require 'artist)

(defvar psw-in-window-center nil
  "Non-nil means horizontal locate popup menu in the window center.
Locate popup menu in the `fill-column' center otherwise.")

(defvar bc-hide-star nil)  ;Hide all buffers that start and end with '*'
; potentially hop to other window if it contains the buffer you want to switch to. Maybe that's a separate keybind that only lists open ones.
; a key to close a selected buffer
(defvar bc-hide-list ())
(add-to-list 'bc-hide-list '"*Messages*")

(defun psw-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun psw-window-line-number ()
  (save-excursion
    (goto-char (window-start))
    (line-number-at-pos)))

(defun psw-get-buffer-list ()
  (psw-filter (lambda (a) (and
                      (buffer-live-p a)
                      (not (minibufferp a))
                      (not (equal (substring (buffer-name a) 0 2) " *"))
		      (not (member a bc-hide-list))
		      (if (and
			   bc-hide-star
			   (equal (substring (buffer-name a) 0 1) "*")
			   (equal (substring (buffer-name a) -1 nil) "*"))
			  nil
			  t)))
              (buffer-list)))

(defun psw-get-buffer (&optional window-center)
  (let* ((buf-list (psw-get-buffer-list))
         (menu-height (min 15 (length buf-list) (- (window-height) 4)))
         (x (/ (- (if (or psw-in-window-center window-center)
                      (window-width)
                    fill-column)
                  (apply 'max (mapcar (lambda (a)
                                        (length (buffer-name a)))
                                      buf-list))) 2))
         (y (+ (- (psw-window-line-number) 2)
               (/ (- (window-height) menu-height) 2)))
         (modified (buffer-modified-p))
         (saved-text (buffer-substring (window-start) (window-end)))
         (old-pos (point)))
    (unwind-protect
        (let* ((inhibit-read-only t)
               (menu-pos (save-excursion
                           (artist-move-to-xy x y)
                           (point)))
               (target-buffer (popup-menu* buf-list
                                           :point menu-pos
                                           :height menu-height
                                           :scroll-bar t
                                           :margin-left 1
                                           :margin-right 1
                                           :around nil
                                           :isearch t)))
          target-buffer)
      (when (buffer-modified-p)
        (delete-region (window-start) (window-end))
        (insert saved-text)
        (goto-char old-pos)
        (set-buffer-modified-p modified)))))

(defun psw-switch ()
  (interactive)
  (switch-to-buffer
   (psw-get-buffer)))

(provide 'popup-switcher)
