;; known issues:
;;    1: The lock file persists, so I just set desktop-load-locked-desktop to true


(defvar sm-auto-save nil "Default nil; get prompt to save upon exiting Emacs. Set to t to automatically save.")
(defvar sm-auto-load t "Default t; automatically load saved session. Set to nil to get prompt upon loading Emacs.")


;; use only one desktop
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      ;; desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t)



(defun sm-saved-desktop-p ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use sm-delete-desktop to remove the saved desktop manually
(defun sm-delete-desktop ()
  "Clear the saved emacs session."
  (interactive)
  (if (sm-saved-desktop-p)
      (let* ((temp-name desktop-dirname))
	(desktop-remove)
	(setq desktop-dirname temp-name))
    (message "No saved session (to delete).")))

;; use sm-load-desktop to restore the desktop manually
(defun sm-load-desktop ()
  "Restore a saved emacs session."
  (interactive)
  (if (sm-saved-desktop-p)
      (let* ((temp-name desktop-dirname))
	(desktop-read)
	(setq desktop-dirname temp-name))
    (message "No saved session (to load).")))

;; use sm-save-desktop to save the desktop manually
(defun sm-save-desktop ()
  "Save an emacs session. Will overwrite previous session."
  (interactive)
  (sm-delete-desktop)
  (desktop-save desktop-dirname))




(if sm-auto-save
    (add-hook 'kill-emacs-hook 'sm-save-desktop)
  (add-hook 'kill-emacs-hook
	    '(lambda ()
	       (if (y-or-n-p "Save session?")
		   (sm-save-desktop)
		 (if (sm-saved-desktop-p)
		     (if (not (y-or-n-p "Preserve previous session?"))
			 (sm-delete-desktop)))))))

(if sm-auto-load
    (add-hook 'after-init-hook 'sm-load-desktop)
  (add-hook 'after-init-hook
	    '(lambda ()
	       (if (sm-saved-desktop-p)
		   (if (y-or-n-p "Restore desktop? ")
		       (sm-load-desktop))))))
