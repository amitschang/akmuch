(defun akmuch-reply-sender ()
  (interactive)
  (akmuch-reply t))

(defun akmuch-reply (&optional senderonly)
  (interactive)
  (let (file m-strt m-end)
    (setq
     file
     (if (buffer-live-p (get-buffer akmuch-message-buffer))
	 (with-current-buffer akmuch-message-buffer
	   akmuch-message-filename)
       (akmuch-message-latest (akmuch-get-thread-id))))
    (when file
      (switch-to-buffer-other-frame (generate-new-buffer "*unsent mail*"))
      (if senderonly
	  (call-process akmuch-notmuch-helper nil (current-buffer) nil
			"reply" "--reply-to=sender" file)
	(call-process akmuch-notmuch-helper nil (current-buffer) nil
		      "reply" file))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (forward-line -1)
      (insert "--text follows this line--\n\n")
      (goto-char (point-min))
      ;; hide the in-reply-to and references fields
      (save-excursion
	(search-forward-regexp "^In-Reply-To:")
	(setq m-strt (point-at-bol))
	(search-forward-regexp "^[^ \t]")
	(add-text-properties m-strt (- (point) 1) '(invisible t read-only t)))
      (save-excursion
	(search-forward-regexp "^References:")
	(setq m-strt (point-at-bol))
	(search-forward-regexp "^[^ \t]")
	(add-text-properties m-strt (- (point) 1) '(invisible t read-only t)))
      (search-forward "--text follows this line--")
      (forward-line)
      (message-mode)
      (run-hooks 'message-setup-hook)
      (set-buffer-modified-p nil))))

(defun akmuch-forward ()
  (interactive)
  (let (file)
    (setq
     file
     (if (buffer-live-p (get-buffer akmuch-message-buffer))
         (with-current-buffer akmuch-message-buffer
           akmuch-message-filename)
       (akmuch-message-latest (akmuch-get-thread-id))))
    (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion)
	  (coding-system-for-write 'no-conversion)
	  buff subject start (done nil)
          )
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not done)
      	(if (or (looking-at (concat "^\\(From\\|To\\|Date\\|Subject\\|"
				    "Content-Transfer-Encoding\\|"
				    "Cc\\|Content-Type\\|MIME-Version\\): "))
		(looking-at "^[ \t]"))
	    (forward-line 1)
	  (delete-region
	   (point)
	   (save-excursion
	     (forward-char 1)
	     (search-forward-regexp "^[^ \t]")
	     (forward-char -1)
	     (point))))
	(when (looking-at "^$")
	  (setq done t)))
      (setq buff (current-buffer))
      (message-mail)
      (message-forward-make-body buff nil)
      (search-forward-regexp "^<#mml")
      (when
	  (search-forward-regexp
	   "^Subject: "
	   (save-excursion (search-forward-regexp "^$") (point)) t)
	(setq subject (buffer-substring (point) (point-at-eol))))
      (goto-char (point-min))
      (search-forward-regexp "^Subject: ")
      (insert (concat "Fwd: " subject))
      (goto-char (point-min))
      (search-forward-regexp "^To: ")
      (set-buffer-modified-p nil))))

(provide 'akmuch-reply)
