(defun akmuch-reply-sender ()
  (interactive)
  (akmuch-reply t))

(defun akmuch-reply (&optional senderonly)
  (interactive)
  (let (mid faddr m-strt m-end)
    (if (eq akmuch-display-type 'search)
	(setq mid (akmuch-get-latest-message-id))
      (setq mid akmuch-current-message-id))
    (when mid
      (switch-to-buffer (generate-new-buffer "*unsent mail*"))
      (if senderonly
	  (call-process "notmuch" nil (current-buffer) nil
			"reply" "--reply-to=sender" mid)
	(call-process "notmuch" nil (current-buffer) nil
		      "reply" mid))
      (when akmuch-fill-messages
	(akmuch-fill-message))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (forward-line -1)
      (insert "--text follows this line--\n\n")
      (goto-char (point-min))
      ;; make to and cc fields a little prettier
      (save-excursion
	(search-forward-regexp "^To:")
	(setq m-end
	      (save-excursion
		(search-forward-regexp "^[^ \t]") (point)))
	(while (re-search-forward "\t" m-end t)
	  (replace-match " ")))
      (save-excursion
	(when (search-forward-regexp "^Cc:" nil t)
	(setq m-end
	      (save-excursion
		(search-forward-regexp "^[^ \t]") (point)))
	(while (re-search-forward "\t" m-end t)
	  (replace-match " "))))
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
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion)
	  (coding-system-for-write 'no-conversion)
	  buff subject start (done nil))
      (call-process "notmuch" nil (current-buffer) nil
		    "show" "--format=raw" akmuch-current-message-id)
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
