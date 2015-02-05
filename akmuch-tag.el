(defun akmuch-tag (tags &optional marked)
  (interactive "sTags: ")
  (let ((tag-list (split-string tags " "))
	arg-list ident buff (current-buffer) tbuff)
    (if marked
	(setq ident "tag:*")
      (setq ident (akmuch-get-thread-id)))
    (setq tag-list
	  (mapcar (lambda (s)
		    (if (string-match "^[-\\+]" s) s (concat "+" s)))
	    tag-list))
    (setq arg-list
	  (append '(call-process "notmuch" nil nil nil "tag")
		  tag-list
		  (list "--" ident)))
    (eval arg-list))
  (akmuch-refresh))

(defun akmuch-delete (&optional from-here)
  (interactive "P")
  (let ((tid (akmuch-get-thread-id)))
    (if (and from-here (or (eq akmuch-display-type 'search)
			   (eq akmuch-display-type 'threadlist)))
	(save-excursion
	  (while (< (point) (point-max))
	    (forward-line 1)
	    (setq tid (concat tid " " (akmuch-get-thread-id))))))
    (call-process "notmuch" nil nil nil "tag"
		  "-inbox" "-unread" tid)
    (akmuch-refresh)))

(defun akmuch-mark ()
  (interactive)
  (akmuch-tag "*"))

(defun akmuch-mark-move ()
  (interactive)
  (akmuch-mark)
  (akmuch-next))

(defun akmuch-unmark ()
  (interactive)
  (akmuch-tag "-*"))

(defun akmuch-unmark-all ()
  (interactive)
  (akmuch-tag "-*" t))

(defun akmuch-tag-marked (tags)
  (interactive "sTags: ")
  (akmuch-tag tags t))

(provide 'akmuch-tag)