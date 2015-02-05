(require 'akmuch-search)

(defcustom akmuch-show-expanded-recipients nil
  "whether to show full recipient list by default or summary")

(defun akmuch-get-thread-id ()
  ;;
  ;; return the threadid of the currently highlighted thread
  ;;
  (with-current-buffer akmuch-search-buffer
    (save-excursion
      (goto-char (point-at-bol))
      (buffer-substring-no-properties
       (point-at-bol)
       (+ (point-at-bol) 23)))))

(defun akmuch-get-latest-message-id ()
  ;;
  ;; get the latest message id of this thread. If there are any unread
  ;; message, return the first one, otherwise return the last message
  ;;
  (let ((thread (akmuch-get-thread-id)))
    (with-temp-buffer
      (call-process "notmuch" nil (current-buffer) nil
		    "search" "--output=messages" thread
		    "tag:unread")
      (when (eq (point) (point-min))
	(call-process "notmuch" nil (current-buffer) nil
		      "search" "--output=messages" thread))
      (goto-char (point-min))
      (buffer-substring (point) (point-at-eol)))))

(defun akmuch-view ()
  ;;
  ;; View a message in the message buffer
  ;;
  (interactive)
  (let ((mid (akmuch-get-latest-message-id)))
    (unless (buffer-live-p akmuch-message-buffer)
      (setq akmuch-message-buffer
	    (get-buffer-create "*akmuch message*")))
    (with-current-buffer akmuch-message-buffer
      (akmuch-message-view mid nil))))

(defun akmuch-message-view (id expanded-recip &optional type)
  (let ((buffer-read-only nil)
	(buff (current-buffer))
	(c 0)
	partshown theend hend
	headerline mid start)
    (if (null type)
	(setq type 'view))
    (when (eq akmuch-display-type 'threadlist)
      (setq akmuch-quit-to 'threadlist))
    (setq akmuch-display-type 'message)
    (with-temp-buffer
      (call-process "notmuch" nil (current-buffer) nil
		    "show" "--entire-thread" id)
      (start-process "junk" nil "notmuch"
		     "tag" "-unread" id)
      (goto-char (point-min))
      (search-forward-regexp "\^Lmessage{ .* match:1 ")
      ;;
      ;; Get the boundaries of this message, for processing the header
      ;; and parts
      ;;
      (setq start (point))
      (setq theend (save-excursion (search-forward-regexp
				    "\^Lmessage}" nil t)
				   (point)))
      (setq hend (save-excursion (search-forward-regexp
				  "\^Lheader}" nil t)
				 (point)))
      ;;
      ;; Now we process message info and header
      ;;
      (goto-char (+ (point-at-bol) 10))
      (setq mid
	    (buffer-substring-no-properties
	     (point)
	     (progn (search-forward-regexp " ")
		    (- (point) 1))))
      (forward-line 2)
      (setq tmp (buffer-substring (point-at-bol) (point-at-eol)))
      (search-forward-regexp "^Subject: ")
      (setq headerline
	    (format "[%d/%d] %s"
		    (count-matches
		     "\^Lmessage{ " (point-min) start)
		    (count-matches
		     "\^Lmessage{ " (point-min) (point-max))
		    (replace-regexp-in-string
		     "^ *R[Ee]: *" ""
		     (buffer-substring-no-properties
		      (point) (point-at-eol)))))
      ;;
      ;; Now that we have the header info, start formatting the
      ;; message in the akmuch buffer
      ;;
      (with-current-buffer buff
	(erase-buffer)
	(setq header-line-format headerline)
	(setq akmuch-current-message-id mid)
	(insert (propertize tmp 'face font-lock-comment-face))
	(insert "\n"))
      ;;
      ;; If there are recipients specified, make a shortened version
      ;; of them for the display
      ;;
      (if (or expanded-recip akmuch-show-expanded-recipients)
	  (progn
	    (forward-line 1)
	    (setq tmp (buffer-substring (point-at-bol) (point-at-eol)))
	    (forward-line 1)
	    (setq tmp (concat
		       "\n"
		       tmp
		       (buffer-substring (point-at-bol) (point-at-eol))))
	    (forward-line 1)
	    (setq tmp (concat
		       "\n"
		       tmp
		       (buffer-substring (point-at-bol) (point-at-eol)))))
	(goto-char start)
	(when (search-forward-regexp "^to: " hend t)
	  (setq nrecip (count-matches "," (point) (point-at-eol)))
	  (unless (search-forward-regexp "," (point-at-eol) t)
	    (goto-char (point-at-eol)))
	  (setq tmp (propertize (buffer-substring (point-at-bol) (point))
				'face font-lock-comment-face))
	  (when (> nrecip 0)
	    (setq tmp (concat tmp (propertize (format " +%d more" nrecip)
					      'face font-lock-comment-face))))
	  (goto-char start)
	  (when (search-forward-regexp "^cc: " hend t)
	    (setq nrecip (+ 1 (count-matches "," (point) (point-at-eol))))
	    (setq tmp (propertize (format "%s +%d Cc'd" tmp nrecip)
				  'face font-lock-comment-face)))))
      (with-current-buffer buff
	(insert tmp)
	(insert "\n"))
      (with-current-buffer buff (insert "\n"))
      ;;
      ;; Now go through the message and format the output
      ;;
      (akmuch-parse-message-contents (point) theend buff type))
    (when akmuch-fill-messages
      (akmuch-fill-message))
    (akmuch-colorize-message)
    (goto-char (point-min))
    (when (eq type 'list)
      (search-forward-regexp "^$")
      (search-forward-regexp "^ [0-9]")
      (forward-char -1))))

(defun akmuch-parse-message-contents (start end buff type)
  (let (tmp pend (depth 0) (c 0))
    (goto-char start)
    (search-forward-regexp "^\^Lbody{")
    (while (search-forward-regexp "^\^L\\(part\\|attachment\\){" end t)
      (setq c (+ 1 c))
      (setq depth (+ depth 1))
      (search-forward-regexp "Content-type: " (point-at-eol))
      (setq tmp (buffer-substring (point) (point-at-eol)))
      (when (and (eq type 'view)
		 (string-equal tmp "message/rfc822"))
	(search-forward-regexp "\^Lheader{")
	(forward-line 1)
	(setq pend (save-excursion
		     (search-forward-regexp "\^Lheader}")
		     (- (point) 8)))
	(setq tmp (buffer-substring (point) pend))
	(with-current-buffer buff
	  (insert "---------- Forwarded message ----------\n")
	  (insert tmp))
	(setq pend (save-excursion (search-forward-regexp "\^Lbody}") (point)))
	(akmuch-parse-message-contents (point) pend buff type))
      (when (eq type 'list)
	(goto-char (point-at-bol))
	(when (looking-at "\^Lattachment{")
	  (search-forward-regexp "Filename: ")
	  (setq tmp (format "%s (%s)" tmp (buffer-substring
					   (point)
					   (progn (search-forward-regexp ",")
						  (- (point) 1))))))
	(with-current-buffer buff
	  (insert (format "%2d%s%s\n" c (make-string depth 32) tmp))))
      (forward-line 1)
      (when (and (null partshown)
		 (looking-at "Non-text part: text/html")
		 (eq type 'view))
	(with-current-buffer buff
	  (let ((start (point)))
	    (call-process "notmuch" nil (current-buffer) nil
			  "show" (format "--part=%d" c) mid)
	    (if (featurep 'w3m)
		(w3m-region start (point))
	      (shell-command-on-region start (point) "w3m -dump -T text/html"
				       (current-buffer) t nil nil))
	    (font-lock-unfontify-region start (point)))))
      (if (looking-at "\^Lpart{")
	  nil
	(setq start (point))
	(search-forward-regexp "\^L\\(part\\|attachment\\)}")
	(search-backward-regexp "\^L")
	(setq pend (point))
	(while (looking-at "\^L\\(part\\|attachment\\)}")
	  (setq depth (- depth 1))
	  (forward-line 1))
	(goto-char start)
	(when (and (not (looking-at "Non-text part:"))
		   (eq type 'view))
	(setq partshown t)
	(setq tmp (buffer-substring start pend))
	(with-current-buffer buff
	  (insert tmp)))))))

(defun akmuch-fill-message ()
  (interactive)
  (let (fillstart fillpre fillend)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^$")
      (while (< (point) (point-max))
	(when (and (> (- (point-at-eol) (point-at-bol)) akmuch-long-line))
	  (setq fillstart (point))
	  (if (not (looking-at "^>* "))
	      (setq fillpre "")
	    (search-forward-regexp "\\(>* \\)*[^>]")
	    (search-backward-regexp "> ")
	    (forward-char 2)
	    (setq fillpre (buffer-substring fillstart (point))))
	  (fill-region (point-at-bol) (point-at-eol))
	  (setq fillend (point))
	  (save-excursion
	    (goto-char fillstart)
	    (forward-line 1)
	    (while (< (point) fillend)
	      (insert fillpre)
	      (forward-line 1))
	    (forward-line -1)))
	(forward-line 1)))))

(defun akmuch-view-mime ()
  (interactive)
  (akmuch-view akmuch-current-message-id 'list))

(defun akmuch-view-original ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Akmuch Original*")
    (erase-buffer)
    (call-process "notmuch" nil (current-buffer) nil
		  "show" "--format=raw" akmuch-current-message-id))
  (pop-to-buffer "*Akmuch Original*")
  (goto-char (point-min)))

(defun akmuch-view-message ()
  (interactive)
  (let ((thread (akmuch-get-threadid))
	(buffer-read-only nil))
    (akmuch-view thread)))

(defun akmuch-colorize-message ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (when (or (looking-at "^>\\( \\|$\\)")
		(looking-at "^On .* wrote:$"))
	(put-text-property (point) (point-at-eol)
			   'face 'font-lock-warning-face))
      (forward-line))))

(provide 'akmuch-message)
