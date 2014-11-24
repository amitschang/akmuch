(defvar akmuch-search '("tag:inbox"))
(defvar akmuch-search-history '())
(defvar akmuch-last-position 0)
(defvar akmuch-last-window-start 0)
(defvar akmuch-last-position-t 0)
(defvar akmuch-last-window-start-t 0)
(defvar akmuch-display-type 'search)
(defvar akmuch-quit-to 'search)
(defvar akmuch-current-thread-id)
(defvar akmuch-current-message-id)
(defvar akmuch-mail-indicator '(""))
(defcustom akmuch-subject-display-length 70
  "number of characters to display subject")
(defcustom akmuch-default-dir "~/"
  "The default directory for writing attachments to")
(defcustom akmuch-long-line 100
  "What should be considered long line for filling mail/reply")
(defcustom akmuch-summary-terms
  '(("inbox" . "tag:inbox")
    ("unread" . "tag:unread")
    ("recent" . "date:-1d.."))
  "Example summary terms")

(defvar akmuch-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") 'akmuch-view-latest)
	(define-key map (kbd "n") 'akmuch-next)
	(define-key map (kbd "p") 'akmuch-prev)
	(define-key map (kbd "q") 'akmuch-quit)
	(define-key map (kbd "g") 'akmuch-refresh)
	(define-key map (kbd "s") 'akmuch-search)
	(define-key map (kbd "S") 'akmuch-add-search)
	(define-key map (kbd "i") 'akmuch-inbox)
	(define-key map (kbd "u") 'akmuch-unread)
	(define-key map (kbd "r") 'akmuch-reply)
	(define-key map (kbd "R") 'akmuch-reply-sender)
	(define-key map (kbd "f") 'akmuch-forward)
	(define-key map (kbd "d") 'akmuch-delete)
	(define-key map (kbd "t") 'akmuch-tag)
	(define-key map (kbd "T") 'akmuch-tag-marked)
	(define-key map (kbd "l") 'akmuch-thread-list)
	(define-key map (kbd "M") 'akmuch-view-mime)
	(define-key map (kbd "|") 'akmuch-pipe-part)
	(define-key map (kbd "SPACE") 'akmuch-mark)
	(define-key map (kbd "m") 'akmuch-mark-move)
	(define-key map (kbd "U") 'akmuch-unmark-all)
	map))

(defun akmuch-format-search-result ()
  (let (sstart)
    (goto-char (point-at-bol))
    (forward-char 41)
    (search-forward-regexp " ")
    (insert "[")
    (search-forward-regexp ";")
    (delete-char -1)
    (while (> (current-column) 65)
      (delete-char -1))
    (while (< (current-column) 65)
      (insert " "))
    (insert "] --- ")
    (goto-char (point-at-eol))
    (search-backward-regexp " (")
    (while (> (current-column) (+ 65 akmuch-subject-display-length))
      (delete-char -1))
    (while (< (current-column) (+ 65 akmuch-subject-display-length))
      (insert " "))
    (save-excursion
    (when (search-forward-regexp "*" (point-at-eol) t)
      (delete-char -1)
      (move-to-column 24)
      (delete-char 1)
      (insert "*"))
    (when (search-forward-regexp "attachment" (point-at-eol) t)
      (delete-char -10)
      (move-to-column 69)
      (delete-char 1)
      (insert "&"))
    (when (search-forward-regexp "replied" (point-at-eol) t)
      (delete-char -7)
      (move-to-column 68)
      (delete-char 1)
      (insert "R"))
    (when (search-forward-regexp "unread" (point-at-eol) t)
      (delete-char -6)
      (move-to-column 67)
      (delete-char 1)
      (insert "U")))
    (when (re-search-forward "( *" (point-at-eol) t)
      (replace-match "(" nil nil))
    (when (re-search-forward "\\(  *\\)[^)]" (point-at-eol) t)
      (replace-match " " nil nil nil 1))
    (when (re-search-forward " *)" (point-at-eol) t)
      (replace-match ")" nil nil))
    ))

(defun akmuch-fontify-search-result (bold)
  (let (sstart)
    (goto-char (point-at-bol))
    (set-text-properties (point-at-bol) (point-at-eol) nil)
    (search-forward-regexp " ")
    (put-text-property (point-at-bol) (- (point) 1) 'invisible t)
    (when (looking-at "*")
      (put-text-property (point) (+ 1 (point)) 'face 'font-lock-warning-face))
    (search-forward-regexp "\\] \\[")
    (put-text-property
     (point)
     (progn (search-forward-regexp "]")
	    (- (point) 1))
     'face 'font-lock-keyword-face)
    (forward-char 1)
    (when (looking-at "U")
      (put-text-property (point) (+ 1 (point)) 'face 'font-lock-warning-face))
    (forward-char 1)
    (when (looking-at "R")
      (put-text-property (point) (+ 1 (point)) 'face 'font-lock-string-face))
    (goto-char (point-at-eol))
    (search-backward-regexp " (")
    (put-text-property (point) (point-at-eol) 'face 'font-lock-comment-face)))

(defun akmuch-display-search ()
  (interactive)
  (let ((buffer-read-only nil) sstart)
    (setq akmuch-display-type 'search)
    (erase-buffer)
    (eval (append (list 'call-process "notmuch" nil (current-buffer) nil
			"search")
		  akmuch-search))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (akmuch-format-search-result)
      (akmuch-fontify-search-result nil)
      (forward-line 1))
      (setq header-line-format
	    (concat (number-to-string (- (line-number-at-pos) 1))
		    " results for search '"
		    (mapconcat 'identity akmuch-search " ")
		    "'"))
      (unless (eq (point-min) (point-max))
	(delete-char -1)
	(goto-char (point-min))))
  (akmuch-enable-following))

(defun akmuch-quit ()
  (interactive)
  (if (eq akmuch-quit-to 'threadlist)
      (progn (akmuch-thread-list t)
	     (setq akmuch-quit-to 'search))
    (akmuch-display-search))
  (akmuch-restore-state))

(defun akmuch-save-state ()
  (cond 
    ((eq akmuch-display-type 'search)
     (setq akmuch-last-position (line-number-at-pos (point)))
     (setq akmuch-last-window-start (window-start)))
    ((eq akmuch-display-type 'threadlist)
     (setq akmuch-last-position-t (line-number-at-pos (point)))
     (setq akmuch-last-window-start-t (window-start)))))

(defun akmuch-restore-state ()
  (let ((wstart akmuch-last-window-start)
	(lastpos akmuch-last-position))
    (when (eq akmuch-display-type 'threadlist)
       (setq wstart akmuch-last-window-start-t)
       (setq lastpos akmuch-last-position-t))
    (set-window-start nil wstart)
    (akmuch-next (- lastpos 1))
    (unless (pos-visible-in-window-p (point))
      (recenter -2))))

(defun akmuch-refresh ()
  (interactive)
  (akmuch-save-state)
  (cond ((eq akmuch-display-type 'search)
	 (akmuch-display-search))
	((eq akmuch-display-type 'threadlist)
	 (akmuch-display-threadlist))
	((eq akmuch-display-type 'message)
	 (akmuch-display-message)))
  (akmuch-restore-state))

(defun akmuch-next (&optional num)
  (interactive "p")
  (let ((buffer-read-only nil))
    (if (eq akmuch-display-type 'message)
	(akmuch-next-message num)
      (akmuch-next-line num))
    (akmuch-get-threadid)))

(defun akmuch-prev (&optional num)
  (interactive "p")
  (akmuch-next (* -1 num)))

(defun akmuch-next-line (&optional num)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^[^ ]+>" nil t)
      (delete-char -1)
      (insert " ")
      (goto-char (point-at-eol))
      (delete-char -4)
      (when (eq akmuch-display-type 'search)
	  (akmuch-fontify-search-result nil))))
  (forward-line num)
  (when (search-forward-regexp " " nil t)
    (delete-char -1)
    (insert (propertize ">" 'face 'font-lock-type-face))
    (goto-char (point-at-eol))
    (insert (propertize "<===" 'face 'font-lock-type-face))
    (goto-char (point-at-bol))
    (add-text-properties (point-at-bol) (point-at-eol) '(face bold))
    (when (eq akmuch-display-type 'threadlist)
      (message "%s" (akmuch-preview-message-text (akmuch-get-threadid))))
    (akmuch-save-state)))

(defun akmuch-next-message (&optional num)
  (let (start theid)
    (with-temp-buffer
      (call-process "notmuch" nil (current-buffer) nil
		    "show" "--entire-thread" akmuch-current-thread-id)
      (goto-char (point-min))
      (search-forward-regexp
       (concat "\^Lmessage{ "
	       (regexp-quote akmuch-current-message-id)))
      (when
	  (if (< num 0)
	      (search-backward-regexp "\^Lmessage{ " nil t (+ (* -1 num) 1))
	    (search-forward-regexp "\^Lmessage{ " nil t num))
	(setq start (point-at-bol)))
      (if (null start)
	  (message "No more messages")
	(goto-char (point-at-bol))
	(search-forward-regexp " ")
	(setq start (point))
	(search-forward-regexp " ")
	(setq theid (buffer-substring start (- (point) 1)))))
    (when theid
      (akmuch-view theid))))
  
(defun akmuch-highlight-current ()
  (when (not (eq akmuch-display-type 'message))
    (akmuch-next 0)))

(defun akmuch-get-threadid ()
  (if (eq akmuch-display-type 'message)
      akmuch-current-thread-id
    (save-excursion
      (goto-char (point-at-bol))
      (when (search-forward-regexp "[ >]" nil t)
	(let ((thread
	       (buffer-substring-no-properties
		(point-at-bol) (- (point) 1))))
	  (when (eq akmuch-display-type 'search)
	    (setq akmuch-current-thread-id thread))
	  thread)))))

(defun akmuch-pipe-part (command)
  (interactive (list (read-shell-command "Command: ")))
  (let ((pt (point))
	n)
    (if (looking-at "[0-9]")
	(save-excursion
	  (search-forward-regexp " ")
	  (setq n (string-to-number (buffer-substring pt (point)))))
      (setq n (read-number "Part number: ")))
    (shell-command (format "notmuch show --part=%d '%s' | %s"
			   n akmuch-current-message-id command))))

(defun akmuch-view-mime ()
  (interactive)
  (akmuch-view akmuch-current-message-id 'list))

(defun akmuch-view (id &optional type)
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
				'face font-lock-comment-face)))
	(with-current-buffer buff
	  (insert tmp)
	  (insert "\n")))
      (with-current-buffer buff (insert "\n"))
      ;;
      ;; Now go through the message and format the output
      ;;
      (akmuch-parse-message-contents (point) theend buff type))
    (akmuch-fill-message)
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
	    (w3m-region start (point))
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

(defun akmuch-get-latest-message-id ()
  (let ((thread (akmuch-get-threadid)))
    (with-temp-buffer
      (call-process "notmuch" nil (current-buffer) nil
		    "search" "--output=messages" thread
		    "tag:unread")
      (when (eq (point) (point-min))
	(call-process "notmuch" nil (current-buffer) nil
		      "search" "--output=messages" thread))
      (goto-char (point-min))
      (buffer-substring (point) (point-at-eol)))))

(defun akmuch-view-latest ()
  (interactive)
  (akmuch-next 0)
  (let ((mid (akmuch-get-latest-message-id)))
    (pop-to-buffer "*akmuch message*")
    (akmuch-view mid)))

(defun akmuch-thread-list (&optional nomove)
  (interactive)
  (let ((thread (akmuch-get-threadid))
	(buffer-read-only nil)
	subj depth id start)
    (message "got thread id %s, display type %s" thread akmuch-display-type)
    (setq akmuch-display-type 'threadlist)
    (erase-buffer)
    (call-process "notmuch" nil (current-buffer) nil
		  "show" thread)
    (goto-char (point-min))
    (search-forward-regexp "Subject: ")
    (setq header-line-format (buffer-substring (point) (point-at-eol)))
    ;; (setq subj (buffer-substring (point) (point-at-eol)))
    (goto-char (point-min))
    ;; (insert (concat subj "\n"))
    (setq start (point))
    (while (search-forward-regexp "^\^Lmessage{ " nil t)
      (setq id (buffer-substring
		(point)
		(progn (search-forward-regexp " ") (- (point) 1))))
      (forward-char 6)
      (setq depth (string-to-number (buffer-substring (point) (+ 2 (point)))))
      (forward-line 2)
      (delete-region start (point))
      (insert (propertize id 'invisible t))
      (insert (concat " " (make-string (* depth 2) 32)))
      (forward-line 1)
      (setq start (point)))
    (delete-region (- (point) 1) (point-max))
    (goto-char (point-min))
    (unless nomove
      (if (not (search-forward-regexp "unread[^)]*)$" nil t))
	  (goto-char (point-max)))
      (akmuch-next 0))))

(defun akmuch-preview-message-text (id)
  (with-temp-buffer
    (call-process "notmuch" nil (current-buffer) nil
		  "show" id)
    (goto-char (point-min))
    (search-forward-regexp "^\^Lbody{")
    (search-forward-regexp "^[^\^L]")
    (let ((start (- (point) 1))
	  end)
      (search-forward-regexp "^\^L")
      (if (< (- (- (point) 1) start) 60)
	  (setq end (- (point) 1))
	(setq end (+ start 60)))
      (concat
       (replace-regexp-in-string
	"\n" "..."
	(buffer-substring start end))
       "..."))))

(defun akmuch-enumerate-attachments ()
  (let ((id (if (eq akmuch-display-type 'message)
		akmuch-current-message-id
	      (akmuch-get-threadid)))
	(list '())
	tmp)
    (with-temp-buffer
      (call-process "notmuch" nil (current-buffer) nil
		    "show" id)
      (goto-char (point-min))
      (while (search-forward-regexp "^\^Lattachment{ " nil t)
	(setq tmp (list (save-excursion
			  (buffer-substring
			   (progn
			     (search-backward-regexp "^\^Lmessage{ ")
			     (search-forward " ")
			     (point))
			   (progn
			     (search-forward-regexp " ")
			     (- (point) 1))))))
	(search-forward-regexp "ID: ")
	(setq tmp
	      (append
	       tmp
	       (list (buffer-substring
		      (point)
		      (progn (search-forward ",") (- (point) 1))))))
	(search-forward-regexp "Filename: ")
	(setq tmp
	      (append
	       tmp
	       (list (replace-regexp-in-string
		      " " "_"
		      (buffer-substring
		       (point)
		       (progn (search-forward ",") (- (point) 1)))))))
    	(search-forward-regexp "Content-type: ")
	(setq tmp (append tmp (list (buffer-substring
				     (point)
				     (point-at-eol)))))
	(setq list (append list (list tmp)))))
    list))

(defun akmuch-download-attachment (&optional number dir name)
  (interactive)
  (let ((att (akmuch-enumerate-attachments)))
    (if (= (length att) 0)
	(message "Message has no attachments")
      (when (interactive-p)
	(setq number
	      (read-string (format "Att number [%d]: " (length att))))
	(setq dir (read-directory-name
		   (format "Save to [%s]: " akmuch-default-dir)))
	(if (string-match "," number)
	    (setq number (map 'list 'string-to-number (split-string number ",")))
	  (if (string-equal number "*")
	      (setq number (number-sequence 1 (length att)))
	    (setq number (list (string-to-number number)))))
	(while number
	  (with-temp-buffer
	    (let ((coding-system-for-read 'no-conversion)
		  (coding-system-for-write 'no-conversion))
	      (setq buffer-file-coding-system nil)
	      (call-process "notmuch" nil (current-buffer) nil
			    "show"
			    (concat "--part=" (nth 1 (nth (- (car number) 1) att)))
			    (nth 0 (nth (- (car number) 1) att)))
	      (setq default-directory dir)
	      (write-region nil nil (nth 2 (nth (- (car number) 1) att)))))
	  (setq number (cdr number)))))))

(defun akmuch-search (search)
  (interactive "sSearch: ")
  (setq akmuch-search (list search))
  (setq akmuch-search-history
	(append akmuch-search-history akmuch-search))
  (akmuch-display-search)
  (akmuch-next 0))

(defun akmuch-add-search (search)
  (interactive "sAdd to search: ")
  (akmuch-search (concat (car akmuch-search) " " search)))

(defun akmuch-inbox ()
  (interactive)
  (setq akmuch-search '("tag:inbox"))
  (akmuch-display-search)
  (akmuch-next 0))

(defun akmuch-unread ()
  (interactive)
  (setq akmuch-search '("tag:unread"))
  (akmuch-display-search)
  (akmuch-next 0))

(defun akmuch-summarize ()
  (interactive)
  (when (not (listp akmuch-summary-terms))
    (message "no summary terms registered"))
  (setq akmuch-display-type 'summary)
  (let ((terms akmuch-summary-terms)
	(buffer-read-only nil)
	psave)
    (erase-buffer)
    (while terms
      (insert (cdr (car terms)))
      (insert "\n")
      (setq terms (cdr terms)))
    (shell-command-on-region
     (point-min) (point-max)
     "notmuch count --batch"
     (current-buffer) t)
    (goto-char (point-min))
    (setq terms akmuch-summary-terms)
    (while terms
      (insert (format "%-30s" (car (car terms))))
      (forward-line 1)
      (setq terms (cdr terms)))))
    ;; (if (> (string-to-number (buffer-substring (point) (point-at-eol))) 40)
    ;; 	  (put-text-property (point) (point-at-eol) 'face 'font-lock-warning-face))

(defun akmuch-delete (&optional from-here)
  (interactive "P")
  (let ((tid (akmuch-get-threadid)))
    (if (and from-here (or (eq akmuch-display-type 'search)
			   (eq akmuch-display-type 'threadlist)))
	(save-excursion
	  (while (< (point) (point-max))
	    (forward-line 1)
	    (setq tid (concat tid " " (akmuch-get-threadid))))))
    (call-process "notmuch" nil nil nil "tag"
		  "-inbox" "-unread" tid)
    (akmuch-quit)))

(defun akmuch-tag (tags &optional marked)
  (interactive "sTags: ")
  (let ((tag-list (split-string tags " "))
	arg-list ident buff (current-buffer) tbuff)
    (if marked
	(setq ident "tag:*")
      (setq ident (akmuch-get-threadid)))
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
      (akmuch-fill-message)
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

(defun akmuch-check-unread-timer ()
  (when (get-buffer "mail")
    (with-current-buffer "mail"
      (when (string-equal mode-name "Akmuch")
	(let (unread marked total)
	  (with-temp-buffer
	    (insert "tag:unread\ntag:*\n*")
	    (shell-command-on-region
	     (point-min) (point-max)
	     "notmuch count --batch --output=threads"
	     (current-buffer) t)
	    (goto-char (point-min))
	    (setq unread (buffer-substring (point) (point-at-eol)))
	    (forward-line)
	    (setq marked (buffer-substring (point) (point-at-eol)))
	    (forward-line)
	    (setq total (buffer-substring (point) (point-at-eol))))
	  (setq mode-line-buffer-identification
		(format "%s (U:%s M:%s T:%s)" (buffer-name) unread marked total))
	  (if (> unread 0)
	      (setq akmuch-mail-indicator (list (format " ✉ (%d)" unread)))
	    (setq akmuch-mail-indicator (list ""))))))))

(defun akmuch-disable-following ()
  (setq post-command-hook nil))

(defun akmuch-enable-following ()
  (setq post-command-hook 'akmuch-highlight-current))

(defun akmuch ()
  (interactive)
  (switch-to-buffer "mail")
  (akmuch-mode)
  (akmuch-display-search))

(define-derived-mode akmuch-mode nil "Akmuch"
  "Major mode for mail using notmuch"
  (run-with-timer 2 2 'akmuch-check-unread-timer)
  (set (make-local-variable 'face-remapping-alist) '((header-line mode-line)))
  (set (make-local-variable 'post-command-hook) 'akmuch-highlight-current)
  (set (make-local-variable 'isearch-mode-hook) 'akmuch-disable-following)
  (set (make-local-variable 'isearch-mode-end-hook) 'akmuch-enable-following)
  (or global-mode-string (setq global-mode-string '("")))
  (add-to-list 'global-mode-string 'akmuch-mail-indicator t)
  (setq header-line "Akmuch mail")
  (setq buffer-read-only t)
  (toggle-truncate-lines t))