(require 'akmuch-search)
(require 'akmuch-message)
(require 'akmuch-reply)
(require 'akmuch-summarize)
(require 'akmuch-tag)

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
(defcustom akmuch-fill-messages t
  "fill messages in view and reply buffers?")
(defcustom akmuch-long-line 100
  "What should be considered long line for filling mail/reply")
(defcustom akmuch-summary-terms
  '(("inbox" . "tag:inbox")
    ("unread" . "tag:unread")
    ("recent" . "date:-1d.."))
  "Example summary terms")

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


(defun akmuch-check-unread-timer ()
  (when (get-buffer "mail")
    (with-current-buffer "mail"
      (when (string-equal mode-name "Akmuch")
	(let (unread marked total)
	  (with-temp-buffer
	    (insert "tag:unread\ntag:*\n*")
	    (shell-command-on-region
	     (point-min) (point-max)
	     "notmuch count --batch"
	     (current-buffer) t)
	    (goto-char (point-min))
	    (setq unread (buffer-substring (point) (point-at-eol)))
	    (forward-line)
	    (setq marked (buffer-substring (point) (point-at-eol)))
	    (forward-line)
	    (setq total (buffer-substring (point) (point-at-eol))))
	  (setq mode-line-buffer-identification
		(format "%s (U:%s M:%s T:%s)" (buffer-name) unread marked total))
	  (setq unread (string-to-number unread))
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
  (akmuch-search-mode)
  (akmuch-display-search))

