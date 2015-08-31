(require 'akmuch-search)

(defvar akmuch-seen-whole-message nil)
(defvar akmuch-message-filename nil)
(defvar akmuch-message-mode nil)
(defcustom akmuch-notmuch-helper "akmuch-helper"
  "akmuch view helper program")

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

(defun akmuch-view (&optional type)
  ;;
  ;; View a message in the message buffer
  ;;
  (interactive)
  (let ((mid (akmuch-get-thread-id))
	(sbuff akmuch-search-buffer)
	file)
    (unless (buffer-live-p akmuch-message-buffer)
      (setq akmuch-message-buffer
	    (get-buffer-create "*akmuch message*")))
    ;; prepare a list of message files
    (akmuch-message-prep mid)
    (akmuch-message-view mid t nil type)))

(defun akmuch-message-view (mid any-unread show-all type)
  (let (file)
  ;; see if there is unread one to view
    (if show-all
	(setq file (with-current-buffer " *akmuch message files*"
		     (buffer-substring (point-min) (point-max))))
      (when any-unread
	(setq file (akmuch-message-latest-unread mid))
	(when (not (string= file ""))
	  (akmuch-message-set-filename file)))
      (when (or (not file) (string= file ""))
	(setq file (akmuch-message-get-filename))))
    (if (string= file "")
	(message "sorry could not find filename...")
      (with-current-buffer akmuch-message-buffer
	(akmuch-message-mode)
	(akmuch-message-view-file file type)
        (when linum-mode
          (linum-update (current-buffer)))
	(setq akmuch-search-buffer akmuch-search-buffer)
	(setq akmuch-message-filename file)
	(setq akmuch-message-mode type)
	(setq akmuch-message-id mid))
      (display-buffer akmuch-message-buffer))))

(defun akmuch-message-next ()
  (interactive)
  (with-current-buffer " *akmuch message files*"
    (goto-char (point-min))
    (when (search-forward-regexp "^ " nil t)
      (delete-char -1))
    (forward-line)
    (if (not (looking-at "$"))
	(insert " ")
      (forward-line -1)
      (insert " ")))
  (akmuch-message-view nil nil nil 'view))

(defun akmuch-message-prev ()
  (interactive)
  (with-current-buffer " *akmuch message files*"
    (goto-char (point-min))
    (when (search-forward-regexp "^ " nil t)
      (delete-char -1))
    (forward-line -1)
    (insert " "))
  (akmuch-message-view nil nil nil 'view))

(defun akmuch-page ()
  ;;
  ;; Page through messages
  ;;
  (interactive)
  (let ((buffer akmuch-message-buffer)
	(window (car (window-list))))
    (display-buffer buffer)
    (set-buffer buffer)
    (if akmuch-seen-whole-message
	(akmuch-search-next 1)
      (select-window (get-buffer-window buffer))
      (condition-case nil
	  (save-excursion
	    (scroll-up)
	    (when (pos-visible-in-window-p (point-max))
	      (setq akmuch-seen-whole-message t)
	      (goto-char (point-max))
	      (recenter-top-bottom -2)
	      (message "End of message")))
	(end-of-buffer
	 ;; (akmuch-tag "-unread")
	 (setq akmuch-seen-whole-message t))))
    (select-window window)))

(defun akmuch-message-get-filename ()
  (with-current-buffer " *akmuch message files*"
    (goto-char (point-min))
    (search-forward-regexp "^ " nil t)
    (buffer-substring (point) (point-at-eol))))

(defun akmuch-message-set-filename (file)
  (with-current-buffer " *akmuch message files*"
    (goto-char (point-min))
    (search-forward-regexp "^ " nil t)
    (delete-char -1)
    (goto-char (point-min))
    (search-forward-regexp file)
    (goto-char (point-at-bol))
    (insert " ")))

(defun akmuch-message-prep (id)
  ;; get the list of filenames for the message
  (with-current-buffer
      (get-buffer-create " *akmuch message files*")
    (erase-buffer)
    (call-process "notmuch" nil (current-buffer) nil
		  "search" "--output=files"
		  "--sort=oldest-first"
		  id)
    (goto-char (point-max))
    (forward-line -1)
    (insert " ")))

(defun akmuch-message-latest-unread (id)
  (with-temp-buffer
    (call-process "notmuch" nil (current-buffer) nil
		  "search" "--output=files"
		  "--sort=oldest-first"
		  "--limit=1"
		  "tag:unread" id)
    (goto-char (point-min))
    (search-forward-regexp "$")
    (buffer-substring (point-at-bol) (point))))

(defun akmuch-message-view-file (file &optional type)
  (let ((buffer-read-only nil)
	(helper-command "view"))
    ;; find the message of thread we should read. The oldest unread or
    ;; the most recent message
    ;; now call our shower helper
    (erase-buffer)
    (if (eq type 'mime)
	(setq helper-command "mime"))
    (if (eq type 'attach)
	(setq helper-command "attach"))
    (save-excursion
      (eval (append
	     (list
	      'call-process
	      akmuch-notmuch-helper
	      nil (current-buffer) nil
	      helper-command)
	     (split-string file))))
    ;; make header line out of subject
    (setq header-line-format
	  (format
	   "%s %s"
	   (akmuch-message-get-thread-pos)
	   (buffer-substring
	    (point-at-bol)
	    (point-at-eol))))
    (delete-region (point-at-bol)
		   (+ 1 (point-at-eol)))
    (akmuch-colorize-message)))

(defun akmuch-message-get-thread-pos ()
  (let (n tot)
    (with-current-buffer " *akmuch message files*"
      (goto-char (point-min))
      (search-forward-regexp "^ ")
      (setq n (line-number-at-pos))
      (setq tot (count-matches "^." (point-min) (point-max))))
    (format "[%d/%d]" n tot)))

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
  (akmuch-message-view nil nil nil 'mime))

(defun akmuch-message-show-attachments-all ()
  (interactive)
  (akmuch-message-view nil nil t 'attach))

(defun akmuch-select-part ()
  (let ((mode (with-current-buffer akmuch-message-buffer
		akmuch-message-mode)))
    (when (not (or (eq mode 'mime) (eq mode 'attach)))
      (akmuch-view-mime))
    (let ((pt (point))
	  n)
      (if (looking-at "[0-9]")
	(save-excursion
	  (search-forward-regexp " ")
	  (setq n (string-to-number (buffer-substring pt (point)))))
      (setq n (read-number "Part number: ")))
    n)))

(defun akmuch-pipe-part (&optional command)
  (interactive)
  (let ((part (akmuch-select-part))
	file)
    (unless command
      (setq command (read-shell-command "[pipe] command: ")))
    (setq file (with-current-buffer akmuch-message-buffer
		 akmuch-message-filename))
    (shell-command
     (format "%s part %d %s | %s"
	     akmuch-notmuch-helper part file command))))

(defun akmuch-command-part (&optional command)
  (interactive)
  (let ((part (akmuch-select-part))
	file)
    (unless command
      (setq command (read-shell-command "[async] command: ")))
    (setq file
          (replace-regexp-in-string
           "\n" " "
           (with-current-buffer akmuch-message-buffer
             akmuch-message-filename)))
    (setq filename (make-temp-file "/tmp/akmuch_attachment_"))
    (shell-command
     (format "%s part %d %s > %s"
	     akmuch-notmuch-helper part file filename))
    (start-process-shell-command "akmuch-att-command" nil
		   (format "%s %s; rm %s" command filename filename))))

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
    (while (not (looking-at "\n"))
      (put-text-property (point) (point-at-eol)
			 'face 'font-lock-string-face)
      (forward-line))
    (while (< (point) (point-max))
      (when (or (looking-at "^>>*\\( \\|$\\)")
		(looking-at "^On .* wrote:$"))
	(put-text-property (point) (point-at-eol)
			   'face 'font-lock-warning-face))
      (forward-line))))

(defvar akmuch-message-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "q")   'kill-buffer-and-window)
	(define-key map (kbd "o")   'other-window)
	map))

(define-derived-mode akmuch-message-mode nil "Akmuch [message]"
  "Major mode for mail using notmuch (message buffers)"
  (set (make-local-variable 'akmuch-seen-whole-message) nil)
  (set (make-local-variable 'akmuch-mime-viewing-all) nil)
  (set (make-local-variable 'akmuch-message-filename) nil)
  (set (make-local-variable 'akmuch-message-mode) nil)
  (set (make-local-variable 'akmuch-message-id) nil)
  (make-local-variable 'akmuch-search-buffer)
  (setq header-line "akmuch message")
  (setq buffer-read-only t))

(provide 'akmuch-message)
