(defcustom akmuch-search-limit 100
  "Default limit to the number of threads to display in search page")
(defvar akmuch-search-offset 0)
(defvar akmuch-search-buffer nil)
(defvar akmuch-message-buffer nil)
(defvar akmuch-current-thread-id nil)
(defvar akmuch-search-sorting "newest-first")

(defvar akmuch-search-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") 'akmuch-view)
	(define-key map (kbd "n")   'akmuch-search-next)
	(define-key map (kbd "p")   'akmuch-search-prev)
	(define-key map (kbd "N")   'akmuch-message-next)
	(define-key map (kbd "P")   'akmuch-message-prev)
	(define-key map (kbd "g")   'akmuch-refresh)
	(define-key map (kbd "s")   'akmuch-search)
	(define-key map (kbd "S")   'akmuch-add-search)
	(define-key map (kbd "i")   'akmuch-inbox)
	(define-key map (kbd "u")   'akmuch-unread)
	(define-key map (kbd "r")   'akmuch-reply)
	(define-key map (kbd "R")   'akmuch-reply-sender)
	(define-key map (kbd "f")   'akmuch-forward)
	(define-key map (kbd "d")   'akmuch-delete)
	(define-key map (kbd "t")   'akmuch-tag)
	(define-key map (kbd "T")   'akmuch-tag-marked)
	(define-key map (kbd "l")   'akmuch-thread-list)
	(define-key map (kbd "A")   'akmuch-message-show-attachments-all)
	(define-key map (kbd "M")   'akmuch-view-mime)
	(define-key map (kbd "o")   'other-window)
	(define-key map (kbd "O")   'akmuch-view-original)
	(define-key map (kbd "|")   'akmuch-pipe-part)
	(define-key map (kbd "!")   'akmuch-command-part)
	(define-key map (kbd "SPC") 'akmuch-page)
	(define-key map (kbd "]")   'akmuch-search-page-next)
	(define-key map (kbd "[")   'akmuch-search-page-prev)
	(define-key map (kbd "m")   'akmuch-mark-move)
	(define-key map (kbd "U")   'akmuch-unmark-all)
	(define-key map (kbd "<")   'scroll-right)
	(define-key map (kbd ">")   'scroll-left)
	(define-key map (kbd "'")   'akmuch-reverse-order)
	map))

(defun akmuch-refresh ()
  (interactive)
  (with-current-buffer akmuch-search-buffer
    (let ((tid (akmuch-get-thread-id))
	  (lin (line-number-at-pos)))
      (akmuch-display-search)
      (unless (search-forward-regexp (concat "^" tid) nil t)
	(forward-line lin))
      (akmuch-search-next 0))
    (if (get-buffer akmuch-summarize-buffer)
        (akmuch-summary-update))))

(defun akmuch-search (search)
  (interactive "sSearch: ")
  (setq akmuch-search-offset 0)
  (setq akmuch-search (list search))
  (setq akmuch-search-history
	(append akmuch-search-history akmuch-search))
  (akmuch-display-search)
  (akmuch-next 0)
  (if (get-buffer akmuch-summarize-buffer)
      (akmuch-summary-update)))

(defun akmuch-add-search (search)
  (interactive "sAdd to search: ")
  (akmuch-search (concat (car akmuch-search) " " search)))

(defun akmuch-inbox ()
  (interactive)
  (setq akmuch-search '("tag:inbox"))
  (akmuch-search (car akmuch-search))
  (akmuch-next 0))

(defun akmuch-unread ()
  (interactive)
  (setq akmuch-search '("tag:unread"))
  (akmuch-search (car akmuch-search))
  (akmuch-next 0))

(defun akmuch-reverse-order ()
  (interactive)
  (if (string-equal akmuch-search-sorting "newest-first")
      (setq akmuch-search-sorting "oldest-first")
    (setq akmuch-search-sorting "newest-first"))
  (akmuch-refresh))

(defun akmuch-search-page-next ()
  (interactive)
  (setq akmuch-search-offset (+ akmuch-search-limit akmuch-search-offset))
  (akmuch-refresh))

(defun akmuch-search-page-prev ()
  (interactive)
  (setq akmuch-search-offset (- akmuch-search-offset akmuch-search-limit))
  (akmuch-refresh))

(defun akmuch-format-search-result ()
  ;;
  ;; Take notmuch search results and make them look pretty
  ;; Includes hiding the thread id and taking standard tags and
  ;; putting them catchy spots
  ;;
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
    (forward-char 1)
    (when (looking-at "&")
      (put-text-property (point) (+ 1 (point)) 'face 'font-lock-variable-name-face))
    (goto-char (point-at-eol))
    (search-backward-regexp " (")
    (put-text-property (point) (point-at-eol) 'face 'font-lock-comment-face)))

(defun akmuch-display-search ()
  (interactive)
  (let ((buffer-read-only nil) sstart)
    (setq akmuch-display-type 'search)
    (erase-buffer)
    (eval (append
	   (list 'call-process
		 "notmuch"
		 nil (current-buffer) nil
		 "search"
		 (format "--sort=%s" akmuch-search-sorting)
		 (format "--limit=%d" akmuch-search-limit)
		 (format "--offset=%d" akmuch-search-offset))
	   akmuch-search))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (akmuch-format-search-result)
      (akmuch-fontify-search-result nil)
      (forward-line 1))
    (setq header-line-format
	  (format "+%d %d results for search '%s' - %s"
		  akmuch-search-offset
		  (- (line-number-at-pos) 1)
		  (mapconcat 'identity akmuch-search " ")
		  akmuch-search-sorting))
    (unless (eq (point-min) (point-max))
      (delete-char -1)
      (goto-char (point-min))))
  (akmuch-enable-following))

(defun akmuch-set-current-thread-id ()
  (save-excursion
    (goto-char (point-at-bol))
    (setq akmuch-current-thread-id
	  (buffer-substring-no-properties
	   (point-at-bol)
	   (+ (point-at-bol) 23)))))

(defun akmuch-search-prev (&optional num)
  (interactive "p")
  (akmuch-search-next (* -1 num)))

(defun akmuch-search-next (&optional num)
  (interactive "p")
  (with-current-buffer akmuch-search-buffer
    (let ((buffer-read-only nil))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward-regexp "^[^ ]+>" nil t)
	  (delete-char -1)
	  (insert " ")
	  (goto-char (point-at-eol))
	  (delete-char -4)
	  (akmuch-fontify-search-result nil)))
      (forward-line num)
      (goto-char (point-at-bol))
      (when (search-forward-regexp " " nil t)
	(delete-char -1)
	(insert (propertize ">" 'face 'font-lock-type-face))
	(goto-char (point-at-eol))
	(insert (propertize "<===" 'face 'font-lock-type-face))
	(goto-char (point-at-bol))
	(add-text-properties (point-at-bol) (point-at-eol) '(face bold))
	(when (eq akmuch-display-type 'threadlist)
	  (message "%s" (akmuch-preview-message-text (akmuch-get-threadid))))
	(akmuch-save-state))
      (akmuch-set-current-thread-id)
      (when (buffer-live-p akmuch-message-buffer)
	(akmuch-view)))))

(defun akmuch-highlight-current ()
  (let ((previous-thread-id akmuch-current-thread-id))
    (if (= (point-min) (point-max))
    	(message "no messages")
    (akmuch-set-current-thread-id)
    (unless (string-equal previous-thread-id
			akmuch-current-thread-id)
      (akmuch-search-next 0))))
  )

(defun akmuch-disable-following ()
  (setq post-command-hook nil))

(defun akmuch-enable-following ()
  (setq post-command-hook 'akmuch-highlight-current))

(define-derived-mode akmuch-search-mode nil "Akmuch [search]"
  "Major mode for mail using notmuch"
  (run-with-timer 2 2 'akmuch-check-unread-timer)
  (set (make-local-variable 'face-remapping-alist) '((header-line mode-line)))
  (set (make-local-variable 'post-command-hook) 'akmuch-highlight-current)
  (set (make-local-variable 'isearch-mode-hook) 'akmuch-disable-following)
  (set (make-local-variable 'isearch-mode-end-hook) 'akmuch-enable-following)
  (set (make-local-variable 'akmuch-search-buffer) (current-buffer))
  (make-local-variable 'akmuch-message-buffer)
  (or global-mode-string (setq global-mode-string '("")))
  (add-to-list 'global-mode-string 'akmuch-mail-indicator t)
  (setq header-line "Akmuch mail")
  (setq buffer-read-only t)
  (setq truncate-lines t))

(provide 'akmuch-search)
