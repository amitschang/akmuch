(defvar akmuch-summarize-buffer "*mail summary*")

(defun akmuch-summarize ()
  (interactive)
  (let ((terms akmuch-summary-terms)
	(sbuff akmuch-search-buffer)
	psave)
    (when (not (listp akmuch-summary-terms))
      (message "no summary terms registered"))
    (pop-to-buffer akmuch-summarize-buffer)
    (setq buffer-read-only nil)
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
      (when (> (string-to-number
	      (buffer-substring (point) (point-at-eol))) 40)
    	  (put-text-property (point) (point-at-eol)
			     'face 'font-lock-warning-face))
      (insert (format "%-15s" (car (car terms))))
      (forward-line 1)
      (setq terms (cdr terms)))
    (goto-char (point-min))
    (akmuch-summary-mode)
    (setq akmuch-search-buffer sbuff)
    ))

(defun akmuch-summary-get-terms ()
  (let (index terms i)
    (save-excursion
      (goto-char (point-at-bol))
      (search-forward-regexp " ")
      (forward-char -1)
      (setq index (buffer-substring
		   (point-at-bol)
		   (point)))
      (setq i akmuch-summary-terms)
      (while i
	(when (string= index (caar i))
	  (setq terms (cdar i)))
	(setq i (cdr i))))
    terms)
  )

(defun akmuch-summary-search ()
  (interactive)
  (let ((terms (akmuch-summary-get-terms)))
    (when (and terms akmuch-search-buffer)
      (pop-to-buffer akmuch-search-buffer)
      (akmuch-search terms))))

(defun akmuch-summary-next ()
  (interactive)
  (forward-line 1))

(defun akmuch-summary-prev ()
  (interactive)
  (forward-line -1))

(defvar akmuch-summary-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") 'akmuch-summary-search)
	(define-key map (kbd "n")   'akmuch-summary-next)
	(define-key map (kbd "p")   'akmuch-summary-prev)
	(define-key map (kbd "t")   'akmuch-summary-tag)
	(define-key map (kbd "o")   'other-window)
	map))

(define-derived-mode akmuch-summary-mode nil "Akmuch [summary]"
  "Major mode for mail using notmuch (message buffers)"
  (make-local-variable 'akmuch-search-buffer)
  (setq buffer-read-only t))

(provide 'akmuch-summarize)
