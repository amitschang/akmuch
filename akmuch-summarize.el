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

(provide 'akmuch-summarize)
