;; org-lect.el -- flexible tracking for large childless tasks
;;    Copyright (C) 2016 Bruce Chiarelli


(defun org-lect-needed-effort-today (&optional pom)
  "Returns the number of units needed today to stay
on track"
  (interactive "P")
  (let ((curpage (string-to-number (nth 0 (org-entry-get-multivalued-property
							 (or pom (point)) "PAGES"))))
	(totpage (string-to-number (nth 1 (org-entry-get-multivalued-property
							 (or pom (point)) "PAGES"))))
	(deadl (org-time-stamp-to-now (format-time-string
				       (org-time-stamp-format nil t)
				       (org-get-deadline-time (or pom (point))))))
	)
    (if (org-entry-get (or pom (point)) "LECT_EFFORT") (* (/ (float (+ 1 (- totpage curpage )))
							     (org-lect-get-effort-sum))
							  (org-lect-get-effort (nth 6 (decode-time))))
    (/ (float (+ 1 (- totpage curpage))) (float (+ 1 deadl))))
    )
  
  )

(defun org-lect-update-today (&optional pom)
  (interactive "P")
  (org-entry-put (point) "PAGESTODAY" (number-to-string (org-lect-needed-effort-today))))



;; Decide whether the last progress happened today or not
;; (if (= 0 (org-time-stamp-to-now (org-entry-get (point) "LAST_PROGRESS"))) (message "SWEET") (message "OLD"))

;; Sets the last progress field to today
;; (org-set-property "LAST_PROGRESS" (format-time-string (org-time-stamp-format nil t)))

;; Split a last progress entry consisting of a date followed by a number (get the number)
;; (string-to-number (nth 1 (split-string (org-entry-get (point) "LAST_PROGRESS") "]")))
;; or use org-entry-get-multivalued-property
;; or use org-entry-put-multivalued-property

;; Round the division above to two digits
;; (format "%0.2f" (* .01 (round (* 100 (string-to-number (org-lect-needed-effort-today))))))
;; or (format "%0.2f" (string-to-number (org-lect-needed-effort-today)))

(defun org-agenda-lect-update-today (arg)
  "Like org-lect-needed-effort-today, but works from the agenda.
ARG is passed through. Implementation was inspired by
org-agenda.el/org-agenda-deadline."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 )
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-lect-update-today)))
      )
  )



(defun org-lect-get-effort (day &optional pom)
  "Returns the effort required on day (0=Sunday...6=Saturday)"
  (string-to-number (nth day (org-entry-get-multivalued-property
			      (or pom (point)) "LECT_EFFORT"))))

(defun org-lect-get-effort-sum (&optional pom)
  "Returns the total amount of effort to invest before
the deadline"
  (let* ((deadl (org-time-stamp-to-now (format-time-string
				       (org-time-stamp-format nil t)
				       (org-get-deadline-time (or pom (point))))))
	 (weeks (/ deadl 7))
	 (days (% deadl 7))
	 (curday (nth 6 (decode-time)))
	 (k 0))
    (while (> days -1) (setf k (+ k (org-lect-get-effort
				     (% (+ days curday ) 7))))
	   (setf days (- days 1)))
    (unless (< weeks 1) (setf j 6) (while (> j -1)
				     (setf k (+ k (* weeks (org-lect-get-effort j))))
					  (setf j (- j 1))))
    k)
  )


