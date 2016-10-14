;;; org-lect.el -- flexible tracking for large childless tasks

;;    Copyright (C) 2016 Bruce Chiarelli

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'org)
(require 'org-agenda)

(defun org-lect-needed-effort-today (&optional pom)
  "Returns the number of units needed today to stay
on track"
  (interactive "P")
  (let ((curpage (string-to-number (nth 0 (org-entry-get-multivalued-property
							 (or pom (point)) "LECT_PAGES"))))
	(totpage (string-to-number (nth 1 (org-entry-get-multivalued-property
							 (or pom (point)) "LECT_PAGES"))))
	(deadl (org-time-stamp-to-now (format-time-string
				       (org-time-stamp-format nil t)
				       (org-get-deadline-time (or pom (point)))))))
	(* (/ (float (1+ (- totpage curpage)))
	      (org-lect-get-effort-sum))
	   (org-lect-get-effort (nth 6 (decode-time))))))

(defun org-lect-update-today (&optional pom)
  "Puts the amount of effort needed for the current day into the
PAGESTODAY property"
  (interactive "P")
  (org-entry-put (point) "PAGESTODAY"
		 (number-to-string (org-lect-needed-effort-today))))

(defun org-lect-agenda-update-today (arg)
  "Like org-lect-needed-effort-today, but works from the agenda.
ARG is passed through. Implementation was inspired by
org-agenda.el/org-agenda-deadline."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-lect-update-today)))))

(defun org-lect-get-effort (day &optional pom)
  "Returns the effort required on day (0=Sunday...6=Saturday)"
  (if (org-entry-get (or pom (point)) "LECT_EFFORT")
      (string-to-number (nth day (org-entry-get-multivalued-property
				  (or pom (point)) "LECT_EFFORT")))
    1))

(defun org-lect-get-effort-sum (&optional pom)
  "Returns the total amount of effort to invest before the
deadline. The units returned are the same as are returned by
org-lect-get-effort"
  (let* ((deadl (org-time-stamp-to-now (format-time-string
				       (org-time-stamp-format nil t)
				       (org-get-deadline-time (or pom (point))))))
	 (weeks (/ deadl 7))
	 (days (% deadl 7))
	 (curday (nth 6 (decode-time))) ;; FIXME: respect org-extend-day-until
	 (k 0))

    ;; days stores the number of days until the deadline that are not
    ;; within a complete week. These extra days can be counted from
    ;; the current day. We add these up first, multiplied by the
    ;; appropriate effort factor
    (while (> days -1) (setf k (+ k (org-lect-get-effort
				     (% (+ days curday) 7))))
	   (setf days (- days 1)))
    ;; weeks stores the number of complete weeks. After the extraneous
    ;; days have been added above, we simply take the complete weeks
    ;; and multiply by the effort factors for each day.     
    (unless (< weeks 1) (setf j 6) (while (> j -1)
				     (setf k (+ k (* weeks (org-lect-get-effort j))))
					  (setf j (1- j))))
    (if (= k 0) 1 k)))


(provide 'org-lect)
;;; org-lect.el ends here
