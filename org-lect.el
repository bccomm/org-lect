;;; org-lect.el -- Pace calculation for large childless tasks in Org

;; Copyright (C) 2016 Bruce Chiarelli

;; Version: 0.1
;; Package-Requires: ((org-mode "8.0"))
;; Keywords: pim, org-mode, calendar
;; URL https://github.com/bccomm/org-lect

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


;;; Commentary:
;; This package provides functions to calculate an appropriate pace
;; for completing large tasks, given a deadline, number of units or
;; pages, and (optionally) a specification for scaling the effort up
;; or down depending on the day of the week.  See the README.org file
;; for more details.

(require 'org)
(require 'org-agenda)

;;; Code:

(defun org-lect-is-lect-p (&optional pom)
  "Return t if the current item is managed by org-lect.
Reads either from point or a specified point or marker POM."
  (if (or (org-entry-get (or pom (point)) "LECT_UNITS")
	  (org-entry-get (or pom (point)) "LECT_PAGES")) t nil))

(defun org-lect-needed-effort-today (&optional pom)
  "Return the number of units needed today to stay on track.
Considers the heading at point or at POM, if specified."
  (let* ((propstring (or (org-entry-get-multivalued-property
			  (or pom (point)) "LECT_UNITS")
			 (org-entry-get-multivalued-property
			  (or pom (point)) "LECT_PAGES")))
	 (curpage (string-to-number (nth 0 propstring)))
	 (totpage (string-to-number (nth 1 propstring)))
	 (effort (org-lect-get-effort
		  (nth 6 (decode-time
			  (org-read-date nil t "today" nil)))))
	 (deadl (org-time-stamp-to-now
		 (format-time-string (org-time-stamp-format nil t)
				     (org-get-deadline-time
				      (or pom (point)))))))
    (* (/ (float (- totpage curpage))
	  (org-lect-get-effort-sum))
       effort)))

(defun org-lect-update-today (&optional pom)
  "Puts the effort needed for the current day into PAGESTODAY.
Proceeds either from point or the specified POM."
  (interactive)
  (org-entry-put (or pom (point)) "PAGESTODAY"
		 (number-to-string (org-lect-needed-effort-today))))

(defun org-lect-get-last (&optional pom)
  "Gets and parse the list LECT_LAST multivalued property.
This consists of the last date on which progress was made and the
number of pages made then, considered either at point or POM. The
function returns a list with the number of days since last
progress."
  (let ((lastdate (or (nth 0 (org-entry-get-multivalued-property
			      (or pom (point)) "LECT_LAST"))
		      nil))
	(lastprog (or (nth 1 (org-entry-get-multivalued-property
			      (or pom (point)) "LECT_LAST")))))
    (if lastdate (list (org-time-stamp-to-now lastdate)
		       (string-to-number lastprog))
      (list nil nil))))

(defun org-lect-agenda-update-today ()
  "Like ‘org-lect-needed-effort-today’, but works from the agenda.
Implementation was inspired by org-agenda.el/org-agenda-deadline."
  (interactive)
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
  "Return the effort required on DAY (0=Sunday...6=Saturday).
Reads from point or POM."
  (if (org-entry-get (or pom (point)) "LECT_EFFORT")
      (string-to-number (nth day (org-entry-get-multivalued-property
				  (or pom (point)) "LECT_EFFORT")))
    1))

(defun org-lect-get-effort-sum (&optional pom)
  "Return the total effort to invest before the deadline.
The units returned are the same as are returned by ‘org-lect-get-effort’.
Considers the task near point or POM."
  (let* ((deadl (org-time-stamp-to-now
		 (format-time-string (org-time-stamp-format nil t)
				     (org-get-deadline-time (or pom (point))))))
	 (weeks (/ deadl 7))
	 (days (% deadl 7))
	 (curday (nth 6 (decode-time (org-read-date nil t "today" nil))))
	 (total 0))

    ;; days stores the number of days until the deadline that are not
    ;; within a complete week. These extra days can be counted from
    ;; the current day. We add these up first, multiplied by the
    ;; appropriate effort factor
    (while (> days -1) (setf total (+ total (org-lect-get-effort
					     (% (+ days curday) 7))))
	   (setf days (- days 1)))
    ;; weeks stores the number of complete weeks. After the extraneous
    ;; days have been added above, we simply take the complete weeks
    ;; and multiply by the effort factors for each day.
    (unless (< weeks 1)
      (setf dow 6) (while (> dow -1)
		     (setf total (+ total (* weeks (org-lect-get-effort dow))))
		     (setf dow (1- dow))))
    (if (= total 0) 1 total)))


(provide 'org-lect)
;;; org-lect.el ends here
