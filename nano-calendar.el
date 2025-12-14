 ;;; nano-calendar.el --- Nano calendar -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-calendar
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package displays a calendar in the current buffer and allows
;; to select one or several dates.
;;
;;; Usage:
;;
;; (require 'nano-calendar)
;; (nano-calendar)
;;

;; NEWS:
;;
;; Version 1.0
;; - Reboot: much simpler code
;;
;; Version 0.1
;; - First version

;;; Code
(require 'hl-line)
(require 'calendar)
(require 'holidays)
(require 'org-agenda)

(defgroup nano-calendar nil
  "Settings for nano calendar"
  :group 'applications)

(defgroup nano-calendar-faces nil
  "Faces "
  :group 'nano-calendar)

(defgroup nano-calendar-layout nil
  "Layout settings"
  :group 'nano-calendar)

(defgroup nano-calendar-faces nil
  "Face settings"
  :group 'nano-calendar)

(defcustom nano-calendar-layout '(3 . 4)
  "Calendar layout as (rows . columns)"
  :group 'nano-calendar-layout
  :type '(choice (const :tag "1x1" (1 . 1))
                 (const :tag "1x2" (1 . 2))
                 (const :tag "1x3" (1 . 3))
                 (const :tag "3x1" (3 . 1))
                 (const :tag "3x4" (3 . 4))
                 (const :tag "4x3" (4 . 3))
                 (const :tag "2x6" (2 . 6))
                 (const :tag "6x2" (6 . 2))))

(defcustom nano-calendar-prefix "  "
  "String for prefixing lines"
  :type 'string
  :group 'nano-calendar-layout)

(defcustom nano-calendar-colsep "  "
  "String for separating columns"
  :type 'string
  :group 'nano-calendar-layout)

(defcustom nano-calendar-rowsep "\n"
  "String for separating rows"
  :type 'string
  :group 'nano-calendar-layout)

(defface nano-calendar-header-month-today
  `((t :inverse-video t :inherit (bold)))
  "Face for header month"
  :group 'nano-calendar-faces)

(defface nano-calendar-header-month-regular
  `((t :inherit (bold highlight)))
  "Face for header month"
  :group 'nano-calendar-faces)

(defface nano-calendar-header-weekday
  '((t :underline t
       :inherit (default)))
  "Face for header weekday"
  :group 'nano-calendar-faces)

(defface nano-calendar-header-weekend
  '((t :underline t
       :inherit (font-lock-comment-face)))
  "Face for header weekend"
  :group 'nano-calendar-faces)

(defface nano-calendar-weekday
  '((t :inherit (default)))
  "Face for week days"
  :group 'nano-calendar-faces)

(defface nano-calendar-weekend
  '((t :inherit (font-lock-comment-face)))
  "Face for week end"
  :group 'nano-calendar-faces)

(defface nano-calendar-holidays
  '((t :inherit (font-lock-comment-face)))
  "Face for holidays"
  :group 'nano-calendar-faces)

(defface nano-calendar-selected
  `((t :foreground ,(face-background 'default nil 'default)
       :background ,(face-foreground 'default nil 'default)
       :inherit bold))
  "Face for selected date"
  :group 'nano-calendar-faces)

(defface nano-calendar-workload-compact-free
  `((t :inherit (widget-field)))
  "Face for free slots on compact view."
  :group 'nano-calendar-faces)

(defface nano-calendar-workload-compact-busy
  `((t :inverse-video t
       :inherit (font-lock-comment-face)))
  "Face for busy slots on compact view."
  :group 'nano-calendar-faces)

(defface nano-calendar-workload-compact-overlap
  `((t :inverse-video t
       :foreground "#FF8A65"))
  "Face for overlapping slots on compact view."
  :group 'nano-calendar-faces)

(defface nano-calendar-today
  `((t :inverse-video t
       :inherit (link bold)))
  "Face for today"
  :group 'nano-calendar-faces)

(defcustom nano-calendar-workload-color t
  "Whether to show workload as background color."
  :type 'boolean
  :group 'nano-calendar)

(defcustom nano-calendar-workload-symbol nil
  "Whether to show workload as symbol next to day."
  :type 'boolean
  :group 'nano-calendar)

(defcustom nano-calendar-workload-detail nil
  "Whether to show workload detail in echo area."
  :type 'boolean
  :group 'nano-calendar)

(defcustom nano-calendar-workload-compact t
  "Whether to show workload com=pact form in echo area."
  :type 'boolean
  :group 'nano-calendar)

(defcustom nano-calendar-workload-palette
  (let* ((black (face-foreground 'default nil 'default))
         (white (face-background 'default nil 'default)))
    (list (cons black white)       ;; Base entry
                                   ;; Material deep orange shades
          (cons black "#FBE9E7")   ;; 50
          (cons black "#FFCCBC")   ;; 100
          (cons black "#FFAB91")   ;; 200
          (cons black "#FF8A65")   ;; 300
          (cons black "#FF7043")   ;; 400
          (cons white "#FF5722")   ;; 500
          (cons white "#F4511E")   ;; 600
          (cons white "#E64A19")   ;; 700
          (cons white "#D84315")   ;; 800
          (cons white "#BF360C"))) ;; 900  
  "List of (FG . BG) color pairs for Nano Calendar workload highlighting.
The first entry is a base (foreground . background) pair using the default Emacs
colors. Subsequent entries correspond to the standard Deep Orange shades (50–900),
with foreground chosen for readability (black on light backgrounds, white on dark)."
  :type '(repeat (cons color color))
  :group 'nano-calendar)

(defcustom nano-calendar-workload-symbols " ¹²³⁴⁵⁶⁷⁸⁹*"
  "String of workload symbols."
  :type 'string
  :group 'nano-calendar)
          
(defconst nano-calendar-buffer "*nano-calendar*"
  "Name of the buffer used for the calendar.")

(defvar nano-calendar--workload-cache nil
  "Cached list of (date workload) for internal use")

(defun nano-calendar--workload-get (date)
  "Search workload for DATE in cached workload."
    (or (alist-get date nano-calendar--workload-cache nil nil #'equal) 0))

(defun nano-calendar--workload-update-all ()
  "Computes the number of active timestamps per day, based on
all org agenda files."
  (setq nano-calendar--workload-cache nil)
  (message "Updating workload")
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-element-map (org-element-parse-buffer) 'timestamp
         #'nano-calendar--workload-parse)))))

(defun nano-calendar--workload-parse (timestamp)
  "This computes workload for the given TIMESTAMP (as generated
 by org-element-parse-buffer) in order to add 1 to all days within the
start and end date of the timestamp of it is active."
  (when (memq (org-element-property :type timestamp) '(active active-range))
    (let* ((start-date
            (encode-time (list 0 0 0
                               (org-element-property :day-start timestamp)
                               (org-element-property :month-start timestamp)
                               (org-element-property :year-start timestamp))))
           (end-date
            (encode-time (list 0 0 0
                               (org-element-property :day-end timestamp)
                               (org-element-property :month-end timestamp)
                               (org-element-property :year-end timestamp))))
           (dates (let ((dates (list start-date)))
                    (while (time-less-p start-date end-date)
                      (setq start-date (time-add start-date (days-to-time 1)))
                      (setq dates (add-to-list 'dates start-date t)))
                    dates)))
      (dolist (date dates)
        (let* ((date (decode-time date))
               (date (list (nth 4 date) (nth 3 date) (nth 5 date))))
          (setf (alist-get date nano-calendar--workload-cache nil nil #'equal)
                (1+ (or (alist-get date nano-calendar--workload-cache
                                   nil nil #'equal) 0))))))))

(defun nano-calendar--workload-slots (date)
  "Compute 30-minute workload density slots for a given DATE from org agenda entries.

This function scans all files in `org-agenda-files` and collects all agenda
entries for the specified DATE. It extracts the `dotime` timestamp property
from each entry and computes how many agenda items overlap each 30-minute slot.

Return value:
A list of 48 integers, each corresponding to a 30-minute interval in the day:
- Index 0  → 00:00–00:30
- Index 1  → 00:30–01:00
- …
- Index 47 → 23:30–24:00

Each element is:
- 0 if the slot is free
- N (>=1) if N agenda items overlap this slot

Time parsing details:
- Start time is extracted using `org-parse-time-string` from the `dotime` property.
- End time is extracted from ranges like \"12:00-14:00\" in the timestamp string.
- If no explicit end time exists, the event is assumed to last 30 minutes.
- Partial-hour appointments are handled correctly.
- Any slot touched by an appointment increments that slot by 1.

NOTE:
- Appointments spanning outside the current day are not handled specially
  (assumed to stay within DATE)."
  (let ((files (org-agenda-files))
        (slots (make-list 48 0)))
    (dolist (file files)
      (dolist (entry (org-agenda-get-day-entries file date))
        (when-let* ((dt (get-text-property 0 'dotime entry))
                    (is-string (stringp dt)))
          (let* ((ts (substring-no-properties dt))
                 
                 ;; Parse start time
                 (start-parsed (org-parse-time-string ts))
                 (h-start (nth 2 start-parsed))
                 (m-start (nth 1 start-parsed))
                 
                 ;; Parse end time if present
                 (has-end (string-match "-\\([0-9]+\\):\\([0-9]+\\)" ts))
                 (h-end (if has-end
                            (string-to-number (match-string 1 ts))
                          h-start))
                 (m-end (if has-end
                            (string-to-number (match-string 2 ts))
                          (+ m-start 30))))

            ;; Normalize end minutes
            (when (>= m-end 60)
              (setq h-end (1+ h-end)
                    m-end (- m-end 60)))

            (let* ((start-min (+ (* h-start 60) m-start))
                   (end-min   (+ (* h-end 60) m-end))
                   (slot-start (/ start-min 30))
                   (slot-end   (/ end-min 30)))

              ;; Increment covered slots
              (dotimes (i 48)
                (when (and (>= i slot-start) (< i slot-end))
                  (setf (nth i slots)
                        (1+ (nth i slots))))))))))
    slots))

(defun nano-calendar-workload-compact (date &optional start-hour end-hour)
  "Return a compact representation of workload as 30-min half-hour blocks
per hour for DATE, between START-HOUR and END-HOUR.

Slot values come from `nano-calendar--workload-slots`:
- 0   → free
- 1   → busy
- >1  → overlapping events."
  
  (let* ((timestamp (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
         (date-str (format-time-string "%A %d %B %Y" timestamp))
         (slots (nano-calendar--workload-slots date))
         (start (or start-hour 8))
         (end   (or end-hour  18))
         (free    (propertize " " 'face 'nano-calendar-workload-compact-free))
         (busy    (propertize " " 'face 'nano-calendar-workload-compact-busy))
         (overlap (propertize " " 'face 'nano-calendar-workload-compact-overlap))
         (hours (mapcar (lambda (h) (format "%02d" h))
                        (number-sequence start end)))
         ;; extract the slots for the selected hours
         (hour-slots (cl-subseq slots (* 2 start) (* 2 (1+ end))))
         ;; group every 2 slots into 1 hour block with half-hour precision
         (blocks
          (cl-loop for i from 0 below (length hour-slots) by 2
                   for first  = (nth i hour-slots)
                   for second = (nth (1+ i) hour-slots)
                   for left =
                   (cond
                    ((= first 0) free)
                    ((= first 1) busy)
                    (t overlap))
                   for right =
                   (cond
                    ((= second 0) free)
                    ((= second 1) busy)
                    (t overlap))
                   collect (concat left right)))
         (line1 (concat
                 (propertize (format "%26s  " date-str) 'face 'bold)
                 (propertize (mapconcat #'identity hours " ")
                             'face 'font-lock-comment-face)))
         (line2 (concat
                 (propertize (format "%28s" "Free / Busy  ")
                             'face 'font-lock-comment-face)
                 (mapconcat #'identity blocks " ")))
         (space (make-string (- (window-width)
                                (length line1)
                                (- left-margin-width)
                                (- right-margin-width)
                                1)
                             ?\s)))
    (concat space line1 "\n" space line2)))

(defun nano-calendar-workload-update-all ()
  "Update all workloads"
  (interactive)
  (nano-calendar--workload-update-all)
  (nano-calendar))

(defun nano-calendar-workload-face (workload face)
  "Return the corresponding face for WORKLOAD given current FACE."
  (let* ((black (face-foreground face nil 'default))
         (white (face-background face nil 'default))
         (palette nano-calendar-workload-palette)
         (workload (min workload (length palette))))
    `( :foreground ,(car (nth workload palette))
       :background ,(cdr (nth workload palette)))))

(defun nano-calendar-workload-symbol (workload)
  "Return the corresponding symbol for WORKLOAD."

  (if nano-calendar-workload-symbol
      (let* ((symbols nano-calendar-workload-symbols)
             (workload (min workload (1- (length symbols)))))
        (substring symbols workload (1+ workload)))
    " "))

(defun nano-calendar--generate-day (day date is-today is-holidays is-weekend)
  "Internal function to generate a day number"
  (let* ((face (cond (is-today     'nano-calendar-today)
                     (is-holidays  'nano-calendar-holidays)
                     (is-weekend   'nano-calendar-weekend)
                     (t            'nano-calendar-weekday)))
         (workload (nano-calendar--workload-get date))
         (symbol (nano-calendar-workload-symbol workload))
         (face (cond (is-today 'nano-calendar-today)
                     ((eq workload 0) face)
                     (nano-calendar-workload-color
                      (nano-calendar-workload-face workload face))
                     (t face))))
    (propertize (format "%2d%s" day symbol)
                'date date
                'workload workload
                'is-today is-today
                'is-holidays is-holidays
                'is-weekend is-weekend
                'face face)))

(defun nano-calendar--collect--agenda-entries (date)
  "Retrieve org agenda entries for the given DATE."

  (let ((entries))
    (dolist (file (org-agenda-files))
      (dolist (entry (org-agenda-get-day-entries file date))
        (let* ((display (get-text-property 0 'display entry))
               (tags (get-text-property 0 'tags entry))
               (tags (mapconcat #'identity tags ","))
               (extra (string-trim (get-text-property 0 'extra entry)))
               (text (string-trim (get-text-property 0 'txt entry)))
               (text (replace-regexp-in-string (concat org-tsr-regexp " ") "" text))     
               (time (get-text-property 0 'time entry)))
          (unless (member "CANCELLED" (string-split tags ","))
            (push (concat
                 (propertize " " 'display display)
                   (concat
                    (unless (string-empty-p extra)
                      (concat (propertize extra
                                          'face 'default) " "))
                    (unless (or (string-empty-p time) (not (string-empty-p extra)))
                      (concat (propertize time
                                          'face 'bold) " - "))
                    (unless (string-empty-p text)
                      (concat (propertize (substring-no-properties text)
                                          'face 'nano-default) " "))
                    (unless (string-empty-p tags)
                      (propertize tags 'face 'org-tag))))
          entries)))))
  (sort entries)))

(defun nano-calendar--generate-month (year month)
  "Generate the string representation of YEAR MONTH."

  (let* ((first (calendar-day-of-week (list month 1 year)))
         (first (+ (mod (+ (- first 1) 7) 7) 1)) ;; Week starts on Monday
         (last  (+ first (calendar-last-day-of-month month year)))
         (today (decode-time (current-time)))
         (is-today-month (and (= year (nth 5 today))
                              (= month (nth 4 today))))
         (face-today                'nano-calendar-today)
         (face-selected             'nano-calendar-selected)
         (face-holidays             'nano-calendar-holidays)
         (face-weekend              'nano-calendar-weekend)
         (face-weekday              'nano-calendar-weekday)
         (face-header-weekday       'nano-calendar-header-weekday)
         (face-header-weekend       'nano-calendar-header-weekend)
         (face-header-month-regular 'nano-calendar-header-month-regular) 
         (face-header-month-today   'nano-calendar-header-month-today)
         (face-header-month (if is-today-month
                                face-header-month-today
                              face-header-month-regular))
         (month-name (format "%s %d" (calendar-month-name month) year))
         ;; 20 = 7x3 -1 : This allow to privilege slightly left centering
         (month-padding (- 20 (length month-name)))
         (month-name (concat
                      (make-string (/ month-padding 2) ? )
                      month-name
                      (make-string (- month-padding (/ month-padding 2)) ? )))
         (month-name (concat (propertize month-name
                                         'font-lock-face face-header-month
                                         'face           face-header-month)
                             ;; The terminal space does not get the face
                             ;; for aesthetic reason. If there is a background
                             ;; for face, it will be aligned with last day number.
                             " "))
         (day-names (concat (propertize "Mo Tu We Th Fr "
                                        'font-lock-face face-header-weekday
                                        'face           face-header-weekday)
                            (propertize "Sa Su"
                                        'font-lock-face face-header-weekend
                                        'face           face-header-weekend)
                             ;; The terminal space does not get the face
                             ;; for aesthetic reason. If there is a background
                             ;; for face, it will be aligned with month name
                            " "))
         (output (concat month-name "\n"
                         day-names "\n")))
    (dotimes (row 6) ;; 6 rows at most (any month, any year)
      (dotimes (col 7) ;; 7 columns for week iteration
        (let* ((index (+ 1 col (* row 7)))
               (day (- index first -1))
               (date (list month day year))
               (is-holidays (calendar-check-holidays date))
               (is-today (and (= (nth 2 date) (nth 5 today))
                              (= (nth 0 date) (nth 4 today))
                              (= (nth 1 date) (nth 3 today))))
               (is-selected nil)
               (is-saturday (eq col 5))
               (is-sunday (eq col 6))
               (is-weekend (or is-saturday is-sunday))
               (face (cond (is-today     face-today)
                           (is-holidays  face-holidays)
                           (is-weekend   face-weekend)
                           (t            face-weekday))))
          (setq output
                (concat output 
                        (if (or (< index first) (>= index last))
                            "   "
                          (nano-calendar--generate-day day
                                                       date
                                                       is-today
                                                       is-holidays
                                                       is-weekend))))))
      (when (< row 5)
        (setq output (concat output "\n"))))
    output))
  
(defun nano-calendar--concat-months (months prefix separator)
  "Concatenate MONTHS, prefixes with PREFIX and separated by SEPARATOR."
  (let* ((lists (mapcar (lambda (month) (split-string month "\n")) months))
         (lines (apply #'cl-mapcar
                       (lambda (&rest parts)
                         (concat prefix (mapconcat #'identity parts separator)))
                       lists)))
    (mapconcat #'identity lines "\n")))

(defun nano-calendar--generate (&optional date layout)
  "Generate a calendar showing DATE and using specified LAYOUT."
  (let* ((layout (or layout nano-calendar-layout))
         (nrows (car layout))
         (ncols (cdr layout))
         (prefix nano-calendar-prefix)
         (colsep nano-calendar-colsep)
         (rowsep nano-calendar-rowsep)         
         (today (nano-calendar-today))
         (date-c (or date today))
         (date-1 (list 1 1 (nth 2 date-c)))
         (date-p (nano-calendar-date-add date-c '(0 -1 0)))
         ;; Based on layout, we compute the first month to be displayed
         ;; such as to generate the whole sequence of months.
         (start (cond ((equal layout '(1 . 1)) date-c)
                      ((equal layout '(1 . 2)) date-c)
                      ((equal layout '(1 . 3)) date-p)
                      ((equal layout '(3 . 1)) date-p)
                      ((equal layout '(2 . 6)) date-1)
                      ((equal layout '(6 . 2)) date-1)
                      ((equal layout '(3 . 4)) date-1)
                      ((equal layout '(4 . 3)) date-1)
                      (t                       date-1)))
         (month (nth 0 start))
         (year  (nth 2 start))
         (months (let (result)
                   (dotimes (i (* nrows ncols) (nreverse result))
                     (push (nano-calendar--generate-month (+ year (/ (+ month i -1) 12))
                                                          (+ 1 (mod (+ month i -1) 12)))
                           result))))
         (chunks (seq-partition months ncols)))
    (mapconcat
     (lambda (batch)
       (nano-calendar--concat-months batch prefix colsep))
     chunks
     (concat "\n" rowsep))))

(defun nano-calendar-date-add (date delta)
  "Return a new DATE (month day year) adjusted by DELTA (day mont year).

DATE must be a list of the form (MONTH DAY YEAR).
DAY and YEAR are integer offsets in days and years.
WEEK is an integer offset in weeks (each week = 7 days).
MONTH is an integer offset in months.  All units may be negative.

This function uses `encode-time' to normalize overflow, so adding
months or days that exceed the calendar bounds (e.g. from January 31
to \"the next month\") will produce the correct normalized date."
  
  (calendar-gregorian-from-absolute
   (time-to-days
    (encode-time
     0 0 0
     (+ (nth 1 date) (nth 0 delta)) 
     (+ (nth 0 date) (nth 1 delta))
     (+ (nth 2 date) (nth 2 delta))))))

(defun nano-calendar-today ()
  "Return today date."
  (interactive)
  (let ((date (decode-time (current-time))))
    (list (nth 4 date) (nth 3 date) (nth 5 date))))

(defun nano-calendar-is-date-visible (date)
  "Return whether DATE is currently visible."

  (when-let ((buffer (get-buffer nano-calendar-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (text-property-search-forward 'date date t)))))

(defun nano-calendar-goto-date (date &optional redisplay)
  "Move point to the first character in the buffer whose 'date property
equals DATE (month day year).  If the date is not currently visible 
and REDISPLAY is t, the calendar is re-generated such as to make the date
 visible.  Else, return t if found, nil otherwise."

  (cond ((nano-calendar-is-date-visible date)
         (goto-char (point-min))
         (when-let ((match (text-property-search-forward 'date date t)))
           (goto-char (prop-match-beginning match))
           (force-mode-line-update)
           (when nano-calendar-workload-compact
             (nano-calendar-show-workload-compact))
           (when nano-calendar-workload-detail
             (nano-calendar-show-org-agenda-entries))
           t))
  
        (redisplay
         (nano-calendar date)
         (when nano-calendar-workload-compact
           (nano-calendar-show-workload-compact))
         (when nano-calendar-workload-detail
           (nano-calendar-show-org-agenda-entries))
         t)
  
        (t
         nil)))

(defun nano-calendar-goto-relative-date (delta &optional redisplay)
  "Move point to date under cursor adjusted by DELTA (day month year).

If the date is not currently displayed and REDISPLAY is t, the calendar
is redisplayed such as to make the date visible.  When there is no date
under cursor, move to the closest date (this may following user
non-navigation moves."
  (let ((cursor-date (nano-calendar-cursor-date))
        (closest-date (nano-calendar-closest-date)))
    (cond (cursor-date
           (nano-calendar-goto-date 
            (nano-calendar-date-add cursor-date delta) redisplay))
          (closest-date
           (nano-calendar-goto-date closest-date redisplay))
          (t (message "No date under cursor")))))

(defun nano-calendar-cursor-date ()
  "Move cursor to today."
  (interactive)
  (get-text-property (point) 'date))

(defun nano-calendar-cursor-workload ()
  "Return workload for date under cursor."
  (interactive)
  (get-text-property (point) 'workload))

(defun nano-calendar-closest-date ()
  "Get point for closest date from cursor. Backward search first."
  (interactive)
  (let ((curr (get-text-property (point) 'date))
        (prev (save-excursion
                (text-property-search-backward 'date)
                (get-text-property (point) 'date)))
        (next (save-excursion
                (text-property-search-forward 'date)
                (backward-char)
                (get-text-property (point) 'date))))
    (or curr prev next)))

(defun nano-calendar--hl-line-range ()
  "Return the start and end of the region that has the same `date` property
at point."
  (save-match-data
    (save-excursion
      (if-let* ((date (get-text-property (point) 'date))
                (match (text-property-search-forward 'date date t))
                (beg (prop-match-beginning match))
                (end (prop-match-end match)))
          (cons beg end)
      (cons (point-min) (point-min))))))

(defun nano-calendar-goto-today ()
  "Move cursor to today."
  (interactive)
  (nano-calendar-goto-date (nano-calendar-today) t))

;; Macro to declare multiple nano-calendar navigation commands at once.
;; Each SPEC is of the form:
;;   (FUNCTION-NAME "Docstring" DAY WEEK MONTH YEAR)
;; The macro expands each SPEC into:
;;   (defun FUNCTION-NAME ()
;;     "Docstring"
;;     (interactive)
;;     (nano-calendar-goto-relative-date DELTA)
(defmacro nano-calendar-define-moves (&rest specs)
  "Macro to declare multiple nano-calendar navigation commands at once."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (let ((name (nth 0 spec))
                (doc  (nth 1 spec))
                (args (nthcdr 2 spec))) ; (DAY MONTH YEAR)
            `(defun ,name ()
               ,doc
               (interactive)
               (nano-calendar-goto-relative-date ,@args t))))
        specs)))

(nano-calendar-define-moves
  ;; Day navigation
  (nano-calendar-goto-prev-day  "Move cursor to previous day."  '(-1 0 0))
  (nano-calendar-goto-next-day  "Move cursor to next day."      '(+1 0 0))
  ;; Week navigation
  (nano-calendar-goto-prev-week "Move cursor to previous week."  '(-7 0 0))
  (nano-calendar-goto-next-week "Move cursor to next week."      '(+7 0 0))
  ;; Month navigation
  (nano-calendar-goto-prev-month "Move cursor to previous month." '(0 -1 0))
  (nano-calendar-goto-next-month "Move cursor to next month."     '(0 +1 0))
  ;; Year navigation
  (nano-calendar-goto-prev-year  "Move cursor to previous year."  '(0 0 -1))
  (nano-calendar-goto-next-year  "Move cursor to next year."      '(0 0 +1)))

(defun nano-calendar-quit ()
  "Burry calendar buffer."
  (interactive)
  (bury-buffer))

(defun nano-calendar-workload-menu ()
  "Toggle one of the mode flags via single key: [c]olor [s]ymbol [d]etails."
  (interactive)
  (let ((key (read-key (concat "Workload display:"
                               "   [b] background color"
                               "   [s] symbol"
                               "   [d] detail"
                               "   [c] compact"
                               )))
        (message-log-max nil))
    (pcase key
      (?b (setq nano-calendar-workload-color
                (not nano-calendar-workload-color))
          (nano-calendar))
      (?s (setq nano-calendar-workload-symbol
                (not nano-calendar-workload-symbol))
          (nano-calendar))
      (?c (setq nano-calendar-workload-compact
                (not nano-calendar-workload-compact))
          (nano-calendar))
      (?d (setq nano-calendar-workload-detail
                (not nano-calendar-workload-detail))
          (nano-calendar))
      (_ ))))

(defun nano-calendar-show-org-agenda-entries ()
  "Show agenda entries  in echo area for current date."
  (interactive)
  (when-let* ((date (nano-calendar-cursor-date))
              (entries (nano-calendar--collect--agenda-entries date)))
    (let ((message-log-max nil))
      (message "%s" (mapconcat #'identity entries "\n")))))

(defun nano-calendar-show-workload-compact ()
  "Show compact workload in echo area for current date."
  
  (interactive)
  (when-let* ((date (nano-calendar-cursor-date)))
    (let ((message-log-max nil))
      (message (nano-calendar-workload-compact date)))))

(defun nano-calendar-goto-org-agenda ()
  "Got to org agenda for current DATE."

  (interactive)
  (when-let* ((date (nano-calendar-cursor-date))
              (year (nth 2 date))
              (month (nth 0 date))
              (day (nth 1 date))
              (date (format "%04d-%02d-%02d" year month day)))
    (nano-calendar-quit)
    (let ((org-agenda-start-day date))
      (org-agenda nil "n"))))

(define-minor-mode nano-calendar-mode
  "Nano calendar mode"
  :keymap  `((,(kbd "<left>")    . nano-calendar-goto-prev-day)
             (,(kbd "<right>")   . nano-calendar-goto-next-day)
             (,(kbd "<up>")      . nano-calendar-goto-prev-week)
             (,(kbd "<down>")    . nano-calendar-goto-next-week)
             (,(kbd "<S-left>")  . nano-calendar-goto-prev-month)
             (,(kbd "<S-right>") . nano-calendar-goto-next-month)
             (,(kbd "<S-down>")  . nano-calendar-goto-next-year)
             (,(kbd "<S-up>")    . nano-calendar-goto-prev-year)
             (,(kbd "v")         . nano-calendar-show-org-agenda-entries)
             (,(kbd "<RET>")     . nano-calendar-goto-org-agenda)
             (,(kbd "<SPC>")     . nano-calendar-show-org-agenda-entries)
             (,(kbd "w")         . nano-calendar-workload-menu)
             (,(kbd ".")         . nano-calendar-goto-today)
             (,(kbd "r")         . nano-calendar-workload-update-all)
             (,(kbd "q")         . nano-calendar-quit))
  (when nano-calendar-mode
    (setq-local hl-line-range-function #'nano-calendar--hl-line-range)
    (face-remap-set-base 'hl-line 'nano-calendar-selected)
    (hl-line-mode 1)
    (setq buffer-read-only t)))

(defun nano-calendar (&optional date layout)
  "Display a Gregorian calendar showing DATE and enforcing LAYOUT."
  (interactive)
  (let* ((buffer (get-buffer nano-calendar-buffer))
         (date (cond (date date)
                     ((buffer-live-p buffer)
                      (with-current-buffer buffer
                        (nano-calendar-closest-date)))
                     (t (nano-calendar-today))))
         (layout (or layout nano-calendar-layout)))
    (let ((buffer (get-buffer-create nano-calendar-buffer)))
      (unless (get-buffer-window buffer)
        (pop-to-buffer buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (concat "\n" (nano-calendar--generate date layout) "\n")))
        (fit-window-to-buffer nil nil (window-height))

        ;; Enter calendar mode if not already in calendar mode
        (unless nano-calendar-mode
          (nano-calendar-mode 1)
          ;; Upate all workloads (when visible)
          (when (or nano-calendar-workload-color
                    nano-calendar-workload-symbol)
            (nano-calendar--workload-update-all))
          (run-hooks 'nano-calendar-hook))        
        (nano-calendar-goto-date date)))))
