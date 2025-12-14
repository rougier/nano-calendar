;;; nano-calendar.el --- Nano calendar -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-calendar
;; Version: 0.1
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
;; (message (format-time-string "%A %d %B %Y") nano-calendar-date)


;; NEWS:
;;
;; Version 0.1
;; - First version

;;; Code
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

(defface nano-calendar-header-month-face
  `((t :inherit (bold)))
  "Face for header month"
  :group 'nano-calendar-faces)

(defface nano-calendar-header-weekday-face
  '((t :inherit (nano-strong)))
  "Face for header weekday"
  :group 'nano-calendar-faces)

(defface nano-calendar-header-weekend-face
  '((t :inherit (font-lock-comment-face bold)))
  "Face for header weekend"
  :group 'nano-calendar-faces)

(defface nano-calendar-weekday-face
  '((t :inherit (default)))
  "Face for week days"
  :group 'nano-calendar-faces)

(defface nano-calendar-weekend-face
  '((t :inherit (font-lock-comment-face)))
  "Face for week end"
  :group 'nano-calendar-faces)

(defface nano-calendar-holiday-face
  '((t :inherit (nano-calendar-header-weekend-face)))
  "Face for holidays"
  :group 'nano-calendar-faces)

(defface nano-calendar-today-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :bold t))
  "Face for today"
  :group 'nano-calendar-faces)

(defface nano-calendar-marked-face
  '((t :inherit (nano-default-i)))
  "Face for marked days"
  :group 'nano-calendar-faces)

(defface nano-calendar-current-face
  `((t :foreground ,(face-background 'link nil 'default)
       :background ,(face-foreground 'link nil 'default)
       :bold t))
  "Face for current selection"
  :group 'nano-calendar-faces)

(defcustom nano-calendar-navigation-mode 'spatial
  "Navigation system when arrows are pressed. Spatial means to move to the prev/next date on the same line while chronological means to move to the prev/next date on the same month."

  :group 'nano-calendar
  :type '(choice (const :tag "Spatial" spatial)
                 (const :tag "Chronological" chronological)))

(defcustom nano-calendar-prefix ""
  "String for prefixing lines"
  :type 'string
  :group 'nano-calendar-layout)

(defcustom nano-calendar-column-separation "  "
  "String for separating columns"
  :type 'string
  :group 'nano-calendar-layout)

(defcustom nano-calendar-row-separation ""
  "String for separating rows"
  :type 'string
  :group 'nano-calendar-layout)

(defcustom nano-calendar-default-layout '(3 . 4)
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

(defconst nano-calendar-buffer "*nano-calendar*"
  "Name of the calendar buffer")

(defvar nano-calendar--current nil
  "Current selected date")

(defvar nano-calendar--current-overlay nil
  "Current selected date overlay")

(defvar nano-calendar-update-hook nil
  "Hook ran after a calendar update")

(defvar nano-calendar-date-changed-hook nil
  "Hook for when current date has changed")

(defvar nano-calendar--marked nil
  "List of marked dates")

;; See https://material.io/design/color/the-color-system.html
(defvar nano-calendar-palettes
  (let ((palettes))
    (dolist (item  '((red         . ("#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
                                     "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"))
                     (pink        . ("#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
                                     "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"))
                     (purple      . ("#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
                                     "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"))
                     (deep-purple . ("#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
                                     "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"))
                     (indigo      . ("#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
                                     "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"))
                     (blue        . ("#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
                                     "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"))
                     (light-blue  . ("#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
                                     "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"))
                     (cyan        . ("#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
                                     "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"))
                     (teal        . ("#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
                                     "#009688" "#00897B" "#00796B" "#00695C" "#004D40"))
                     (green       . ("#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
                                     "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"))
                     (light-green . ("#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
                                     "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"))
                     (lime        . ("#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
                                     "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"))
                     (yellow      . ("#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
                                     "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"))
                     (amber       . ("#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
                                     "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"))
                     (orange      . ("#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
                                     "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"))
                     (deep-orange . ("#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
                                     "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"))
                     (brown       . ("#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
                                     "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723"))
                     (grey        . ("#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
                                     "#9E9E9E" "#757575" "#616161" "#424242" "#212121"))
                     (blue-grey   . ("#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
                                     "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238"))
                     (viridis     . ("#fde725" "#b5de2b" "#6ece58" "#35b779" "#1f9e89"
                                     "#26828e" "#31688e" "#3e4989" "#482878" "#440154"))
                     (magma       . ("#fcfdbf" "#feca8d" "#fd9668" "#f1605d" "#cd4071"
                                     "#9e2f7f" "#721f81" "#440f76" "#180f3d" "#000004"))
                     (inferno     . ("#fcffa4" "#f7d13d" "#fb9b06" "#ed6925" "#cf4446"
                                     "#a52c60" "#781c6d" "#4a0c6b" "#1b0c41" "#000004"))))
      (push (cons (car item) (cdr item)) palettes)
      (push (cons (intern (concat (symbol-name (car item)) "_r")) (reverse (cdr item)))  palettes))
    palettes))

;; Copied for s library (s-center)
(defun nano-calendar--center (len s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (declare (pure t) (side-effect-free t))
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) ?\s)
     s
     (make-string (floor extra 2) ?\s))))

(defun nano-calendar--color-luminance (color)
  "Calculate the relative luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (red (/ (car values) 256.0))
         (green (/ (cadr values) 256.0))
         (blue (/ (caddr values) 256.0)))
    (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 255)))

(defcustom nano-calendar-palette 'deep-orange
  (concat
   "Background colors to use to highlight a day in calendar view according to workload.\n\n"
   (mapconcat (lambda (name)
                (let* ((colors (alist-get name nano-calendar-palettes))
                       (colors (mapconcat
                                (lambda (color)
                                  (propertize "  " 'face `(:background ,color)))
                                colors ""))
                       (name_r (intern (concat (symbol-name name) "_r")))
                       (colors_r (alist-get name_r nano-calendar-palettes))
                       (colors_r (mapconcat
                                  (lambda (color)
                                    (propertize "  " 'face `(:background ,color)))
                                  colors_r "")))
                  (concat (format "%-14s" name) " : " colors
                          "      "
                          (format "%-14s" name_r) " : " colors_r)))
              '(red pink purple deep-purple indigo blue light-blue cyan 
                    teal green light-green lime yellow amber orange
                    deep-orange brown grey blue-grey viridis magma inferno)
              "\n"))

  :type `(choice (const red)              (const red_r)          
                 (const pink)             (const pink_r)         
                 (const purple)           (const purple_r)       
                 (const deep-purple)      (const deep-purple_r)  
                 (const indigo)           (const indigo_r)       
                 (const blue)             (const blue_r)         
                 (const light-blue)       (const light-blue_r)   
                 (const cyan)             (const cyan_r)         
                 (const teal)             (const teal_r)         
                 (const green)            (const green_r)        
                 (const light-green)      (const light-green_r)  
                 (const lime)             (const lime_r)         
                 (const yellow)           (const yellow_r)       
                 (const amber)            (const amber_r)        
                 (const orange)           (const orange_r)       
                 (const deep-orange)      (const deep-orange_r)  
                 (const brown)            (const brown_r)        
                 (const grey)             (const grey_r)         
                 (const blue-grey)        (const blue-grey_r)    
                 (const viridis)          (const viridis_r)      
                 (const magma)            (const magma_r)        
                 (const inferno)          (const inferno_r))     
  :group 'nano-calendar-faces)

(defcustom nano-calendar-show-busy t
  "Whether to color days according to busy level (slow)"
  :type 'boolean
  :group 'nano-calendar)

(defun nano-calendar--date-inc (date minutes hours days months years)
  "Add MINUTES minutes, HOURS hours, DAYS day, MONTH months & YEARS
years to DATE (default to nano-calendar--current"

  (let* ((date (or date nano-calendar--current))
         (minute (nano-calendar--date-minute date))
         (hour (nano-calendar--date-hour date))
         (day (nano-calendar--date-day date))
         (month (nano-calendar--date-month date))
         (year (nano-calendar--date-year date)))
    (encode-time 0
                 (+ minute minutes)
                 (+ hour hours)
                 (+ day days)
                 (+ month months)
                 (+ year years))))

  
(defun nano-calendar-current-date ()
  "Return the current date in a list (month day year)."

  (with-current-buffer nano-calendar-buffer
    (let ((decoded (decode-time nano-calendar--current)))
      (list (nth 4 decoded)
            (nth 3 decoded)
            (nth 5 decoded)))))

(defun nano-calendar--datetime-to-date (datetime)
  "Return DATE as (day month year)"

  (list (nano-calendar--date-day datetime)
        (nano-calendar--date-month datetime)
        (nano-calendar--date-year datetime)))

(defun nano-calendar--date-to-datetime (date)
  "Convert DATE to datetime format. DATE can be expressed as (day
month year), (month year) or (year)"

  (cond ((eq (length date) 1)
         (encode-time (list 0 0 0 1 1 (nth 0 date))))
         ((eq (length date) 2)
          (encode-time (list 0 0 0 1 (nth 0 date) (nth 1 date))))
         (t
          (encode-time (list 0 0 0
                       (nth 0 date) (nth 1 date) (nth 2 date))))))

(defun nano-calendar--date-equal (date-1 date-2)
  "Return t if DATE-1 is equal to DATE-2 (irrespective of time)"

  (and date-1 date-2
       (eq (nano-calendar--date-day date-1)
           (nano-calendar--date-day date-2))
       (eq (nano-calendar--date-month date-1)
           (nano-calendar--date-month date-2))
       (eq (nano-calendar--date-year date-1)
           (nano-calendar--date-year date-2))))

(defun nano-calendar-today ()
  "Return DATE for today (time set to 0)"
  (current-time))

(defun nano-calendar-yesterday ()
  "Return DATE for yesterday (time set to 0)"
  (nano-calendar-backward-day (current-time)))

(defun nano-calendar-tomorrow ()
  "Return DATE for testerday (time set to 0)"
  (nano-calendar-forward-day (current-time)))

(defun nano-calendar--date-second (date)
  "Return DATE seconds (0-59)"
  (nth 0 (decode-time date)))

(defun nano-calendar--date-minute (date)
  "Return DATE minutes (0-59)"
  (nth 1 (decode-time date)))

(defun nano-calendar--date-hour (date)
  "Return DATE hour (0-23)"
  (nth 2 (decode-time date)))

(defun nano-calendar--date-day (date)
  "Return DATE day of month (1-31)"
  (nth 3 (decode-time date)))

(defun nano-calendar--date-month (date)
  "Return DATE month number (1-12)"
  (nth 4 (decode-time date)))

(defun nano-calendar--date-year (date)
  "Return DATE year"
  (nth 5 (decode-time date)))

(defun nano-calendar-forward-day (&optional date)
  "Move current date 1 day forward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 +1 0 0))

(defun nano-calendar-backward-day (&optional date)
  "Move current date 1 day backward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 -1 0 0))

(defun nano-calendar-forward-week (&optional date)
  "Move current date 1 week forward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 +7 0 0))

(defun nano-calendar-backward-week (&optional date)
  "Move current date 1 week backward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 -7 0 0))

(defun nano-calendar-first-month (&optional date)
  "Move current date to first month of the year (1/1/YYYY)"

  (interactive)
  (let* ((date (or date nano-calendar--current))
         (second (nano-calendar--date-second date))
         (minute (nano-calendar--date-minute date))
         (hour (nano-calendar--date-hour date))
         (day (nano-calendar--date-day date))
         (year (nano-calendar--date-year date)))
    (encode-time second minute hour 1 1 year)))

(defun nano-calendar-last-month (&optional date)
  "Move current date to last month of the year (1/12/YYYY)"

  (interactive)
  (let* ((date (or date nano-calendar--current))
         (second (nano-calendar--date-second date))
         (minute (nano-calendar--date-minute date))
         (hour (nano-calendar--date-hour date))
         (day (nano-calendar--date-day date))
         (year (nano-calendar--date-year date)))
    (encode-time second minute hour 1 12 year)))

(defun nano-calendar-forward-month (&optional date)
  "Move current date 1 month forward"

  (let* ((date (or date nano-calendar--current))
         (minute (nano-calendar--date-minute date))
         (hour (nano-calendar--date-hour date))
         (day (nano-calendar--date-day date))
         (month (1+ (nano-calendar--date-month date)))
         (year (nano-calendar--date-year date))
         (year (if (> month 12) (1+ year) year))
         (month (if (> month 12) 1 month))
         (day (min day (date-days-in-month year month))))
    (encode-time 0 minute hour day month year)))

(defun nano-calendar-backward-month (&optional date)
  "Move current date 1 month backward"

  (let* ((date (or date nano-calendar--current))
         (minute (nano-calendar--date-minute date))
         (hour (nano-calendar--date-hour date))
         (day (nano-calendar--date-day date))
         (month (1- (nano-calendar--date-month date)))
         (year (nano-calendar--date-year date))
         (year (if (< month 1) (1- year) year))
         (month (if (< month 1) 12 month))
         (day (min day (date-days-in-month year month))))
    (encode-time 0 minute hour day month year)))

(defun nano-calendar-forward-year (&optional date)
  "Move current date 1 year forward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 0 0 +1))

(defun nano-calendar-backward-year (&optional date)
  "Move current date 1 year backward"
  (interactive)
  (nano-calendar--date-inc (or date nano-calendar--current) 0 0 0 0 -1))

(defun nano-calendar-goto (date)
  "Go to given date."

  (let* ((overlay nano-calendar--current-overlay)
         (beg (overlay-start overlay))
         (end (overlay-end overlay))
         (inhibit-read-only t))

    ;; If the whole year is displayed, we can save some updates
    (setq nano-calendar--current date)
    (remove-text-properties (point-min) (point-max) '(current))
    (nano-calendar-update)
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                        'date date #'nano-calendar--date-equal))
                (beg (prop-match-beginning match))
                (end (prop-match-end match)))
      (move-overlay overlay beg end)
      (add-text-properties beg end '(current t))
      (setq nano-calendar--current date)
      (run-hooks 'nano-calendar-date-changed-hook))
    (force-mode-line-update)
    (fit-window-to-buffer nil nil (window-height))
    (backward-char 3)))

(defun nano-calendar-goto-today ()
  "Move current date to today"

  (interactive)
  (nano-calendar-goto (nano-calendar-today)))

(defun nano-calendar-goto-prev-day ()
  "Go to previous day"

  (interactive)
  (nano-calendar-goto (nano-calendar-backward-day)))

(defun nano-calendar-goto-next-day ()
  "Go to next day"

  (interactive)
  (nano-calendar-goto (nano-calendar-forward-day)))

(defun nano-calendar-goto-down ()
  "Go to date below (irrespective of month)"

  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'current t)
  (setq goal-column (string-to-number (format-mode-line "%c")))
  
  (let ((date (catch 'found
                (while (not (eobp))
                  (next-line 1 nil)
                  (let ((date_ (get-text-property (point) 'date)))
                    (when date_
                      (throw 'found date_)))))))
    (when date
      (nano-calendar-goto date))))

(defun nano-calendar-goto-up ()
  "Go to date above (irrespective of month)"

  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'current t)
  (setq goal-column (string-to-number (format-mode-line "%c")))
  (let ((date (catch 'found
                (while (not (eobp))
                  (previous-line 1 nil)
                  (let ((date_ (get-text-property (point) 'date)))
                    (when date_
                      (throw 'found date_)))))))
    (when date
      (nano-calendar-goto date))))

(defun nano-calendar-goto-next ()
  "Go to next date on the line (irrespective of month)"

  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'current t)
  (text-property-search-forward 'current nil)
  (if-let* ((match (text-property-search-forward 'date))
            (date (prop-match-value match)))
      (nano-calendar-goto date)
    (nano-calendar-goto-next-day)))

(defun nano-calendar-goto-prev ()
  "Go to prev date on the line  (irrespective of month)"

  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'current t)
  (text-property-search-backward 'current nil)
  (if-let* ((match (text-property-search-backward 'date))
            (date (prop-match-value match)))
      (nano-calendar-goto date)
    (nano-calendar-goto-prev-day)))

(defun nano-calendar-goto-prev-week ()
  "Go to previous week"

  (interactive)
  (nano-calendar-goto (nano-calendar-backward-week)))

(defun nano-calendar-goto-next-week ()
  "Go to next week"

  (interactive)
  (nano-calendar-goto (nano-calendar-forward-week)))

(defun nano-calendar-goto-prev-month ()
  "Go to previous month"

  (interactive)
  (nano-calendar-goto (nano-calendar-backward-month)))

(defun nano-calendar-goto-next-month ()
  "Go to next month"

  (interactive)
  (nano-calendar-goto (nano-calendar-forward-month)))

(defun nano-calendar-goto-prev-year ()
  "Go to previous year"

  (interactive)
  (nano-calendar-goto (nano-calendar-backward-year)))

(defun nano-calendar-goto-next-year ()
  "Go to next year"

  (interactive)
  (nano-calendar-goto (nano-calendar-forward-year)))

(defun nano-calendar-palette-next ()
  "Select next color palette and redisplay calendar"
  (interactive)

  (let ((palettes (mapcar #'car nano-calendar-palettes)))
    (setq nano-calendar-palette
        (or (cadr (member nano-calendar-palette palettes))
            (car palettes))))
  (nano-calendar-force-update-month-all)
  (message "Switching to palette %s" nano-calendar-palette))

(defun nano-calendar-mark ()
  "Mark current date"

  (interactive)
  (nano-calendar-generate-month nil nil t)
  (setq nano-calendar--marked
        (if (cl-member nano-calendar--current nano-calendar--marked
                       :test #'nano-calendar--date-equal)
            (delete nano-calendar--current nano-calendar--marked)
          (add-to-list 'nano-calendar--marked nano-calendar--current)))
  ;;(nano-calendar-generate-month nil nil t)
  (nano-calendar-force-update-month-current)
  (nano-calendar-goto-next-day))

(defun nano-calendar-unmark-all ()
  "Unmake all marked dates"

  (interactive)
  (setq nano-calendar--marked nil)
  (setq nano-calendar--cached-month nil)
  (nano-calendar-update))

(defvar nano-calendar--workloads nil
  "Cached list of (date workload) for internal use")

(defun nano-calendar--workloads-process (timestamp)
  "This process TIMESTAMP (as generated by org-element-parse-buffer) in
order to add 1 to all days within the start and end date of the
timestamp of it is active."

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
               (date (list (nth 5 date) (nth 4 date) (nth 3 date))))
          (setf (alist-get date nano-calendar--workloads nil nil #'equal)
                (1+ (or (alist-get date nano-calendar--workloads nil nil #'equal) 0))))))))

(defun nano-calendar--workloads-update ()
  "Computes the number of active timestamps per day, based on
all org agenda files."

  (setq nano-calendar--workloads nil)
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-element-map (org-element-parse-buffer) 'timestamp
         #'nano-calendar--workloads-process)))))

(defun nano-calendar-workloads-update ()
  "Computes the number of active timestamps per day, based on
all org agenda files and redisplay calendar."

  (interactive)
  (nano-calendar--workloads-update)
  (nano-calendar-force-update-month-all))

(defun nano-calendar--workload (date)
  "Return the workload for the given DATE."

  (or (alist-get date nano-calendar--workloads nil nil #'equal) 0))

(defun nano-calendar--org-agenda-entries (&optional date)
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
                      (concat (propertize extra 'face 'default) " "))
                    (unless (or (string-empty-p time) (not (string-empty-p extra)))
                      (concat (propertize time 'face 'bold) " - "))
                    (unless (string-empty-p text)
                      (concat (propertize (substring-no-properties text) 'face 'nano-default) " "))
                    (unless (string-empty-p tags)
                      (propertize tags 'face 'org-tag))))
          entries)))))
  (sort entries)))

(defun nano-calendar-insert-agenda (&optional date)
  "Insert org agenda for given DATE. Depending on the calendar layout,
agenda will be displayed on the right or below the calendar."

  (let* ((date (or date nano-calendar--current (current-time)))
         (str-date (format-time-string "%A, %d %B %Y" date))
         (alignment (if (eq (cdr nano-calendar-layout) 1)
                        (propertize " " 'display '(space :align-to (+ left 26)))
                      ""))
         (separator (make-string (length str-date) ? ))
         (next-line-add-newlines t)
         (org-date (decode-time date))
         (org-date (list (nth 4 org-date) (nth 3 org-date) (nth 5 org-date)))
         (holidays (calendar-check-holidays org-date)))
    (if (eq (cdr nano-calendar-layout) 1)
        (goto-char (point-min))
      (goto-char (point-max)))
    (forward-line)
    (end-of-line)
    (insert (concat alignment (propertize str-date 'face 'nano-salient-s)))
    (next-line 1) (end-of-line)
    ;; (insert (concat alignment (propertize separator 'face 'nano-salient)))
    (when holidays 
        (insert(concat alignment (propertize (car holidays) 'face 'nano-faded))))
    (next-line 1) (end-of-line)
    (let ((point (point)))
      (dolist (entry (nano-calendar--org-agenda-entries org-date)) 
        (insert (concat alignment (format "%s" entry)))
        (next-line) (end-of-line))
      (goto-char point))))

(defvar nano-calendar--cached-month nil
  "Cached month for faster rendering")

(defun nano-calendar-generate-month (month year &optional force)
  "Return the (possibility cached) representation of MONTH YEAR."

  (let* ((month (or month (nano-calendar--date-month
                           nano-calendar--current)))
         (year (or year (nano-calendar--date-year
                           nano-calendar--current)))
         (key (cons month year))
         (keys (mapcar 'car nano-calendar--cached-month)))
    (cond ((not (member key keys))
           (add-to-list 'nano-calendar--cached-month
                        (cons key (nano-calendar--generate-month month year force))))
          (force
           (setcdr (assoc key nano-calendar--cached-month #'equal)
                   (nano-calendar--generate-month month year force))))
    (cdr (assoc key nano-calendar--cached-month #'equal))))

(defun nano-calendar--generate-month (month year &optional force)
  "Generate the representation of MONTH YEAR."
  (let* ((first (calendar-day-of-week (list month 1 year)))
         (first (+ (mod (+ (- first 1) 7) 7) 1)) ;; Week starts on Monday
         (last  (+ first (calendar-last-day-of-month month year)))
         (today (nano-calendar-today))
         (days ""))
    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((index (+ 1 col (* row 7)))
               (day (- index first -1))
               (date (encode-time 0 0 0 day month year))
               (is-holidays (calendar-check-holidays (list month day year)))
               (is-today (nano-calendar--date-equal date today))
               (level (nano-calendar--workload (list year month day)))
               (backgrounds (alist-get nano-calendar-palette nano-calendar-palettes))
               (level (min (length backgrounds) level))
               (background (nth (- level 1) backgrounds))
               (foreground (if (< (nano-calendar--color-luminance background) 0.55)
                               "white" "black"))
               (is-marked (cl-member date nano-calendar--marked
                                     :test #'nano-calendar--date-equal))
               (is-current (nano-calendar--date-equal date nano-calendar--current))
               (char-width (frame-char-width))
               (face (cond (is-marked
                            '(:inherit nano-calendar-marked-face))
                           ((> level 0)
                            `( :foreground ,foreground
                               :background ,background
                               :inherit ,(when is-today 'nano-calendar-today-face)))
                           (is-today
                            '(:inherit nano-calendar-today-face))
                           (is-holidays
                            '(:inherit nano-calendar-holiday-face))
                           ((eq col 5)
                            '(:inherit nano-calendar-weekend-face))
                           ((eq col 6)
                            '(:inherit nano-calendar-weekend-face))
                           (t
                            '(:inherit nano-calendar-weekday-face))))
               (day-string (if (< day 10)
                               (format " %d " day)
                             (if (display-graphic-p)
                                 (concat (propertize " " 'display `(space :width (,(/ char-width 2))))
                                         (format "%2d" day)
                                         (propertize " " 'display `(space :width (,(- char-width (/ char-width 2))))))
                               (format "%2d " day))))
               )
          (if (or (< index first) (>= index last))
              (setq days (concat days "   "))
            (setq days (concat days
                   (propertize day-string
                               'date date
                               'current is-current
                               'face face))))))
      (when (< row 5)
        (setq days (concat days "\n"))))
    (concat
     (propertize
      (nano-calendar--center 20 (format "%s %d" (calendar-month-name month) year))
      'face 'nano-calendar-header-month-face)
     " \n"
     (propertize
      (mapconcat #'(lambda (day)
                     (if (display-graphic-p)
                         (let ((cw (frame-char-width)))
                           (concat (propertize " " 'display `(space :width (,(/ cw 2))))
                                   (substring (calendar-day-name day t t) 0 2)
                                   (propertize " " 'display `(space :width (,(- cw (/ cw 2)))))))
                       (substring (calendar-day-name day t t) 0 2)))
                 '(1 2 3 4 5) (if (display-graphic-p) "" " "))
      'face 'nano-calendar-header-weekday-face)
     (unless (display-graphic-p) " ")
     (propertize
      (mapconcat #'(lambda (day)
                     (if (display-graphic-p)
                         (let ((cw (frame-char-width)))
                           (concat (propertize " " 'display `(space :width (,(/ cw 2))))
                                   (substring (calendar-day-name day t t) 0 2)
                                   (propertize " " 'display `(space :width (,(- cw (/ cw 2)))))))
                       (substring (calendar-day-name day t t) 0 2)))
                 '(6 0) (if (display-graphic-p) "" " "))
      'face 'nano-calendar-header-weekend-face)
     (if (display-graphic-p) "\n" " \n")
     days)))


(defun nano-calendar-force-update-month-current ()
  "Force update of current month"

  (interactive)
  (let ((year (nano-calendar--date-year nano-calendar--current))
        (month (nano-calendar--date-month nano-calendar--current)))
    (nano-calendar-generate-month month year t))
  (nano-calendar-update)
  (nano-calendar-goto nano-calendar--current))

(defun nano-calendar-force-update-month-all ()
  "Force update of all months"

  (interactive)
  (let ((year (nano-calendar--date-year nano-calendar--current)))
    (dolist (month (number-sequence 1 12))
      (nano-calendar-generate-month month year t)))
  (nano-calendar-update)
  (nano-calendar-goto nano-calendar--current))

(defun nano-calendar--is-bold (row col grid)
  "Return where cell at (ROW,COL) is bold in GRID.
GRID must be a string (newlines separated) where each line describes
cells with 0 (normal) and 1 (bold). The number of columns and rows is
deduced from the GRID string.

Example: \"000\n010\n000\" for a 3x3 grid with a bold cell in the center.
"
  
  (let* ((lines (split-string grid))
         (rows (length lines))
         (cols (length (car lines)))
         (row (1- row))
         (col (1- col)))
    (if (and (>= col 0) (< col cols) (>= row 0) (< row rows))
        (substring (nth row lines) col (1+ col))
      "0")))

(defun nano-calendar--col-separator (row col grid)
  "This returns the left border for the given (ROW,COL) in GRID.
COL must be between 0 (left border) and the total number of cols (right
border) according to the provided GRID. ROW must be between 0 (top
border) and the total number of rows (bottom border) according to the
provided GRID.  GRID must be a string (newlines separated) where each
line describes cells with 0 (normal) and 1 (bold). The number of columns
and rows is deduced from the GRID string.

Example: \"000\n010\n000\" for a 3x3 grid with a bold cell in the center.
"
  (let ((bold-left (equal (nano-calendar--is-bold row col grid) "1"))
        (bold-right (or (equal (nano-calendar--is-bold row col grid) "1")
                        (equal (nano-calendar--is-bold row (1+ col) grid) "1")))
        (bold ))
    (if (or bold-left bold-right)
        "┃ "
      "│ ")))

(defun nano-calendar--col-separator (row col grid)
  "This returns the left border for the given (ROW,COL) in GRID.
COL must be between 0 (left border) and the total number of cols (right
border) according to the provided GRID. ROW must be between 0 (top
border) and the total number of rows (bottom border) according to the
provided GRID.  GRID must be a string (newlines separated) where each
line describes cells with 0 (normal) and 1 (bold). The number of columns
and rows is deduced from the GRID string.

Example: \"000\n010\n000\" for a 3x3 grid with a bold cell in the center.
"
  (cond ((eq col 1) "")
        (t " ")))

(defun nano-calendar--row-separator (row grid)
  "This returns the string for the given ROW in GRID.
ROW must be between 0 (top border) and the total number of rows (bottom
border) according to the provided GRID. GRID must be a string (newlines
separated) where each line describes cells with 0 (normal) and
1 (bold). The number of columns and rows is deduced from the GRID
string.

Example: \"000\n010\n000\" for a 3x3 grid with a bold cell in the center.
"

  (let* ((rows (length (split-string grid)))
         (cols (length (car (split-string grid))))
         (vlines 
          '(("00" . "│") ("0." . "│") (".0" . "│") ("01" . "┃")
            ("10" . "┃") ("1." . "┃") (".1" . "┃") ("11" . "┃")))          
         (hlines 
          '(("00" . "─") ("0." . "─") (".0" . "─") ("01" . "━")
            ("10" . "━") ("1." . "━") (".1" . "━") ("11" . "━")))           
         (intersections
          '(;; Corners
            ("1..." . "┛") ("0..." . "┘") (".1.." . "┗") (".0.." . "└")
            ("..1." . "┓") ("..0." . "┐") ("...1" . "┏") ("...0" . "┌")
            ;; Borders
            ("..11" . "┳") ("..00" . "┬") ("..01" . "┲") ("..10" . "┱")
            ("11.." . "┻") ("00.." . "┴") ("01.." . "┺") ("10.." . "┹")
            (".1.1" . "┣") (".0.0" . "├") (".1.0" . "┡") (".0.1" . "┢")
            ("1.1." . "┫") ("0.0." . "┤") ("1.0." . "┩") ("0.1." . "┪")
            ;; Intersections
            ("0000" . "┼") ("0001" . "╆") ("0010" . "╅") ("0011" . "╈")
            ("0100" . "╄") ("0101" . "╊") ("0110" . "╋") ("0111" . "╋")
            ("1000" . "╃") ("1001" . "╋") ("1010" . "╉") ("1011" . "╋")
            ("1100" . "╇") ("1101" . "╋") ("1110" . "╋") ("1111" . "╋"))))
         

    (let ((separator ""))
      (dolist (col (number-sequence 0 cols))
        (let* ((a (nano-calendar--is-bold row col grid))
               (b (nano-calendar--is-bold row (1+ col) grid))
               (c (nano-calendar--is-bold (1+ row) col grid))
               (d (nano-calendar--is-bold (1+ row) (1+ col) grid))
               (A (if (or (= col 0) (= row 0)) "." a))
               (B (if (or (= (1+ col) (1+ cols)) (= row 0)) "." b))
               (C (if (or (= col 0) (= (1+ row) (1+ rows))) "." c))
               (D (if (or (= (1+ col) (1+ cols)) (= (1+ row) (1+ rows))) "." d))               
               (ABCD (concat A B C D))
               (inter (alist-get ABCD intersections nil nil #'equal))               
               (BD (concat B D))
               (hline (alist-get BD hlines nil nil #'equal)))
          (setq separator (concat separator inter))
          (when hline
            (setq separator (concat separator (make-string 22 (aref hline 0)))))))
      (concat separator "\n"))))

(defun nano-calendar--row-separator (row grid)
  "This returns the string for the given ROW in GRID.
ROW must be between 0 (top border) and the total number of rows (bottom
border) according to the provided GRID. GRID must be a string (newlines
separated) where each line describes cells with 0 (normal) and
1 (bold). The number of columns and rows is deduced from the GRID
string.

Example: \"000\n010\n000\" for a 3x3 grid with a bold cell in the center.
"
  (let ((rows (length (string-split grid)))
        (cols (length (car (string-split grid)))))
    (if (eq row rows)
        ""
      "\n")))

(defun nano-calendar-update (&optional layout do-not-erase)
  "Insert calendar with given LAYOUT in current buffer that is first
 erased unless DO-NOT-ERASE is t."

  (interactive)

  (unless do-not-erase
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))))
  
  (let* ((curr nano-calendar--current)
         (prev (nano-calendar-backward-month nano-calendar--current))
         (first (nano-calendar-first-month nano-calendar--current))
         (layout (or layout nano-calendar-layout))
         (date (cond ((equal layout '(1 . 1)) curr)
                     ((equal layout '(1 . 2)) curr)
                     ((equal layout '(1 . 3)) prev)
                     ((equal layout '(3 . 1)) prev)
                     ((equal layout '(2 . 6)) first)
                     ((equal layout '(6 . 2)) first)
                     ((equal layout '(3 . 4)) first)
                     ((equal layout '(4 . 3)) first)))
         (grid (let ((date date)
                     (grid ""))
                 (dolist (row (number-sequence 1 (car layout)))
                   (dolist (col (number-sequence 1 (cdr layout)))
                     (if (eq (nano-calendar--date-month date)
                             (nano-calendar--date-month (nano-calendar-today)))
                         (setq grid (concat grid "1"))
                       (setq grid (concat grid "0")))
                     (setq date (nano-calendar-forward-month date)))
                    (setq grid (concat grid "\n")))
                 grid))
         (inhibit-read-only t))

    (insert (nano-calendar--row-separator 0 grid))
    (dolist (row (number-sequence 1 (car layout)))
      (dolist (col (number-sequence 1 (cdr layout)))
        (let* ((month (nano-calendar--date-month date))
               (year (nano-calendar--date-year date))
               (bold-left (equal (nano-calendar--is-bold row col grid) "1"))
               (bold-right (or (equal (nano-calendar--is-bold row col grid) "1")
                               (equal (nano-calendar--is-bold row (1+ col) grid) "1")))
               (bold (or bold-left bold-right))
               (lines (string-split (nano-calendar-generate-month month year) "\n")))
          (save-excursion
            (dolist (line lines)
              (cond ((eq col 1)
                     (insert (concat
                              (nano-calendar--col-separator row col grid)
                              line
                              (nano-calendar--col-separator row (1+ col) grid)
                              "\n")))
                    ((eq col (cdr layout))
                     (end-of-line)
                     (insert (concat line
                                     (nano-calendar--col-separator row col grid)
                                     ))
                     (forward-line))
                    (t
                     (end-of-line)                     
                     (insert (concat line
                                     (nano-calendar--col-separator row col grid)))
                     (forward-line))))))
        (setq date (nano-calendar-forward-month date)))
      (goto-char (point-max))
      (insert (nano-calendar--row-separator row grid)))
    (remove-text-properties (point-min) (point-max) '(current))
    ;; (goto-char (point-min))
    (fit-window-to-buffer nil nil (window-height))
    (run-hooks 'nano-calendar-update-hook)))

(defun nano-calendar-select-and-quit ()
  "Select date and quit calendar"

  (interactive)
  (if-let* ((window (active-minibuffer-window))
            (buffer (window-buffer window)))
      (exit-minibuffer)
    (kill-buffer)))

(defun nano-calendar-cancel-and-quit ()
  "Cancel selecton and quit calendar"

  (interactive)
  (setq nano-calendar--current nano-calendar--saved)

  (if-let* ((window (active-minibuffer-window))
            (buffer (window-buffer window)))
      (exit-minibuffer)
    (kill-buffer)))

(define-minor-mode nano-calendar-mode
  "Nano calendar mode"

  :keymap  `((,(kbd "<left>")    . nano-calendar-goto-prev)
             (,(kbd "<right>")   . nano-calendar-goto-next)
             (,(kbd "p")         . nano-calendar-goto-prev-day)
             (,(kbd "n")         . nano-calendar-goto-next-day)
             (,(kbd "P")         . nano-calendar-goto-prev-week)
             (,(kbd "N")         . nano-calendar-goto-next-week)
             (,(kbd "<up>")      . nano-calendar-goto-up)
             (,(kbd "<down>")    . nano-calendar-goto-down)
             (,(kbd "=")         . nano-calendar-palette-next)
             (,(kbd "<S-left>")  . nano-calendar-goto-prev-month)
             (,(kbd "<S-right>") . nano-calendar-goto-next-month)
             (,(kbd "<S-down>")  . nano-calendar-goto-next-year)
             (,(kbd "<S-up>")    . nano-calendar-goto-prev-year)
             (,(kbd ".")         . nano-calendar-goto-today)
             (,(kbd "r")         . nano-calendar-workloads-update)
             (,(kbd "SPC")       . nano-calendar-mark)
             (,(kbd "U")         . nano-calendar-unmark-all)
             (,(kbd "M-x")       . execute-extended-command)
             (,(kbd "<RET>")     . nano-calendar-select-and-quit)
             (,(kbd "<ESC>")     . nano-calendar-cancel-and-quit)
             (,(kbd "C-g")       . nano-calendar-cancel-and-quit)
             (,(kbd "j")         . nano-calendar-cancel-and-quit)
             (,(kbd "q")         . nano-calendar-cancel-and-quit))

  (when nano-calendar-mode
    (setq cursor-type nil
          truncate-lines t
          buffer-read-only t)))

(defun nano-calendar (&optional date layout buffer)
  "Display a calendar centered on DATE using specified LAYOUT and BUFFER."

  (interactive)
  (let ((buffer (or buffer (pop-to-buffer nano-calendar-buffer)))
        (layout (or layout nano-calendar-default-layout)))
    (switch-to-buffer buffer)
    (setq-local nano-calendar-layout layout))

  (setq nano-calendar--marked '())
  (setq nano-calendar--cached-month nil)
  (unless nano-calendar--workloads
    (nano-calendar--workloads-update))
  (unless nano-calendar--current
    (setq nano-calendar--current (nano-calendar-today)))
  (setq nano-calendar--saved nano-calendar--current)

  (let ((overlay (make-overlay (point-min) (point-min))))
    (setq nano-calendar--current-overlay overlay)
    (overlay-put overlay'face 'nano-calendar-current-face))
  (when date
    (setq nano-calendar--current date))

  (unless nano-calendar--workloads
    (nano-calendar--workloads-update))
  (nano-calendar-update)
  (nano-calendar-goto nano-calendar--current)
  (nano-calendar-mode 1)

  ;; Enforce spatial or chronological navigation
  (let ((keymap nano-calendar-mode-map))
    (if (eq nano-calendar-navigation-mode 'spatial)
        (progn
          (define-key keymap (kbd "<left>")  #'nano-calendar-goto-prev)
          (define-key keymap (kbd "<right>")  #'nano-calendar-goto-next))
      (progn
          (define-key keymap (kbd "<left>")  #'nano-calendar-goto-prev-day)
          (define-key keymap (kbd "<right>")  #'nano-calendar-goto-next-day))))

  (let ((inhibit-message t))
    (hl-line-mode 0))

  (run-hooks 'nano-calendar-hook))

(defun nano-calendar--minibuffer-setup ()
  (let* ((window (active-minibuffer-window))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (nano-calendar nil '(1 . 3) buffer)
      (setq-local resize-mini-windows 'grow-only
                  max-mini-window-height 1.0))))

(defun nano-calendar--minibuffer-date-changed ()
  (let* ((window (active-minibuffer-window))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (date nano-calendar--current)
             (date (concat (format-time-string "%A " date)
                           (string-trim (format-time-string "%e %B %Y" date)))))
        (goto-char (point-min))
        (insert (concat (propertize "Select a date: " 'face 'bold)
                        (propertize date 'face 'default)
                        (propertize "\n" 'display '(raise -0.5))))
      (goto-char (point-min))))))

(defun nano-calendar-prompt ()
  "Prompt for a date based on calendar selection"

  (interactive)
  (add-hook 'minibuffer-setup-hook #'nano-calendar--minibuffer-setup)
  (add-hook 'nano-calendar-date-changed-hook #'nano-calendar--minibuffer-date-changed)
  (setq nano-calendar--saved nano-calendar--current)
  (unwind-protect
      (let ((nano-calendar-navigation-mode 'chronological)
            (vertico-count 13))
        (read-from-minibuffer ""))
    (remove-hook 'minibuffer-setup-hook #'nano-calendar--minibuffer-setup)
    (remove-hook 'nano-calendar-date-changed-hook #'nano-calendar--minibuffer-date-changed))
  nano-calendar--current)

(defun nano-calendar-current ()
  "Current selected date"

  nano-calendar--current)

(provide 'nano-calendar)
;;; nano-calendar.el ends here
