;;; swimmers.el --- Draw a swimming-pool screensaver
;;; Time-stamp: <2007-08-25 23:32:21 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  unde r the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:
;; 


;;; History:
;; The old time stamp was: <96/09/06 14:19:16 john> so it's pretty old!

;;; Code:
(provide 'swimmers)

(defvar swim-delay .1
  "The delay between frames of the swimming pool animation.")

(defvar swimming-pool-colour "cyan"
  "The colour to draw for the water.")

(defvar swimmers-begin-hook nil
  "Hooks to run when starting the swimming screen saver.")

(defvar swimmers-end-hook nil
  "Hooks to run when finishing the swimming screen saver.")

(defun swevenp (a)
  "Return whether A is even."
  (zerop (mod a 2)))

(defun overwrite-rectangle-carelessly (rectangle)
  "Overwrite text of RECTANGLE with upper left corner at point.
Return whether it overwrote anything non-blank.
RECTANGLE's first line is overwritten at point,
its second line is overwritten at a point vertically under point, etc.
RECTANGLE should be a list of strings.
This version of overwrite-rectangle does not check about going off
the end of the line."
  (let* ((lines rectangle)
	 (linelength (length (car lines)))
	 (insertcolumn (current-column))
	 (first t)
	 (collided nil))
    (while lines
      (or first (forward-line 1))
      (move-to-column insertcolumn)
      (unless (and (eq (char-before (point)) ?\ )
		   (eq (char-after (+ (point) linelength)) ?\ ))
	(setq collided t))
      (delete-region (point) (+ (point) linelength))
      (insert (car lines))
      (setq lines (cdr lines)
	    first nil))
    collided))

(defun make-list-circular (list)
  "Replace the nil at the end of LIST with LIST."
  (let* ((p list)
	 (q (cdr p)))
    (while q
      (setq p q
	    q (cdr p)))
    (rplacd p list))
  list)

(defun colour-swimmer (swimmer)
  "Fill in the background colour on SWIMMER."
  (mapcar (lambda (swimmer-step)
	    (mapcar (lambda (row)
		      (when nil
			;; trying to draw trunks / bikinis by colouring!
			;; todo: need to do this by setting up a special face
			;; todo: do this for the top for only some of them
			(let ((i 0))
			  (while (setq i (string-match "[><+]" row i))
			    (put-text-property i (1+ i)
					       'face (cons 'foreground-color "red")
					       row)
			    (setq i (1+ i))))
			)
		      (setq row (propertize row
					    'face (cons 'background-color swimming-pool-colour)))
		      row)
		    swimmer-step))
	  swimmer))

;; These picture variables are lists of rectangles.

;; They should have two rows or columns of white space on their
;; trailing edge, so that the old picture gets overwritten without
;; having to erase it explicitly.

(defvar breast-stroke-right
  '(("  \\    / "
     "   >--<o "
     "  /    \\ ")
    ("  __   |  "
     "  __>--+o "
     "       |  ")
    ("      \\   "
     "  ==>-->o "
     "      /   ")
    ("  __   |  "
     "  __>--+o "
     "       |  "))
  "A list of rectangles showing an asciiperson swimming to the right.")

(defvar breast-stroke-left
  '(("\\    /   "
     "o>--<    "
     "/    \\   ")
    (" |   __  "
     "o+--<__  "
     " |       ")
    ("  /      "
     "o<--<==  "
     "  \\      ")
    (" |   __  "
     "o+--<__  "
     " |       "))
  "A list of rectangles showing an asciiperson swimming to the left.")

(defvar shark-right
  '(("          "
     "  __|\\__  "
     "          "))
  "A list of rectangles showing an asciishark swimming to the right.")

(defvar shark-left
  '(("          "
     " __/|__   "
     "          "))
  "A list of rectangles showing an asciishark swimming to the left.")

(defvar shark-victim
  '(("         "
     "         "
     "         "))
  "A list of rectangles showing a shark victim, not swimming.")

(defun make-swimmer (species stroke x y dx dy) (vector species stroke (nthcdr (mod (abs (random)) 4) stroke) x y dx dy))
(defmacro swimmer-species (a) (list 'aref a 0))
(defmacro swimmer-base-graphic (a) (list 'aref a 1))
(defmacro swimmer-graphic (a) (list 'aref a 2))
(defmacro swimmer-x (a) (list 'aref a 3))
(defmacro swimmer-y (a) (list 'aref a 4))
(defmacro swimmer-dx (a) (list 'aref a 5))
(defmacro swimmer-dy (a) (list 'aref a 6))
(defmacro set-swimmer-species (a v) (list 'aset a 0 v))
(defmacro set-swimmer-base-graphic (a v) (list 'aset a 1 v))
(defmacro set-swimmer-graphic (a v) (list 'aset a 2 v))
(defmacro set-swimmer-x (a v) (list 'aset a 3 v))
(defmacro set-swimmer-y (a v) (list 'aset a 4 v))
(defmacro set-swimmer-dx (a v) (list 'aset a 5 v))
(defmacro set-swimmer-dy (a v) (list 'aset a 6 v))

(makunbound 'swimmers)

(defvar swimmers nil
  ;; (list
  ;; 		  (make-swimmer 'human breast-stroke-right 12 16 1 0)
  ;; 		  (make-swimmer 'human breast-stroke-right 24 8 1 0)
  ;; 		  (make-swimmer 'human breast-stroke-right 36 4 1 0)
  ;; 		  (make-swimmer 'human breast-stroke-right 48 12 1 0)
  ;; 		  (make-swimmer 'shark shark-right 60 20 1 0)
  ;; 		  (make-swimmer 'shark shark-left 72 24 1 0))
  "The list of swimmers.")

(defvar swimming-with-sharks nil
  "Whether to include sharks.")

(defun make-random-swimmer-list ()
  "Make a list of swimmers for use in `swimming'."
  (setq swimming-pool-width (- (frame-width) (+ 4 (length (car (car breast-stroke-left)))))
	swimming-pool-height (- (frame-height) 5))
  (let ((swimlist nil)
	(xmax (- swimming-pool-width 3))
	(ymax (- swimming-pool-height 2))
	(y 2))
    (while (< y ymax)
      (setq swimlist
	    (let ((direction (swevenp (random)))
		  (shark (and swimming-with-sharks (swevenp (random)))))
	      (cons (make-swimmer
		     (if shark 'shark 'human)
		     (colour-swimmer
		      (if shark
			  (if direction shark-left shark-right)
			(if direction breast-stroke-left breast-stroke-right)))
		     (+ 3 (mod (abs (random)) xmax))
		     y
		     (* (if direction -1 1)
			(if (swevenp (random)) 1 2))
		     (if shark .1 0))
		    swimlist))
	    y (+ y 5)))
    swimlist))

(defvar swimming-pool-width 80
  "The width of the swimming pool.
Cached from a calculation based on the frame width.")

(defvar swimming-pool-width 24
  "The height of the swimming pool.
Cached from a calculation based on the frame height.")

(defun swimmers-move (swimmer-list)
  "Advance SWIMMER-LIST one step each."
  (dolist (swimmer swimmer-list)
    (let* ((species (swimmer-species swimmer))
	   (graphic (swimmer-graphic swimmer))
	   (x (swimmer-x swimmer))
	   (y (swimmer-y swimmer))
	   (dx (swimmer-dx swimmer))
	   (dy (swimmer-dy swimmer)))
      (setq x (+ x dx)
	    y (+ y dy)
	    graphic (cdr graphic))
      (when (>= y swimming-pool-height)
	(setq dy (- dy)
	      y swimming-pool-height))
      (when (<= y 2)
	(setq dy (- dy)
	      y 2))
      (when (>= x swimming-pool-width)
	(set-swimmer-base-graphic swimmer
				  (colour-swimmer (if (eq species 'shark)
						      shark-left
						    breast-stroke-left)))
	(setq dx (* dx -1)
	      graphic nil))
      (when (<= x 2)
	(set-swimmer-base-graphic swimmer
				  (colour-swimmer (if (eq species 'shark)
						      shark-right
						    breast-stroke-right)))
	(setq dx (* dx -1)
	      graphic nil))
      (when (null graphic)
	(setq graphic (swimmer-base-graphic swimmer)))
      (goto-line (floor y))
      (move-to-column x)
      (let ((collided (overwrite-rectangle-carelessly (car graphic))))
	(when (and collided (eq species 'human))
	  (set-swimmer-species swimmer nil)
	  (let ((swimming-pool-colour "red"))
	    (set-swimmer-base-graphic swimmer
				      (colour-swimmer shark-victim))
	    (set-swimmer-dx swimmer 0)
	    (set-swimmer-dy swimmer 0))))
      (set-swimmer-graphic swimmer graphic)
      (set-swimmer-x swimmer x)
      (set-swimmer-dx swimmer dx)
      (set-swimmer-y swimmer y)
      (set-swimmer-dy swimmer dy))))

(defvar swimming-in-pool t
  "Whether to draw a border round the pool.")

(defun swimming ()
  "Display a simple swimming pool."
  (interactive)
  (let ((pool (get-buffer-create "*Swimming pool*"))
	(old-cursor-colour (frame-parameter nil 'cursor-color)))
    (save-window-excursion
      (setq swimmers (make-random-swimmer-list))
      (switch-to-buffer pool)
      (delete-other-windows nil)
      (set-buffer pool)
      (buffer-disable-undo pool)
      (erase-buffer)
      (run-hooks 'swimmers-begin-hook)
      ;; todo: option of sea swimming -- no border, different water colour, shark attacks
      (let* ((i (- (frame-height) 4))
	     (w (frame-width))
	     (s (if swimming-in-pool
		    (concat "|" (make-string (- w 4) ?\ ) "|")
		  (make-string (- w 2) ?\ )))
	     (e (if swimming-in-pool
		    (concat "+" (make-string (- w 4) ?-) "+")
		  (make-string (- w 2) ?\ ))))
	(insert e 10)
	(while (> i 0)
	  (insert s 10)
	  (setq i (1- i)))
	(insert e))
      (put-text-property (point-min) (point-max) '
			 face (cons 'background-color swimming-pool-colour))
      (set-cursor-color swimming-pool-colour)
      (message "Press any key to return to editing")
      (while (not (input-pending-p))
	(swimmers-move swimmers)
	(sit-for swim-delay))
      (read-char)
      (run-hooks 'swimmers-end-hook)
      (set-cursor-color old-cursor-colour)
      (bury-buffer pool))))

(defun swimming-in-sea ()
  "Like `swimming' but without the pool, and with sharks."
  (interactive)
  (let ((swimming-in-pool nil)
	(swimming-with-sharks t)
	(swimming-pool-colour "sea green"))
    (swimming)))

(defun type-break-demo-swimmers ()
  "Display a swimming pool as a `type-break' activity."
  (swimming))

;;; end of swimmers.el

(provide 'swimmers)

;;; swimmers.el ends here
