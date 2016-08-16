;;; kys.el --- Keep Your Session
;; Copyright (C) 2016-2016
;;               by hoverwinter <hoverwinter@gmail.com>
;; Package-Requires: ((cl-lib "0.5"))

;; Introduction:
;;     This package provide the functionality that persists window & buffer
;;     information under `kys-directory' folder. You can resume the session
;;     afterwards even if you shut down the computer. You can also add tags
;;     to each session for convinence.
;;
;;     Functions are:  kys-session-[init/commit/reset/kill/delete/list]
;;     use `kys-session' to find current session.
;;     use `kys-session-init' to start a session.
;;     use `kys-session-reset' to resume a session, current session will be saved.
;;     use `kys-session-kill' to kill current session and save it.
;;     use `kys-session-delete' to delete a session.
;;     use `kys-session-list' to show all sessions.

;;  Any question or issues, please send a mail or
;;               view it on Github <https://github.com/hoverwinter/kys>


;; This file is NOT part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)

;; manage window tree info
(defun kys-window-tree-to-list (tree)
  "TREE is the output of `window-tree' except `minibuffer'. This function
transform the output into list. See also `kys-dump-window-tree'. 

This function shouldn't be used directly."
  (if (windowp tree)
      (buffer-name (window-buffer tree))
    (let ((dir (car tree))
	  (children (cddr tree)))
      (list
       (if dir 'v 'h)
       (if dir (kys-trans-window-height (car children))
	 (kys-trans-window-width (car children)))
       (kys-window-tree-to-list (car children))
       (if (> (length children) 2)
	   (kys-window-tree-to-list (cons dir (cons nil (cdr children))))
	 (kys-window-tree-to-list (cadr children)))))))

(defun kys-dump-window-tree ()
  "Return a list containing the current window tree info. The format looks like:
     ('h(orizontal)/'v(ertical) size-of-left-window xxx xxx ...)
where the first element is the split direction, second is the size used as the
argument of `split-winow'. The rest are simple name of buffer of window, or another
list with the given format."
  (kys-window-tree-to-list (car (window-tree))))

(defun kys-trans-window-width (win)
  "Obtain the width of window if the argument is `windowp', or calculate the width
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-width win)
    (let ((edge (cadr win)))
      (- (cl-third edge) (cl-first edge)))))

(defun kys-trans-window-height (win)
  "Obtain the height of window if the argument is `windowp', or calculate the height
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-height win)
    (let ((edge (cadr win)))
      (- (cl-fourth edge) (cl-second edge)))))

(defun kys-list-to-window-tree (conf win set-winbuf)
  "Resume the window from saved list CONF, WIN is `selected-window', on which performs the
split, SET-WINBUF is a function with parameter WIN & BUF, which associate them. This is part
of `kys-load-window-tree', and shouldn't be used directly."
  (if (atom conf)
      (funcall set-winbuf win conf)
    (let ((newwin (split-window win (cadr conf)
				(eq (car conf) 'h)))
	  (others (nthcdr 3 conf)))
      (kys-list-to-window-tree (cl-third conf) win set-winbuf)
      (kys-list-to-window-tree (if (> (length others) 2) others (car others)) newwin set-winbuf))))

(defun kys-load-window-tree (conf)
  "Perform the action that resume the window tree."
  (let ((buf (current-buffer)))
    (kys-list-to-window-tree conf (selected-window)
			 (lambda (win name)
			   (set-window-buffer win (or (get-buffer name)
						      buf))))))

(defun kys-kill-other-windows ()
  "Just close other windows, get ready for resume."
  (delete-other-windows))

;; manage buffer list
(defun kys-dump-buffer-list ()
  "Save the buffers and it's associate file into a list. Temp buffer and global buffer such as
`*Messages*', `*scratch*' are ignored."
  (let ((buffers (mapcar (lambda (buf)
	    (let ((fname (buffer-file-name buf))
		  (bname (buffer-name buf)))
	      (if fname (list bname fname)
		(if (or (string= (substring bname 0 1) " ") (string= (substring bname 0 1) "*"))
		    nil
		  bname)))) (buffer-list))))
  (cl-remove-if (lambda (x) (eq x nil)) buffers)))

(defun kys-kill-all-buffers ()
  "Just close all the buffers except `*Messages*' and `*scratch*'."
  (dolist (buf (buffer-list))
    (unless (or (string= (buffer-name buf) "*Messages*") (string= (buffer-name buf) "*scratch*"))
	(kill-buffer buf))))		

(defun kys-load-buffer-list (buffers)
  "Reload the buffers from the saved list, and rename the buffer if needed, if file of buffer has 
ben deleted, just open the buffer."
  (let ((buf (current-buffer)))
    (save-excursion
      (dolist (buf buffers)
	(if (listp buf)
	      (if (file-exists-p (cadr buf))
		  (with-current-buffer (find-file-noselect (cadr buf))
		    (rename-buffer (car buf)))
		(get-buffer-create (car buf)))
	  (get-buffer-create buf)))))
  (switch-to-buffer (if (listp (car buffers)) (caar buffers) (car buffers))))

;; Data structure of session
(defvar kys-current-session '() "The current session, session is a list (name wlist blist tags)")
(defvar kys-session-list '()) ;; just name and tags

;; DON'T use defstruct
(defun kys-make-session (sname wlst blst &optional tags)
  (list sname wlst blst tags))

;; Some macros for access
(defmacro kys-session-name (session)
  `(car ,session))

(defmacro kys-session-wlst (session)
  `(cl-second ,session))

(defmacro kys-session-blst (session)
  `(cl-third ,session))

(defmacro kys-session-tags (session)
  `(cl-fourth ,session))

;; Manage file, supported for persistent
(defvar kys-directory "~/.emacs.d/kys/" "The directory to save the session files")

(defun kys-write-session-to-file (lst fname)
  "Write the session LST to path FNAME. For detail pathname info, see `kys-expand-session-file-name'."
  (if (file-exists-p fname)
      (delete-file fname))
  (write-region (format "%S" lst) nil fname))

(defun kys-read-session-from-file (fname)
  "Read the session from FNAME. For detail pathname info, see `kys-expand-session-file-name'."
  (if (file-exists-p fname)
      (with-temp-buffer
	(insert-file-contents fname)
        (read (buffer-string)))))

(defun kys-expand-session-file-name (sname tags)
  "Expand the absolute path for saving session files. The path is generated by tags and session name.
All files are in `kys-directory', and sorted tag names for inner folder."
  (let ((sorted-tags (sort tags #'string<))
	path)
    (if (null sorted-tags)
	(expand-file-name (concat sname ".ssn") kys-directory)
      (progn
	(setf path (cl-reduce (lambda (taga tagb) (concat taga "/" tagb)) sorted-tags))
	(make-directory (expand-file-name path kys-directory) t)
	(expand-file-name (concat path "/" sname ".ssn") kys-directory)))))

(defun kys-walk-directory (dir)
  "Walk the directory and get all files. This is used for updating the session list. See also `kys-init'."
  (apply 'append
	 (delq nil
	       (mapcar (lambda (file)
			 (when (not (string-match "^[.]+$" (file-name-nondirectory file)))
			   (if (file-directory-p file)
			       (kys-walk-directory file)
			     (list file))))
		       (directory-files dir t)))))

(defun kys-obtain-item-from-absolute-path-name (pathname)
  "Gather tags and session name from absolute path. See also `kys-init' for usage."
  (let* ((rname (file-relative-name pathname kys-directory))
	 (fname (file-name-nondirectory rname))
	 (path (file-name-directory rname)))
    (unless (null path)
      (setf path (remove "" (split-string path "/"))))
    (unless (null fname)
      (append (list (substring fname 0 -4)) path))))

(defun kys-find-sname (sname)
  "Find the session name SNAME whether exists in `kys-session-list'."
  (if (cl-find-if
       (lambda (item) (string= (car item) sname))
       kys-session-list)
      t
    nil))

(defun kys-new-session (&optional name tags)
  "Make a new session, and add to `kys-session-list', then set it to the current session."
  (let* ((tags (delete-dups tags))
	 (session (kys-make-session
		  (if (null name) (make-temp-name "session-")  name)
		  (kys-dump-window-tree)
		  (kys-dump-buffer-list)
		  tags)))
    (push (cons name tags) kys-session-list)
    (setq kys-current-session session)))

(defun kys-read-session-by-name (sname)
  "Given the session name SNAME, read the dumped data from file."
  (let ((session-item
	 (cl-find-if (lambda (item)
		       (string= (car item) sname))
		     kys-session-list)))
    (if (null session-item)
	nil
      (kys-read-session-from-file
       (kys-expand-session-file-name (car session-item) (cdr session-item))))))

;; commands, user interface
(defun kys-session-init (sname tags)
  "Start a session with name SNAME and tags TAGS."
  (interactive "sSession Name: \nsTags(split by space): ")
  (if (kys-find-sname sname)
      (error "Session name has been used!")
    (if (not (null kys-current-session))
	(kys-session-commit))
    (kys-new-session sname (remove "" (split-string tags)))))

(defun kys-session-commit ()
  "Save the current session to file."
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (progn
      (setf (kys-session-wlst kys-current-session) (kys-dump-window-tree))
      (setf (kys-session-blst kys-current-session) (kys-dump-buffer-list))
      (kys-write-session-to-file kys-current-session (kys-expand-session-file-name
						      (kys-session-name kys-current-session)
						      (kys-session-tags kys-current-session))))))

(defun kys-session-reset (sname)
  "Switch to the session with name SNAME. If current session exists, save it."
  (interactive "sSession name: ")
  (unless (null kys-current-session)
      (kys-session-commit))
  (unless (kys-find-sname sname)
    (error "No session found"))
  (kys-kill-other-windows)
  (kys-kill-all-buffers)
  (let ((session (kys-read-session-by-name sname)))
    (when session
      (setf kys-current-session session)
      (kys-load-buffer-list (kys-session-blst session))
      (kys-load-window-tree (kys-session-wlst session)))))

(defun kys-session-list ()
  "Show the session list."
  (interactive)
  (message "%s" kys-session-list))

(defun kys-session-kill ()
  "Kill current session and save it."
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (progn
      (kys-session-commit)
      (setf kys-current-session nil))))

(defun kys-session-delete (&optional sname)
  "Delete the session SNAME dumped file, if this is current session, kill the session without saving."
  (interactive "sSession name (default current): ")
  (if (or kys-current-session (string< "" sname))
      (progn
	(setf sname (or (string< "" sname) (kys-session-name kys-current-session)))
	(let ((session-item (cl-find-if (lambda (item) (string= (car item) sname)) kys-session-list)))
	  (delete-file (kys-expand-session-file-name
			(car session-item)
			(cdr session-item)))
	  (if (string= (kys-session-name kys-current-session) sname)
	      (setf kys-current-session nil))
	  (remove session-item kys-session-list)
	  (message "Deleted: %s" sname)))
    (message "No session selected")))

(defun kys-session ()
  "Show current session."
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (message "<%s> with %s" (kys-session-name kys-current-session) (kys-session-tags kys-current-session))))

;; init
(defun kys-init ()
  "Initialize the folder and session list."
  (unless (file-exists-p kys-directory)
    (make-directory kys-directory))
  (let ((session-files (kys-walk-directory kys-directory)))
    (dolist (session-file session-files)
      (push (kys-obtain-item-from-absolute-path-name session-file) kys-session-list))))

(provide 'kys)
;;; kys.el ends here
