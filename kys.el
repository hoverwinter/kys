;; KYS -- Keep Your Session


;; window manage
(defun kys-window-tree-to-list (tree)
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
  (kys-window-tree-to-list (car (window-tree))))

(defun kys-trans-window-width (win)
  (if (windowp win)
      (window-width win)
    (let ((edge (cadr win)))
      (- (third edge) (first edge)))))

(defun kys-trans-window-height (win)
  (if (windowp win)
      (window-height win)
    (let ((edge (cadr win)))
      (- (fourth edge) (second edge)))))

(defun kys-list-to-window-tree (conf win set-winbuf)
  (if (atom conf)
      (funcall set-winbuf win conf)
    (let ((newwin (split-window win (cadr conf)
				(eq (car conf) 'h)))
	  (others (nthcdr 3 conf)))
      (kys-list-to-window-tree (third conf) win set-winbuf)
      (kys-list-to-window-tree (if (> (length others) 2) others (car others)) newwin set-winbuf))))

(defun kys-load-window-tree (conf)
  (let ((buf (current-buffer)))
    ;(delete-other-windows)
    (kys-list-to-window-tree conf (selected-window)
			 (lambda (win name)
			   (set-window-buffer win (or (get-buffer name)
						      buf))))))

(defun kys-kill-other-windows ()
  (delete-other-windows))

;; buffer manage
(defun kys-dump-buffer-list ()
  (let ((buffers (mapcar (lambda (buf)
	    (let ((fname (buffer-file-name buf))
		  (bname (buffer-name buf)))
	      (if fname (list bname fname)
		(if (or (string= (substring bname 0 1) " ") (string= (substring bname 0 1) "*"))
		    nil
		  bname)))) (buffer-list))))
  (remove-if (lambda (x) (eq x nil)) buffers)))

(defun kys-kill-all-buffers ()
  (dolist (buf (buffer-list))
    (unless (or (string= (buffer-name buf) "*Messages*") (string= (buffer-name buf) "*scratch*"))
	(kill-buffer buf))))		

(defun kys-load-buffer-list (buffers)
  ;(kys-kill-all-buffers)
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

;; session manage
(defvar kys-current-session '())
(defvar kys-session-list '()) ;; just name and tags

;; DON'T use defstruct
(defun kys-make-session (sname wlst blst &optional tags consist)
  (list sname wlst blst tags consist))

(defmacro kys-session-name (session)
  `(car ,session))

(defmacro kys-session-wlst (session)
  `(second ,session))

(defmacro kys-session-blst (session)
  `(third ,session))

(defmacro kys-session-tags (session)
  `(fourth ,session))

(defmacro kys-session-consist (session)
  `(fifth ,session))

;; file manage
(defvar kys-directory "~/.emacs.d/kys/")

(defun kys-write-session-to-file (lst fname)
  (if (file-exists-p fname)
      (delete-file fname))
  (write-region (format "%S" lst) nil fname))

(defun kys-read-session-from-file (fname)
  (if (file-exists-p fname)
      (with-temp-buffer
	(insert-file-contents fname)
	(let ((session (read (buffer-string))))
	  (setf (kys-session-consist session) t)
	  session))))

(defun kys-expand-session-file-name (sname tags)
  (let ((sorted-tags (sort tags #'string<))
	path)
    (if (null sorted-tags)
	(expand-file-name (concat sname ".ssn") kys-directory)
      (progn
	(setf path (reduce (lambda (taga tagb) (concat taga "/" tagb)) sorted-tags))
	(make-directory (expand-file-name path kys-directory) t)
	(expand-file-name (concat path "/" sname ".ssn") kys-directory)))))

(defun kys-walk-directory (dir)
  (apply 'append
	 (delq nil
	       (mapcar (lambda (file)
			 (when (not (string-match "^[.]+$" (file-name-nondirectory file)))
			   (if (file-directory-p file)
			       (kys-walk-directory file)
			     (list file))))
		       (directory-files dir t)))))

(defun kys-obtain-item-from-absolute-path-name (pathname)
  (let* ((rname (file-relative-name pathname kys-directory))
	 (fname (file-name-nondirectory rname))
	 (path (file-name-directory rname)))
    (setf path (remove "" (split-string path "/")))
    (append (list (substring fname 0 -4)) path)))

(defun kys-find-sname (sname)
  (if (find-if (lambda (item) (string= (car item) sname))
	       kys-session-list)
      t
    nil))

(defun kys-new-session (&optional name tags)
  (let* ((tags (delete-dups tags))
	 (session (kys-make-session
		  (if (null name) (make-temp-name "session-")  name)
		  (kys-dump-window-tree)
		  (kys-dump-buffer-list)
		  tags
		  nil)))
    (push (cons name tags) kys-session-lists)
    (setq kys-current-session session)))

(defun kys-read-session-by-name (sname)
  (let ((session-item (find-if (lambda (item)
				 (string= (car item) sname))
			       kys-session-list)))
    (if (null session-item)
	nil
      (kys-read-session-from-file (kys-expand-session-file-name (car session-item) (cdr session-item))))))

;; interface

(defun kys-session-commit ()
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (progn
      (kys-write-session-to-file kys-current-session (kys-expand-session-file-name
						      (kys-session-name kys-current-session)
						      (kys-session-tags kys-current-session)))
      (setf (kys-session-consist kys-current-session) t))))

(defun kys-session-init (sname tags)
  (interactive "sSession Name: \nsTags(split by space): ")
  (if (kys-find-sname sname)
      (error "Session name has been used!")
    (if (not (null kys-current-session))
      (progn
	(kys-session-update)
	(kys-session-commit)))
    (kys-new-session sname (remove "" (split-string tags)))))

(defun kys-session-update ()
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (progn
      (setf (kys-session-wlst kys-current-session) (kys-dump-window-tree))
      (setf (kys-session-blst kys-current-session) (kys-dump-buffer-list))
      (setf (kys-session-consist kys-current-session) nil))))

(defun kys-session-reset (sname)
  (interactive "sSession Name: ")
  (unless (null kys-current-session)
    (progn
      (kys-session-update)
      (kys-session-commit)))
  (kys-kill-other-windows)
  (kys-kill-all-buffers)
  (let ((session (kys-read-session-by-name sname)))
    (when session
      (setf kys-current-session session)
      (kys-load-buffer-list (kys-session-blst session))
      (kys-load-window-tree (kys-session-wlst session)))))

(defun kys-session-list ()
  (interactive)
  (message "%s" kys-session-list))

(defun kys-session-kill ()
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (progn
      (kys-session-update)
      (kys-session-commit)
      (setf kys-current-session nil))))
  
(defun kys-session-delete ()
  (interactive)
  (if (not (null kys-current-session))
      (progn
	(setf kys-session-list
	      (remove-if
	       (lambda (item)
		 (string= (car item) (kys-session-name kys-current-session)))
	       kys-session-list))
	(delete-file (kys-expand-session-file-name (kys-session-name kys-current-session)
						   (kys-session-tags kys-current-session)))
	(setf kys-current-session nil))
    (message "No session selected")))
  
(defun kys-session ()
  (interactive)
  (if (null kys-current-session)
      (message "No session selected")
    (message "<%s> with %s" (capitalize (kys-session-name kys-current-session)) (kys-session-tags kys-current-session))))

(defun kys-init ()
  (unless (file-exists-p kys-directory)
    (make-directory kys-directory))
  (let ((session-files (kys-walk-directory kys-directory)))
    (dolist (session-file session-files)
      (push (kys-obtain-item-from-absolute-path-name session-file) kys-session-list))))

(kys-init)
(provide 'kys)
