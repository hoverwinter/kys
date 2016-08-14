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
    (delete-other-windows)
    (kys-list-to-window-tree conf (selected-window)
			 (lambda (win name)
			   (set-window-buffer win (or (get-buffer name)
						      buf))))))


;; buffer manage



;; tests
(setq a (dump-window-tree))
(load-window-tree a)
