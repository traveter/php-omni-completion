;;; php-omni-completion-adodb.el --- php-omni-completion

;; Copyright (C) 2013

;; Author:  traveler

(eval-when-compile (require 'cl))

(defvar php-omni-completion-dictionary-dir (concat (file-name-directory load-file-name) "dic/"))

(defun php-omni-completion-load-file (filename)
  (if (file-readable-p filename)
      (ignore-errors
	(with-temp-buffer
	  (insert-file-contents filename)
	  (mapcar (lambda (str) (split-string str "\t"))
		  (split-string (buffer-string) "\n" t))
	  ))))

(defun php-omni-make-completion-alist (arg-alist)
  (loop with class-name
	with alist = '()
	for list in arg-alist
	for car = (car list)
	and cdr = (cdr list)
	do (if (string= car "")
	       (push cdr (cdr (assoc class-name alist)))
	     (setq class-name car)
	     (push (cons class-name (list cdr)) alist))
	finally return alist))

(defun php-omni-completion-alist (list)
  (let (l)
    (mapcar (lambda (e)
	      (let ((car (car e))
		    (cdr (cdr e)))
		(if (assoc car l)
		    (push cdr (cdr (assoc car l)))
		  (push (cons car (list cdr)) l))))
	    list)
    l))

(setq php-omni-class-tag-list (php-omni-make-completion-alist
			       (php-omni-completion-load-file
				(concat php-omni-completion-dictionary-dir "php-class-tag"))))
(setq php-omni-function-tag-list (php-omni-completion-load-file
	    (concat php-omni-completion-dictionary-dir "php-function-tag")))

(setq php-omni-class-candidates	(mapcar 'car php-omni-class-tag-list))
(setq php-omni-function-candidates (mapcar 'car php-omni-function-tag-list))

(defun php-omni-document ()
  (interactive)
  (popup-tip (php-omni-function-doc (current-word))))

(define-key php-mode-map (kbd "C-0") 'php-omni-document)

(defun php-omni-get-instance-variable-name ()
  (let* (
	 (current-class (save-excursion
			  (if (re-search-backward "\\(\\sw+\\)->" (point-at-bol) t)
			      (current-word))))
	 (alist (save-excursion (php-imenu-class-declaration-create-index)))
	 (cons (assoc current-class alist))
	 )
    (if (consp cons)
	(car (nth 0 (cdr cons))))))

(defun php-omni-function-doc (symbol)
  (let* ((match-class-function-list (cdr (assoc (php-omni-get-instance-variable-name)
				      php-omni-class-tag-list)))
	 (symbol-match-list (or (assoc symbol php-omni-function-tag-list)
				(assoc symbol match-class-function-list))))
    (if (and (consp symbol-match-list)
    	     (listp (cdr symbol-match-list)))
    	(mapconcat 'identity (cdr symbol-match-list) "\n\n"))))

(setq php-imenu-generic-expression
      (let ((modifier '("var" "public" "private" "protected")))
	(append (list
		 (list "Class Variable" (concat (regexp-opt modifier t) " \\(\\sw+\\)") 2)
		 ) php-imenu-generic-expression)))

(defun change-alist-recursive (alist append)
  (if (consp (assoc (car append) alist))
      (if (consp (cdr append))
  	  (change-alist-recursive (assoc (car append) alist) (nth 1 append))
  	)
    (setcdr (last alist) (cons append nil))
    ))

(defun php-imenu-instance-function-create-index ()
  (loop initially (goto-char (point-min))
	with alist = '()
	while (re-search-forward "\\($\\sw+\\)\\(?:->\\sw+\\)+(" (point-max) t)
	unless (member (get-text-property (point) 'face)
		       '(font-lock-comment-face font-lock-comment-delimiter-face))
	for append-cons = (loop	initially (goto-char (match-end 0))
				with first = (match-string-no-properties 1)
				with mark = (point-marker)
				with cons = '()
				with match-e = (match-end 1)
				while (re-search-backward "->\\(\\sw+\\)" match-e t)
				for cdr = mark then (list cons)
				do (setq cons (cons (match-string-no-properties 1) cdr))
				finally return (cons first (list cons)))
	;; if append-cons
	do (if alist
	       (change-alist-recursive alist append-cons)
	     (setq alist (list append-cons)))
	finally return alist))

(defun php-imenu-class-declaration-create-index ()
  (loop initially (goto-char (point-min))
	with alist = '()
	while (re-search-forward "\\($[[:alpha:]][[:alnum:]]*\\)\\s-*=\\s-*new\\s-\\([[:alpha:]][[:alnum:]]*\\)" (point-max) t)
	unless (member (get-text-property (point) 'face)
		       '(font-lock-comment-face font-lock-comment-delimiter-face))
	for append-cons = (cons (match-string-no-properties 1)
				(list (cons (match-string-no-properties 2) (point-marker))))
	if append-cons
	do (if alist
	       (change-alist-recursive alist append-cons)
	     (setq alist (list append-cons)))
	finally return alist))

(defun php-imenu-create-index ()
  (append (list (cons "Instance Functions" (php-imenu-instance-function-create-index))
  		(cons "Class Declaration" (php-imenu-class-declaration-create-index)))
	  (imenu-default-create-index-function)))

(defun php-ac-imenu-candidates ()
  (loop with i = 0
  	with match-list = (save-excursion
  			    (reverse (loop with bolp = (point-at-bol)
  					   while (re-search-backward "\\(\\sw\\)->" bolp t)
  					   collect (current-word))))
  	with stack = (assoc "Instance Functions" (php-omni-get-ac-imenu-index))
  	with result
  	while (and stack (or (not (integerp ac-limit))
  			     (< i ac-limit)))
  	for node = (pop stack)
  	if (consp node)
  	do
  	(let ((car (car node))
  	      (cdr (cdr node)))
  	  (if (stringp car)
  	      (when (string= car (car match-list))
  		(pop match-list)
  		(if (null match-list)
  		    (if (listp cdr)
  			(setq result (mapcar 'car cdr)))
  		  (mapc (lambda (c) (push c stack)) cdr)
  		  ))
  	    (incf i)
  	    ))
  	finally return (nreverse result)))

(defun php-omni-get-ac-imenu-index ()
  (unless (local-variable-p 'ac-imenu-index)
    (make-local-variable 'ac-imenu-index))
  (or ac-imenu-index
      (setq ac-imenu-index
  	    (ignore-errors
  	      (with-no-warnings
  		(imenu--make-index-alist))))))

(defun php-omni-class-instance-candidates ()
    (mapcar 'car (cdr (assoc (php-omni-get-instance-variable-name) php-omni-class-tag-list))))

(ac-define-source php-omni-class
  '((depends imenu)
    (candidates . php-omni-class-candidates)
    (symbol . "class")
    (prefix . " new \\(.*\\)")))

(ac-define-source php-omni-function
  '((depends imenu)
    (candidates . php-omni-function-candidates)
    (symbol . "f")
    (document . php-omni-function-doc)
    ))

(ac-define-source php-imenu
  '((depends imenu)
    (candidates . php-ac-imenu-candidates)
    (symbol . "s")
    (prefix . "\\sw+->\\(.*\\)")
    (requires . 0)))

(ac-define-source php-omni-class-instance
  '((depends imenu)
    (candidates . php-omni-class-instance-candidates)
    (symbol . "Instance")
    (document . php-omni-function-doc)
    (prefix . "\\(?:$\\sw+\\)->\\(.*\\)")
    (requires . 0)))

(defun php-omni-set-ac-source ()
  (setq imenu-create-index-function 'php-imenu-create-index)
  (setq ac-sources (append '(ac-source-php-omni-function
			     ac-source-php-omni-class
			     ac-source-php-imenu
			     ac-source-php-omni-class-instance
			     )
			   ac-sources)))

(add-hook 'php-mode-hook 'php-omni-set-ac-source)

(provide 'php-omni-completion)
;;; php-omni-completion.el ends here
