;;; -*- lexical-binding: t -*-

(eval-and-compile
  (defun cordyceps--normalize-args (args)
    (unless (listp args)
      (error "Invalid argument %s, should be a list" args))
    (mapcar
     (lambda (arg)
       (cond
	((symbolp arg)
	 (cons arg nil))
	((and (consp arg)
	      (symbolp (car arg)))
	 (cons (car arg) (cadr arg)))
	(t (error "Invalid argument %s, should be a symbol or a list" arg))))
     args)))

(defmacro np-defun (name args &rest body)
  (declare (doc-string 3) (indent 2))
  (let* ((name1 (intern (concat (symbol-name name) "--internal")))
	 (cmname (intern (concat (symbol-name name) "--optimizer")))
	 (nargs (cordyceps--normalize-args args))
	 (argsyms (mapcar #'car nargs))
	 (defaults (mapcar (lambda (narg)
			    (cons (intern (concat ":" (symbol-name (car narg))))
				  (cdr narg)))
			  nargs)))
    `(progn
       (defun ,name1 (,@argsyms)
	 ,@body)

       (eval-and-compile
	 (defun ,cmname (_wholeform &rest arglist)
	   (let ((rargs arglist)
		 (defargs ',defaults)
		 (bindings ()) ;; tmp -> form ()) ;; (key . tmp)
		 passargs
		 )
	     ;; check the arglist is a well-formed plist whose keys
	     ;; are all valid
	     (while rargs
	       ;; key and value are not paired
	       (unless (cdr rargs)
		 (error "Invalid arguments %s" arglist))
	       ;; the key is a valid key
	       (let* ((key (car rargs))
		      (val (cadr rargs))
		      (defarg (assq key defargs))
		      (tmp (gensym)))
		 (if defarg
		     (progn
		       (push (list tmp val) bindings)
		       (push (cons key tmp) provider))
		   (error "Invalid key %s" key)
		   ))
	       (setq rargs (cddr rargs)))

	     ;; also let-bind deafult arguments
	     (mapc #'(lambda (kv)
		       (let ((k (car kv))
			     (v (cdr kv)))
			 (unless (plist-member arglist k)
			   (let ((tmp (gensym)))
			     (push (list tmp v) bindings)
			     (push (cons k tmp) provider)))))
		   defargs)

	     (setq bindings (nreverse bindings)
		   passargs (mapcar #'(lambda (kv)
					(let ((k (car kv)))
					  (cdr (assq k provider))))
				    defargs))
	     `(let (,@bindings)
		(,',name1 ,@passargs)))))

       (defun ,name (&rest arglist)
	 (declare (compiler-macro ,cmname))
	 (let ((rargs arglist)
	       (defargs ',defaults))
	   (while rargs
	     (unless (cdr rargs)
	       (error "Invalid arguments %s" arglist))
	     (let ((key (car rargs)))
	       (unless (assq key defargs)
		 (error "Invalid key %s" key)))
	     (setq rargs (cddr rargs)))
	   (apply #',name1
		  (mapcar #'(lambda (kv)
			      (let ((k (car kv))
				    (v (cdr kv)))
				(if (plist-member arglist k)
				    (plist-get arglist k)
				  v)))
			  defargs
			  )))))))

;;; test

(np-defun test-np-defun ((a 1) b (c 3))
  "Test Np defun"
  (message "%S %S %S" a b c))

(defun test-np-defun-hello ()
  (interactive)
  (test-np-defun :a 4 :b 5 :c 6))

