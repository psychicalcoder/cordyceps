;;; -*- lexical-binding: t -*-

(defconst cordyceps-test-dir (file-name-directory (or load-file-name buffer-file-name)))
(load (expand-file-name "cordyceps.el" cordyceps-test-dir) nil t)

(eval (macroexpand-all
       '(let ((cnt 0))
	  (np-defun counter (reset (advance t))
	    "Return the counter value and advance the counter. Arguments are optional
keyword/argument pairs.

:reset BOOL -- if BOOL is non-nil, set the counter to zero after return.
:advance BOOL -- BOOL is t by default; if BOOL is non-nil, advance the
counter after return (or reset).

\(fn &key RESET ADVANCE)"
	    (prog1 cnt
	      (when reset (setq cnt 0))
	      (when advance (setq cnt (1+ cnt)))))))
      t
      )

(eval (macroexpand-all
       '(np-defun two-compiler-macro (a b c)
	  "non"
	  (declare (compiler-macro (lambda (args)
				     "Use the later compiler macro")))
	  (if (= (- a b) 0)
	      c
	    (- a b))))
      t)

(macroexpand-all '(two-compiler-macro :a 1 :b 2 :c 3))
(macroexpand-all '(counter :reset 1))
