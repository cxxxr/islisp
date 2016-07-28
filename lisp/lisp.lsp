(defun repl ()
  (while t
    (print (eval (read)))))

(repl)
