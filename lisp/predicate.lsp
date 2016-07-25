(defun not (x)
  (if x nil t))

(defmacro and (&rest forms)
  (if (null forms)
      t
      `(if ,(car forms)
           (and . ,(cdr forms))
           nil)))

(defmacro or (&rest forms)
  (if (null forms)
      nil
      (let ((g (gensym)))
        `(let ((,g ,(car forms)))
           (if ,g
               ,g
               (or . ,(cdr forms)))))))
