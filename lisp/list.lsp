(defun member (x list)
  (for ((rest list (cdr rest))
        (result nil))
       ((or result (null rest))
        result)
    (if (eql x (car rest))
        (setq result rest))))

(defun null (x)
  (if x nil t))

(defun is:list-length (list)
  (for ((rest list (cdr rest))
        (count 0 (+ 1 count)))
       ((null rest) count)))

(defun assoc (key alist)
  (block nil
         (for ((rest alist (cdr rest)))
              ((null rest) nil)
              (if (eql key (car (car rest)))
                  (return-from nil rest)))))
