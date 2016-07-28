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

(defun mapcar (fn list)
  (let ((newlist nil))
    (for ((rest list (cdr rest)))
         ((null rest)
          (nreverse newlist))
         (setq newlist
               (cons (funcall fn (car rest))
                     newlist)))))

(defun list (&rest list) list)

(defun first (x) (car x))
(defun second (x) (car (cdr x)))
