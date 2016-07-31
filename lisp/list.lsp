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

(defun is:mapcar* (fn list)
  (let ((newlist nil))
    (for ((rest list (cdr rest)))
         ((null rest)
          (nreverse newlist))
      (setq newlist
            (cons (funcall fn (car rest))
                  newlist)))))

(defun mapcar (fn &rest lists)
  (labels ((endp (lists)
                 (for ((rest lists (cdr rest))
                       (result nil))
                      ((or result (null rest)) result)
                   (if (null (car rest))
                       (setq result t))))
           (f (lists)
              (if (endp lists)
                  nil
                  (cons (apply fn (is:mapcar* #'car lists))
                        (f (is:mapcar* #'cdr lists))))))
    (f lists)))

(defun list (&rest list) list)

(defun list* (&rest args)
  (if (null args)
      nil
      (if (null (cdr args))
          (car args)
          (cons (car args)
                (list* (cdr args))))))

(defun first (x) (car x))
(defun second (x) (car (cdr x)))
(defun third (x) (car (cdr (cdr x))))
(defun fourth (x) (car (cdr (cdr (cdr x)))))
(defun fifth (x) (car (cdr (cdr (cdr (cdr x))))))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
