(defdynamic eval:env-var nil)
(defdynamic eval:env-fun nil)

(defglobal eval:fun-tag (gensym))
(defglobal eval:mac-tag (gensym))

(defun eval (x)
  (cond ((or (eq x t) (eq x nil))
         x)
        ((symbolp x)
         (let ((res (assoc x (dynamic eval:env-var))))
           (if res
               (cdr res)
               (error "unbound variable: %A" x))))
        ((not (consp x))
         x)
        ((symbolp (car x))
         (let ((args (cdr x)))
           (case (car x)
             ((function)
              (is:symbol-function (first args)))
             ((if)
              (if (eval* (first args))
                  (eval* (second args))
                  (eval* (third args))))
             ((progn)
              (for ((rest args (cdr rest))
                    (value nil))
                   ((null rest) value)
                (setq value (eval* (car rest)))))
             ((quote)
              (first args))
             ((setq)
              (let ((elt (assoc (first args) (dynamic eval:env-var))))
                (if elt
                    (set-cdr (eval* (second args))
                             elt)
                    (error "unbound variable: %A" (first args)))))
             ((defmacro)
              (set-property `(,eval:mac-tag ,(cadr args) . ,(cddr args))
                            (car args)
                            'function))
             ((defun)
              (set-property `(,eval:fun-tag ,(cadr args) . ,(cddr args))
                            (car args)
                            'function))
             (t
              (let ((funcbody (property (car x) 'function)))
                (if funcbody
                    (dynamic-let ((eval:env-var
                                   (nconc (mapcar (lambda (parm arg)
                                                    (cons parm (eval* arg)))
                                                  (car funcbody)
                                                  args)
                                          (dynamic eval:env-var))))
                      (eval* (cdr funcbody)))
                    (let ((func (is:symbol-function (car x))))
                      (if func
                          (apply func (mapcar #'eval args))
                          (error "undefined function: %A" (car x))))))))))
        ((and (consp (car x))
              (eq 'lambda (car (car x))))
         (let* ((lambda-form (car x))
                (lambda-list (cadr lambda-form))
                (lambda-body (cddr lambda-form))
                (args (cdr x)))
           ))
        (t
         (error "illegal form: %A" x))))
