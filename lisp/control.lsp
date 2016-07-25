
(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
    `(if ,(car (car clauses))
         (progn ,@(cdr (car clauses)))
       (cond . ,(cdr clauses)))))

(defmacro case (keyform &rest cases)
  (let ((g (gensym)))
    `(let ((,g ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (if (eq t (car c))
                             `(t ,@(cdr c))
                           `((member ,g ',(car c))
                             ,@(cdr c))))
                       cases)))))

(defmacro while (test &rest body)
  (let ((gstart-tag (gensym)))
    `(tagbody
      ,gstart-tag
      (if ,test
          (progn
            ,@body
            (go ,gstart-tag))))))

(defmacro for (var-forms test-form &rest body)
  (let ((step-vars
         (mapcar (lambda (var-form)
                   (gensym))
                 var-forms))
        (gvalue (gensym))
        (gstart-tag (gensym))
        (gend-tag (gensym)))
    `(let ,(mapcar (lambda (var-form)
                     `(,(car var-form)
                       ,(car (cdr var-form))))
                   var-forms)
       (let ,(mapcar (lambda (step-var) `(,step-var nil)) step-vars)
         (let ((gvalue nil))
           (tagbody
            ,gstart-tag
            (if ,(car test-form)
                (progn
                  (setq ,gvalue (progn ,@(cdr test-form)))
                  (go ,gend-tag)))
            ,@body
            ,@(mapcan (lambda (var-form step-var)
                        (if (cdr (cdr var-form))
                            `((setq ,step-var
                                    ,(car (cdr (cdr var-form)))))))
                      var-forms
                      step-vars)
            ,@(mapcan (lambda (var-form step-var)
                        (if (cdr (cdr var-form))
                            `((setq ,(car var-form)
                                    ,step-var))))
                      var-forms
                      step-vars)
            (go ,gstart-tag)
            ,gend-tag)
           ,gvalue)))))
