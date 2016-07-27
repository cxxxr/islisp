(defmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

(defmacro return (x) `(return-from nil ,x))

(defun first (x) (car x))
(defun second (x) (car (cdr x)))
(defun third (x) (car (cdr (cdr x))))
(defun fourth (x) (car (cdr (cdr (cdr x)))))
(defun fifth (x) (car (cdr (cdr (cdr (cdr x))))))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

;; (defun list* (&rest args)
;;   (labels ((f (args)
;; 	     (cond ((null args)
;; 		    nil)
;; 		   ((null (cdr args))
;; 		    (car args))
;; 		   (t
;; 		    (cons (car args)
;; 			  (f (cdr args)))))))
;;     (f args)))

(defmacro dolist (var-and-list &rest body)
  (let ((var (car var-and-list))
        (list-form (cadr var-and-list))
        (result-form (if (cddr var-and-list) (third var-and-list)))
        (g (gensym)))
    `(block nil
       (for ((,g ,list-form (cdr ,g)))
            ((null ,g) ,result-form)
         (let ((,var (car ,g)))
           ,@body)))))

(defmacro dotimes (vars &rest forms)
   `(for ((,(car vars) 0 (+ 1 ,(car vars)))) ((>= ,(car vars) ,(second vars)) nil) . ,forms))

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))

(defmacro pop (place)
  (let ((g (gensym)))
    `(let ((,g (car ,place)))
       (setf ,place (cdr ,place))
       ,g)))

(defun remove-if (test list)
  (let ((acc nil))
    (dolist (x list)
      (unless (funcall test x)
	(push x acc)))
    (nreverse acc)))

(defun remove-if-not (test list)
  (let ((acc nil))
    (dolist (x list)
      (when (funcall test x)
        (push x acc)))
    (nreverse acc)))

(defun filter (fun list)
  (let ((acc nil))
    (dolist (x list)
      (let ((res (funcall fun x)))
        (when res
          (push res acc))))
    (nreverse acc)))

(defmacro incf (x)
  `(setf ,x (+ ,x 1)))

(defmacro decf (x)
  `(setf ,x (- ,x 1)))

(defun find-string (str list)
  (let ((result
	  (dolist (x list)
	    (when (string= str x)
	      (return t)))))
    result))

(defun make-ast (op &rest args)
  (cons op args))

(defun ast-op (ast) (car ast))
(defun ast-args (ast) (cdr ast))
(defun ast-arg1 (ast) (first (ast-args ast)))
(defun ast-arg2 (ast) (second (ast-args ast)))
(defun ast-arg3 (ast) (third (ast-args ast)))

(defun syntax-error (msg &rest args)
  (apply #'error msg args))

(defun env-get (env x)
  (let ((v (assoc x env)))
    (if v
        (cdr v)
        nil)))


(defdynamic *pass1-env* nil)
(defdynamic *pass1-fun-env* nil)
(defdynamic *pass1-tag-env* nil)
(defdynamic *pass1-name-env* nil)

(defun make-var (sym)
  (let ((gsym (gensym)))
    (set-property sym gsym 'name)
    gsym))

(defun pass1 (x)
  (cond ((member x '(t nil))
         (make-ast 'CONST x))
        ((symbolp x)
         (pass1-refvar x))
        ((not (consp x))
         (make-ast 'CONST x))
        (t
         (pass1-compound-form x))))

(defun pass1-refvar (s)
  (let ((v (env-get (dynamic *pass1-env*) s)))
    (if v
        (make-ast 'REF-LVAR v)
        (make-ast 'REF-GVAR s))))

(defun check-arg-count (form min max)
  (let ((argnum (length (cdr form))))
    (unless (if (= -1 max)
                (<= min argnum)
                (and (<= min argnum)
                     (<= argnum max)))
      (syntax-error "Wrong number of arguments for ~A in ~A: ~D supplied, ~A expected"
                    (car form)
                    form
                    argnum
                    (if (= max -1)
                        (string-append "more than " (convert min <string>))
                        (if (= min max)
                            min
                            (string-append min " to " max)))))))

(defun type-error (form v type-name)
  (syntax-error "Wrong type for ~A in ~A: ~A supplied, ~A expected"
		(car form)
		form
		type-name
		v))

(defun lambda-form-p (form)
  (and (consp form)
       (eq 'lambda (car form))
       (progn
	 (let ((new-lambda-list (pass1-lambda-list form (cadr form))))
           (set-car new-lambda-list (cdr form)))
	 t)))

(defun lambda-rest-symbol-p (x)
  (or (eq x ':rest)
      (eq x '&rest)))

(defun lambda-optional-symbol-p (x)
  (or (eq x ':optional)
      (eq x '&optional)))

(defun lambda-symbol-p (x)
  (or (lambda-rest-symbol-p x)
      (lambda-optional-symbol-p x)))

(defun pass1-lambda-list (form lambda-list)
  (let ((vars nil)
        (optional-vars nil)
        (rest-var nil))
    (tagbody
      (unless (listp lambda-list)
        (go :ERROR))
      (for ((rest lambda-list (cdr rest))
            (optional-p nil))
           ((null rest) (go :END))
        (let ((x (car rest)))
          (cond ((lambda-rest-symbol-p x)
                 (unless (and (= (length rest) 2)
                              (symbolp (cadr rest))
                              (not (lambda-symbol-p (cadr rest))))
                   (go :ERROR))
                 (setq rest-var (cadr rest))
                 (go :END))
                ((lambda-optional-symbol-p x)
                 (setq optional-p t))
                (optional-p
                 (cond ((symbolp x)
                        (push (list x (make-ast 'const nil)) optional-vars))
                       ((not (and (consp x) (= (length x) 2)))
                        (go :ERROR))
                       (t
                        (push (list (car x) (pass1 (cadr x))) optional-vars))))
                ((not (symbolp x))
                 (go :ERROR))
                (t
                 (push x vars)))))
      :ERROR
      (syntax-error "Illegal lambda list: ~A" form)
      :END)
    (list (nreverse vars)
          (nreverse optional-vars)
          rest-var)))

(defun replace-lambda-list-with-alist (alist lambda-list)
  (cond ((null lambda-list)
         nil)
        ((symbolp lambda-list)
         (let ((res (assoc lambda-list alist)))
           (if (null res) lambda-list (cdr res))))
        ((not (consp lambda-list))
         lambda-list)
        (t
         (cons (replace-lambda-list-with-alist alist (car lambda-list))
               (replace-lambda-list-with-alist alist (cdr lambda-list))))))

(defun lambda-list-vars (lambda-list)
  (append (first lambda-list)
          (mapcar #'car (second lambda-list))
          (if (third lambda-list)
              (list (third lambda-list)))))

(defun pass1-funcbody (form x make-ast-function)
  (let ((lambda-list (car x)))
    (setq lambda-list (pass1-lambda-list form lambda-list))
    (let* ((vars (lambda-list-vars lambda-list))
           (env1 (mapcar (lambda (v)
                           (cons v (make-var v)))
                         vars)))
      (funcall make-ast-function
               (replace-lambda-list-with-alist env1 lambda-list)
               (dynamic-let ((*pass1-env* (append env1 (dynamic *pass1-env*))))
                            (pass1-progn (cdr x)))))))

(defun dset-lambda-list (form lambda-list args)
  (let ((bindings nil))
    (dolist (v (first lambda-list))
      (push (list v (pop args))
            bindings))
    (dolist (v (second lambda-list))
      (push (list (car v)
                  (if (null args)
                      (cadr v)
                      (pop args)))
            bindings))
    (when (third lambda-list)
      (push (list (third lambda-list)
                  (cons 'list args))
            bindings))
    (nreverse bindings)))

(defun pass1-tagbody (x)
  (let* ((tags (remove-if-not #'symbolp (cdr x)))
         (gtags (mapcar (lambda (tag) (make-var tag)) tags))
         (env1 (mapcar #'cons tags gtags))
         (used-tags nil))
    (dynamic-let ((*pass1-tag-env*
                   (append env1 (dynamic *pass1-tag-env*))))
                 (make-ast 'TAGBODY
                           gtags
                           (mapcar (lambda (form)
                                     (cond ((symbolp form)
                                            (if (member form used-tags)
                                                (syntax-error "duplicated tag: ~A in ~A" form x)
                                              (push form used-tags))
                                            (env-get env1 form))
                                           (t
                                            (pass1 form))))
                                   (cdr x))))))

(defun pass1-go (x)
  (check-arg-count x 1 1)
  (let ((tag (second x)))
    (unless (symbolp tag)
      (type-error x tag '<symbol>))
    (let ((result (env-get (dynamic *pass1-tag-env*) tag)))
      (if result
          (make-ast 'GO result)
        (syntax-error "Go Tag not found: ~A" tag)))))

(defun pass1-labels (form definitions body flet-or-labels)
  (let ((fun-env1 nil))
    (dolist (def definitions)
      (check-arg-count def 2 -1)
      (unless (symbolp (car def))
        (type-error form (car def) '<symbol>))
      (push (cons (car def) (make-var (car def)))
            fun-env1))
    (let ((env (append fun-env1 (dynamic *pass1-fun-env*))))
      (make-ast flet-or-labels
                (mapcar #'cdr fun-env1)
                (dynamic-let ((*pass1-fun-env*
                               (if (eq flet-or-labels 'flet)
                                   (dynamic *pass1-fun-env*)
                                   env)))
                             (mapcar (lambda (def)
                                       (pass1-funcbody form
                                                       (cdr def)
                                                       (lambda (lambda-list body)
                                                         (list (env-get fun-env1 (car def))
                                                               lambda-list
                                                               body))))
                                     definitions))
                (dynamic-let ((*pass1-fun-env* env))
                             (pass1-progn body))))))

(defun pass1-progn (forms)
  (make-ast 'PROGN (mapcar #'pass1 forms)))

(defun pass1-compound-form (x)
  (case (car x)
    ((QUOTE)
     (check-arg-count x 1 1)
     (make-ast 'CONST (second x)))
    ((SETQ DEFGLOBAL)
     (check-arg-count x 2 2)
     (unless (symbolp (second x))
       (type-error x (second x) '<symbol>))
     (if (eq 'SETQ (first x))
         (let ((var (env-get (dynamic *pass1-env*) (second x)))
               (val (pass1 (third x))))
           (if var
               (make-ast 'SET-LVAR var val)
               (make-ast 'SET-GVAR (second x) val)))
         (make-ast 'SET-GVAR (second x) (pass1 (third x)))))
    ((LET)
     (let* ((binds
             (mapcar (lambda (b)
                       (unless (and (consp b)
                                    (= 2 (length b))
                                    (symbolp (car b)))
                         (syntax-error "Illegal let form ~A" x))
                       (list (make-var (cadr b))
                             (pass1 (cadr b))))
                     (cadr x)))
            (body
             (when (cddr x)
               (dynamic-let ((*pass1-env*
                              (append (mapcar (lambda (x y)
                                                (cons (car x)
                                                      (car y)))
                                              (cadr x)
                                              binds)
                                      (dynamic *pass1-env*))))
                            (pass1-progn (cddr x))))))
       (make-ast 'LET binds body)))
    ((FUNCTION)
     (check-arg-count x 1 1)
     (unless (second x)
       (type-error x (second x) '<symbol>))
     (let ((res (env-get (dynamic *pass1-fun-env*)
                         (second x))))
       (if res
           (make-ast 'LOCAL-FUNCTION res)
           (make-ast 'FUNCTION (second x)))))
    ((FLET LABELS)
     (check-arg-count x 1 -1)
     (pass1-labels x (cadr x) (cddr x) (car x)))
    ((LAMBDA)
     (check-arg-count x 1 -1)
     (pass1-funcbody x (cdr x)
                     (lambda (lambda-list body)
                       (make-ast 'LAMBDA lambda-list body))))
    ((DEFUN DEFMACRO)
     (check-arg-count x 2 -1)
     (unless (symbolp (second x))
       (type-error x (second x) '<symbol>))
     (cond ((eq 'DEFUN (first x))
            (pass1-funcbody x (cddr x)
                            (lambda (lambda-list body)
                              (make-ast 'DEFUN (second x) lambda-list body))))
           (t
            (set-property (cddr x) (second x) 'macro)
            (make-ast 'CONST (cadr x)))))
    ((IF)
     (check-arg-count x 2 3)
     (make-ast 'IF
               (pass1 (second x))
               (pass1 (third x))
               (if (cdr (cdr (cdr x)))
                   (pass1 (fourth x))
                 (make-ast 'CONST nil))))
    ((PROGN)
     (pass1-progn (cdr x)))
    ((TAGBODY)
     (pass1-tagbody x))
    ((GO)
     (pass1-go x))
    ((BLOCK)
     (check-arg-count x 1 -1)
     (unless (symbolp (second x))
       (type-error x (second x) '<symbol>))
     (let ((name (make-var (second x))))
       (make-ast 'BLOCK
                 name
                 (dynamic-let ((*pass1-name-env*
                                (cons (list (second x) name)
                                      (dynamic *pass1-name-env*))))
                              (pass1-progn (cddr x))))))
    ((RETURN-FROM)
     (check-arg-count x 2 2)
     (unless (symbolp (second x))
       (type-error x (second x) '<symbol>))
     (let ((res (env-get (dynamic *pass1-name-env*) (second x))))
       (if res
           (make-ast 'RETURN-FROM res (pass1 (third x)))
         (syntax-error "Block Tag not found: ~A" (second x)))))
    ((CATCH)
     (check-arg-count x 1 -1)
     (make-ast 'CATCH
               (pass1 (cadr x))
               (pass1-progn (cddr x))))
    ((THROW)
     (check-arg-count x 2 2)
     (make-ast 'THROW
               (pass1 (second x))
               (pass1 (third x))))
    ((UNWIND-PROTECT)
     (check-arg-count x 1 -1)
     (make-ast 'UNWIND-PROTECT
               (pass1 (cadr x))
               (pass1-progn (cddr x))))
    (t
     (cond
      ((lambda-form-p (car x))
       (let* ((lambda-form (car x))
              (args (cdr x))
              (bindings (dset-lambda-list x (cadr lambda-form) args)))
         (pass1 `(let ,bindings
                   ,@(cddr lambda-form)))))
      ((symbolp (car x))
       (let ((macro (property (car x) 'macro))
             (func nil))
         (cond (macro
                (pass1 (eval `((lambda ,@macro)
                               ,@(mapcar (lambda (x) `(quote ,x))
                                         (cdr x))))))
               ((setq func (env-get (dynamic *pass1-fun-env*) (car x)))
                (make-ast 'LCALL
                          func
                          (mapcar #'pass1 (cdr x))))
               (t
                (make-ast 'CALL
                          (car x)
                          (mapcar #'pass1 (cdr x)))))))
      (t
       (syntax-error "Illegal form ~A" x))))))


(defclass is-function ()
  ((name
    :initarg name
    :initform nil
    :accessor is-function-name)
   (lambda-list
    :initarg lambda-list
    :accessor is-function-lambda-list)
   (label
    :initarg label
    :accessor is-function-label)
   (code
    :initarg code
    :accessor is-function-code)))

(defclass context ()
  ((uniq-counter
    :initform 0
    :accessor context-uniq-counter)
   (functions
    :initform nil
    :accessor context-functions)
   (heap-vars
    :initform nil
    :initarg heap-vars
    :accessor context-heap-vars)
   (constant-list
    :initform nil
    :accessor context-constant-list)
   (jmpbuf-vars
    :initform nil
    :accessor context-jmpbuf-vars)))

(defun print-code (code)
  (dolist (instr code)
    (format (standard-output) "~A~%" instr)))

(defun print-context (ctx)
  (dolist (f (context-functions ctx))
    (format (standard-output)
            "~%~A ~A ~A:~%"
            (is-function-name f)
            (is-function-lambda-list f)
            (is-function-label f))
    (print-code (is-function-code f))))

(defun codegen-add-function (ctx is-function)
  (push is-function (context-functions ctx)))

(defun gen-uniq (ctx prefix)
  (string-append prefix
                 (convert (incf (context-uniq-counter ctx))
                          <string>)))


(defun codegen-genvar (ctx)
  (gen-uniq ctx "V"))

(defun codegen-newlabel (ctx)
  (gen-uniq ctx "L"))

(defun gen (op &rest args)
  (list (cons op args)))

(defun genseq (&rest code)
  (let ((acc nil))
    (dolist (instr code)
      (setq acc (append acc instr)))
    acc))

(defun codegen-add-heap-var (ctx sym)
  (set-property t sym 'heap-p)
  (dolist (elt (context-heap-vars ctx)
               (let ((var (codegen-genvar ctx)))
                 (push (cons sym var)
                       (context-heap-vars ctx))
                 var))
    (when (eq sym (car elt))
      (return (cdr elt)))))

(defun codegen (ctx x env)
  (case (ast-op x)
    ((CONST)
     (gen 'CONST (ast-arg1 x)))
    ((FUNCTION)
     (gen 'FUNCTION (ast-arg1 x)))
    ((LOCAL-FUNCTION)
     (gen 'LOCAL-FUNCTION (ast-arg1 x)))
    ((REF-GVAR)
     (gen 'GREF (ast-arg1 x)))
    ((SET-GVAR)
     (genseq (codegen ctx (ast-arg2 x) env)
             (gen 'GSET (ast-arg1 x))))
    ((REF-LVAR)
     (let ((var (env-get env (ast-arg1 x))))
       (if var
           (gen 'LREF var)
           (let ((var (codegen-add-heap-var ctx (ast-arg1 x))))
             (gen 'LREF var)))))
    ((SET-LVAR)
     (let ((var (env-get env (ast-arg1 x))))
       (if var
           (genseq (codegen ctx (ast-arg2 x) env)
                   (gen 'LSET var))
           (let ((var (codegen-add-heap-var ctx (ast-arg1 x))))
             (genseq (codegen ctx (ast-arg2 x) env)
                     (gen 'LSET var))))))
    ((LET)
     (codegen-let ctx (ast-arg1 x) (ast-arg2 x) env))
    ((LAMBDA)
     (codegen-lambda ctx (ast-arg1 x) (ast-arg2 x) env))
    ((DEFUN)
     (codegen-defun ctx (ast-arg1 x) (ast-arg2 x) (ast-arg3 x) env))
    ((LABELS)
     (codegen-labels ctx (ast-arg1 x) (ast-arg2 x) (ast-arg3 x) env))
    ((IF)
     (codegen-if ctx (ast-arg1 x) (ast-arg2 x) (ast-arg3 x) env))
    ((PROGN)
     (codegen-progn ctx (ast-arg1 x) env))
    ((TAGBODY)
     (codegen-tagbody ctx (ast-arg1 x) (ast-arg2 x) env))
    ((GO)
     (codegen-go ctx (ast-arg1 x) env))
    ((BLOCK)
     (codegen-block ctx (ast-arg1 x) (ast-arg2 x) env))
    ((RETURN-FROM)
     (codegen-return-from ctx (ast-arg1 x) (ast-arg2 x) env))
    ((CATCH)
     (codegen-catch ctx (ast-arg1 x) (ast-arg2 x) env))
    ((THROW)
     (codegen-throw ctx (ast-arg1 x) (ast-arg2 x) env))
    ((UNWIND-PROTECT)
     (codegen-unwind-protect ctx (ast-arg1 x) (ast-arg2 x) env))
    ((LCALL)
     (codegen-call ctx (ast-arg1 x) (ast-arg2 x) env t))
    ((CALL)
     (codegen-call ctx (ast-arg1 x) (ast-arg2 x) env nil))
    (t
     (error "unknown operator: ~A" (ast-op x)))))

(defun codegen-extend-env (env1)
  (let ((peeks nil)
        (count 0)
        (stack-offset (length env1)))
    (dolist (e env1)
      (when (property (car e) 'heap-p)
        (set-property count (car e) 'env-offset)
        (push stack-offset peeks)
        (incf count))
      (decf stack-offset))
    (when peeks
      (gen 'extend-env count (nreverse peeks)))))

(defun codegen-make-env1 (ctx symbols)
  (mapcar (lambda (sym)
            (cons sym (codegen-genvar ctx)))
          symbols))

(defun codegen-let (ctx bindings body env)
  (let ((env1
         (codegen-make-env1 ctx
                            (mapcar #'car bindings))))
    (let ((body-code (codegen ctx body (append env1 env))))
      (let ((peek-offset (length bindings))
            (stack-push-count 0))
        (genseq (gen 'BEGIN)
                (mapcan (lambda (b)
                          (codegen ctx (cadr b) env))
                        bindings)
                (codegen-extend-env env1)
                (mapcan (lambda (e)
                          (cond ((property (car e) 'heap-p)
                                 (gen 'HEAP-VAR (cdr e) (car e)))
                                (t
                                 (incf stack-push-count)
                                 (gen 'LOCAL-VAR (cdr e) (+ 1 (decf peek-offset))))))
                        env1)
                body-code
                (gen 'NIP stack-push-count)
                (gen 'END))))))

(defun codegen-lambda-list (vars env1 num-args)
  (let ((peek-offset num-args)
        (code nil))
    (dolist (v vars)
      (setq code
            (genseq code
                    (if (property v 'heap-p)
                        (gen 'HEAP-VAR
                             (env-get env1 v)
                             v)
                      (gen 'LOCAL-VAR
                           (env-get env1 v)
                           (+ 1 (decf peek-offset)))))))
    code))

(defun revert-lambda-list (lambda-list)
  (mapcar (lambda (x)
            (if (lambda-symbol-p x)
                x
              (property x 'name)))
          (append (first lambda-list)
                  (if (second lambda-list)
                      (cons '&optional (mapcar #'car (second lambda-list))))
                  (if (third lambda-list)
                      (list '&rest (third lambda-list))))))

(defun codegen-lambda-load-env (heap-vars)
  (let ((code nil))
    (dolist (e heap-vars)
      (setq code
            (genseq code
                    (gen 'HEAP-VAR
                         (cdr e)
                         (car e)))))
    code))

(defun lambda-list-min-max (lambda-list)
  (let* ((min (length (first lambda-list)))
         (max (if (third lambda-list)
                  nil
                (+ min (length (second lambda-list))))))
    (cons min max)))

(defun codegen-lambda-internal (ctx lambda-list body env)
  (let* ((arg-vars (lambda-list-vars lambda-list))
         (num-args (length arg-vars))
         (env1 (codegen-make-env1 ctx arg-vars))
         (prev-heap-vars (context-heap-vars ctx)))
    (let ((body-code (codegen ctx body env1)))
      (let* ((extend-env-code (codegen-extend-env env1))
             (load-env-code (codegen-lambda-load-env (context-heap-vars ctx))))
        (let* ((lambda-list-code (codegen-lambda-list arg-vars env1 num-args))
               (min-max (lambda-list-min-max lambda-list))
               (min (car min-max))
               (max (cdr min-max)))
          (let ((function
                 (create (class is-function)
                         'lambda-list lambda-list
                         'label (gen-uniq ctx "F")
                         'code (genseq (gen 'ARGS min max
                                            (mapcar (lambda (opt) (codegen ctx (second opt) nil))
                                                    (second lambda-list)))
                                       (gen 'BEGIN)
                                       extend-env-code
                                       lambda-list-code
                                       load-env-code
                                       body-code
                                       (when (/= 0 num-args) (gen 'NIP num-args))
                                       (gen 'END)
                                       (gen 'RETURN)))))
            (codegen-add-function ctx function)
            (setf (context-heap-vars ctx) prev-heap-vars)
            function))))))

(defun codegen-lambda (ctx lambda-list body env)
  (let ((function (codegen-lambda-internal ctx lambda-list body env)))
    (gen 'CLOSE function)))

(defun codegen-defun (ctx name lambda-list body env)
  (let ((function (codegen-lambda-internal ctx lambda-list body env)))
    (setf (is-function-name function) name)
    (gen 'CONST name)))

(defun codegen-labels (ctx fnames definitions body env)
  (let ((functions
         (mapcar (lambda (def)
                   (codegen-lambda-internal ctx
                                            (second def)
                                            (third def)
                                            env))
                 definitions)))
    (mapc (lambda (fname function)
            (set-property function fname 'function))
          fnames
          functions)
    (codegen ctx body env)))

(defun codegen-if (ctx test then else env)
  (let ((label1 (codegen-newlabel ctx))
        (label2 (codegen-newlabel ctx)))
    (genseq (codegen ctx test env)
            (gen 'JUMP-IF-FALSE label1)
            (codegen ctx then env)
            (gen 'JUMP label2)
            (gen 'LABEL label1)
            (codegen ctx else env)
            (gen 'LABEL label2))))

(defun codegen-progn (ctx forms env)
  (cond ((null forms)
         (gen 'CONST nil))
        ((null (cdr forms))
         (codegen ctx (car forms) env))
        (t
         (genseq (codegen ctx (car forms) env)
                 (gen 'POP)
                 (codegen-progn ctx (cdr forms) env)))))

(defun codegen-tagbody (ctx tags forms env)
  (let ((env (append (mapcar (lambda (tag)
                               (let ((ctag (gen-uniq ctx "TAG_")))
                                 (set-property ctag tag 'tag)
                                 (cons tag ctag)))
                             tags)
                     env)))
    (let ((code (genseq (mapcan (lambda (form)
                                  (if (symbolp form)
                                      (gen 'LABEL (env-get env form))
                                    (genseq (codegen ctx form env)
                                            (gen 'POP))))
                                forms)
                        (gen 'CONST nil))))
      (let ((n 0)
            (longtags nil))
        (dolist (tag tags)
          (when (property tag 'longjmp-p)
            (set-property (incf n) tag 'longjmp-value)
            (push tag longtags)))
        (setq longtags (nreverse longtags))
        (if longtags
            (genseq (gen 'TAGBODY-BEGIN longtags)
                    code
                    (gen 'TAGBODY-END longtags))
          code)))))

(defun codegen-go (ctx tag env)
  (let ((res (env-get env tag)))
    (cond (res
           (gen 'JUMP res))
          (t
           (set-property t tag 'longjmp-p)
           (gen 'GO tag)))))

(defun codegen-block (ctx name body env)
  (let* ((label (gen-uniq "NAME_"))
         (code
          (codegen ctx
                   body
                   (append (cons name label)
                           env))))
    (cond ((property name 'longjmp-p)
           (genseq (gen 'BLOCK-BEGIN name)
                   code
                   (gn 'BLOCK-END name)))
          ((property name 'block-name-used-p)
           (genseq code
                   (gen 'LABEL label)))
          (t
           code))))

(defun codegen-return-from (ctx name body env)
  (let ((res (env-get env name)))
    (cond (res
           (set-property t name 'block-name-used-p)
           (genseq (codegen ctx body env)
                   (gen 'JUMP res)))
          (t
           (set-property t name 'longjmp-p)
           (gen 'RETURN-FROM name)))))

(defun codegen-catch (ctx tag-form body env)
  (genseq (codegen ctx tag-form env)
          (gen 'CATCH-BEGIN)
          (codegen ctx body env)
          (gen 'CATCH-END)))

(defun codegen-throw (ctx tag-form body env)
  (genseq (codegen ctx body env)
          (codegen ctx tag-form env)
          (gen 'THROW)))

(defun codegen-unwind-protect (ctx body cleanup-form env)
  (let ((function (codegen-lambda-internal ctx (list nil nil nil) cleanup-form env)))
    (genseq (gen 'UNWIND-BEGIN function)
            (codegen ctx body env)
            (gen 'UNWIND-END function))))

(defun codegen-call (ctx func args env local)
  (let ((code nil)
        (length 0))
    (dolist (arg args)
      (incf length)
      (setq code (genseq code (codegen ctx arg env))))
    (genseq code
            (if local
                (gen 'LCALL func length)
                (gen 'CALL func length)))))


(defun instr-op (instr)
  (first instr))

(defun instr-arg1 (instr)
  (second instr))

(defun instr-arg2 (instr)
  (third instr))

(defun instr-arg3 (instr)
  (fourth instr))

(defdynamic *cc-stream* nil)
(defdynamic *cc-indent-offset* 0)

(defun cc-format (indent string &rest args)
  (format (dynamic *cc-stream*) (create-string (+ indent (dynamic *cc-indent-offset*)) (convert 9 <character>)))
  (apply #'format (dynamic *cc-stream*) string args)
  (format (dynamic *cc-stream*) "~%"))

(defun cc-list-to-string (list prefix-str separator-str)
  (let ((str ""))
    (for ((rest list (cdr rest)))
         ((null rest))
      (setq str
            (if (cdr rest)
                (string-append prefix-str str (convert (car rest) <string>) separator-str)
                (string-append prefix-str str (convert (car rest) <string>)))))
    str))

(defun cc-add-const (ctx value)
  (dolist (elt (context-constant-list ctx)
               (let ((v (gen-uniq ctx "C_")))
                 (push (cons value v) (context-constant-list ctx))
                 v))
    (when (equal value (car elt))
      (return (cdr elt)))))

(defun cc-top (ctx code)
  (let* ((body-str (cc-body ctx code))
         (head-str (cc-head ctx)))
    (string-append head-str body-str)))

(defun cc-head (ctx)
  (dynamic-let ((*cc-stream* (create-string-output-stream)))
    (cc-format 0 "#include \"lisp.h\"")
    (dolist (c (context-constant-list ctx))
      (cc-format 0 "static ISObject ~A;" (cdr c)))
    (dolist (c (context-jmpbuf-vars ctx))
      (cc-format 0 "static jmp_buf ~A;" c))
    (dolist (f (context-functions ctx))
      (cc-format 0 "static void ~A(int);" (is-function-label f)))
    (get-output-stream-string (dynamic *cc-stream*))))

(defun cc-body (ctx code)
  (dynamic-let ((*cc-stream* (create-string-output-stream)))
               (dolist (f (context-functions ctx))
                 (cc-function ctx f))
               (cc-toplevel ctx code)
               (cc-loader ctx)
               (cc-main)
               (get-output-stream-string (dynamic *cc-stream*))))

(defun cc-main ()
  (cc-format 0 "int main(void)~%{")
  (cc-format 1 "is_init();")
  (cc-format 1 "loader();")
  (cc-format 1 "toplevel();")
  (cc-format 0 "}"))

(defun cc-toplevel (ctx code)
  (cc-format 0 "static void toplevel(void)~%{")
  (cc-code ctx code)
  (cc-format 0 "}"))

(defun cc-loader (ctx)
  (cc-format 0 "void loader(void)~%{")
  (cc-format 1 "is_gc_disable();")
  (dolist (c (context-constant-list ctx))
    (let ((var (cdr c))
          (value (car c)))
      (cc-loader-const ctx var value)))
  (dolist (f (context-functions ctx))
    (when (is-function-name f)
      (let ((var (cc-add-const ctx (is-function-name f))))
        (cc-format 1
                   "is_symbol_set_function(~A, is_make_user_function(&~A, ~A));"
                   var
                   var
                   (is-function-label f)))))
  (dolist (c (context-constant-list ctx))
    (cc-format 1 "is_shelter_add(&~A);" (cdr c)))
  (cc-format 1 "is_gc_enable();")
  (cc-format 0 "}"))

(defun cc-loader-const (ctx var value)
  (let ((tmp-vars nil))
    (labels ((f (var value)
	       (cond ((null value)
		      (cc-format 1 "~A = is_nil;" var)
		      var)
		     ((symbolp value)
                      (if (ignore-errors (convert value <string>) t) ; value is not gensym
                          (let ((tmp (gen-uniq ctx "TMP_")))
                            (push tmp tmp-vars)
                            (setq tmp (f tmp (convert value <string>)))
                            (cc-format 1 "~A = is_intern(&~A);" var tmp)
                            var)
                        (progn
                          (cc-format 1 "~A = is_gensym();" var)
                          var)))
		     ((integerp value)
		      (cc-format 1 "~A = is_make_integer(~A);" var value)
		      var)
		     ((floatp value)
		      (cc-format 1 "~A = is_make_float(~A);" var value)
		      var)
		     ((stringp value)
                      (string-append var
                                     " = is_make_string(\""
                                     value
                                     "\");")
		      var)
                     ((characterp value)
                      (cc-format 1 "~A = is_make_character(~D);" var (convert value <integer>))
                      var)
		     ((consp value)
		      (let ((car-var (f (gen-uniq ctx "TMP_") (car value)))
			    (cdr-var (f (gen-uniq ctx "TMP_") (cdr value))))
			(push car-var tmp-vars)
			(push cdr-var tmp-vars)
			(cc-format 1 "~A = is_make_cons(&~A, &~A);" var car-var cdr-var)
			var))
		     (t
		      (error "unexpected const value: ~A" value)))))
      (let ((str
	      (dynamic-let ((*cc-stream* (create-string-output-stream)))
		(f var value)
		(get-output-stream-string (dynamic *cc-stream*)))))
	(dolist (tmp tmp-vars)
	  (cc-format 1 "ISObject ~A;" tmp))
	(format (dynamic *cc-stream*) str)))))

(defun cc-function (ctx function)
  (cc-add-const ctx (is-function-name function))
  (cc-format 0 "static void ~A(int argc)~%{" (is-function-label function))
  (cc-code ctx (is-function-code function))
  (cc-format 0 "}"))

(defun cc-code (ctx code)
  (dolist (instr code)
    (cc-instr ctx instr)))


(defglobal *instr-indicator* (gensym))

(defmacro define-instruction (instr-op args &rest body)
  (let ((gname (gensym))
        (ginstr (gensym)))
    `(progn
       (defun ,gname (,(car args) ,ginstr)
         (let ,(mapcar (lambda (arg accessor)
                         `(,arg (,accessor ,ginstr)))
                       (cdr args)
                       '(instr-arg1 instr-arg2 instr-arg3))
           ,@body))
       (set-property #',gname ',instr-op *instr-indicator*))))

(define-instruction CONST (ctx arg1)
  (cc-format 1 "is_stack_push(~A);"
             (cc-add-const ctx arg1)))

(define-instruction FUNCTION (ctx arg1)
  (cc-format 1 "is_stack_push(is_symbol_function(~A));"
             (cc-add-const ctx arg1)))

(define-instruction LOCAL-FUNCTION (ctx arg1)
  (cc-format 1 "is_stack_push(is_make_closure(~A))"
             (is-function-label
              (property arg1 'function))))

(define-instruction GREF (ctx arg1)
  (cc-format 1 "is_stack_push(is_symbol_global(~A));"
             (cc-add-const ctx arg1)))

(define-instruction GSET (ctx arg1)
  (cc-format 1 "is_symbol_set_global(~A, is_stack_peek(1));"
             (cc-add-const ctx arg1)))

(define-instruction CALL (ctx arg1 arg2)
  (cc-format 1 "is_call(~A, ~A);"
             (cc-add-const ctx arg1)
             arg2))

(define-instruction LCALL (ctx arg1 arg2)
  (cc-format 1 "~A(~A);"
             (is-function-label (property arg1 'function))
             arg2))

(define-instruction ARGS (ctx min max default-code-list)
  (cond ((eql min max)
         (cc-format 1 "if (argc != ~A) is_argc_error();" min))
        ((null max)
         (cc-format 1 "if (argc < ~A) is_argc_error();" min)
         (cc-format 1 "is_stack_build_list(argc-~A);" min))
        (t
         (cc-format 1 "switch (argc) {")
         (dotimes (i min)
           (cc-format 2 "case ~A:" i))
         (let ((i min))
           (dolist (code default-code-list)
             (cc-format 2 "case ~A:" i)
             (dynamic-let ((*cc-indent-offset*
                            (+ 2 (dynamic *cc-indent-offset*))))
                          (cc-code ctx code))
             (incf i))
           (cond ((null max)
                  (cc-format 3 "is_stack_push(nil);")
                  (cc-format 3 "break;")
                  (cc-format 2 "default:")
                  (cc-format 3 "is_stack_build_list(argc-~A);" (+ min (length default-code-list))))
                 (t
                  (cc-format 2 "case ~A:" i)
                  (cc-format 3 "break;")
                  (cc-format 2 "default:")
                  (cc-format 3 "is_argc_error();")))
           (cc-format 1 "}")))))

(define-instruction EXTEND-ENV (ctx n peeks)
  (cc-format 1
             "is_env_extend(~A, ~A);"
             n
             (cc-list-to-string (reverse peeks) "" ", ")))

(define-instruction CLOSE (ctx arg1)
  (cc-format 1 "is_stack_push(is_make_closure(~A));"
             (is-function-label arg1)))

(define-instruction HEAP-VAR (ctx arg1 arg2)
  (cc-format 1 "ISObject *~A = is_env_get(~A);"
             arg1
             (property arg2 'env-offset)))

(define-instruction LOCAL-VAR (ctx arg1 arg2)
 (cc-format 1 "ISObject *~A = is_stack_peek_ptr(~A);"
            arg1 arg2))

(define-instruction LREF (ctx arg1)
  (cc-format 1 "is_stack_push(*~A);" arg1))

(define-instruction LSET (ctx arg1)
  (cc-format 1 "*~A = is_stack_peek(1);" arg1))

(define-instruction NIP (ctx arg1)
  (cc-format 1 "is_stack_nip(~A);" arg1))

(define-instruction POP (ctx)
  (cc-format 1 "is_stack_pop();"))

(define-instruction JUMP-IF-FALSE (ctx arg1)
  (cc-format 1 "if (is_stack_top_null()) goto ~A;" arg1))

(define-instruction JUMP (ctx arg1)
  (cc-format 1 "goto ~A;" arg1))

(define-instruction LABEL (ctx arg1)
  (cc-format 0 "~A:;" arg1))

(define-instruction TAGBODY-BEGIN (ctx tags)
  (cc-format 1 "{")
  (let ((tag-array-var (gen-uniq ctx "tag_array_"))
        (jmpbuf-var (gen-uniq ctx "jmpbuf_")))
    (push jmpbuf-var (context-jmpbuf-vars ctx))
    (dolist (tag tags) (set-property jmpbuf-var tag 'jmpbuf))
    (cc-format 2 "void *~A[] = {~A};"
               tag-array-var
               (cc-list-to-string
                (mapcar (lambda (tag)
                          (property tag 'tag))
                        tags)
                "&&" ", "))
    (let ((tmp-var (gen-uniq ctx "tmp_")))
      (cc-format 2 "int ~A = is_setjmp(&~A);" tmp-var jmpbuf-var)
      (cc-format 2 "if (~A != 0) goto *~A[~A-1];" tmp-var tag-array-var tmp-var))
    (incf (dynamic *cc-indent-offset*))))

(define-instruction TAGBODY-END (ctx tags)
  (decf (dynamic *cc-indent-offset*))
  (cc-format 1 "}"))

(define-instruction GO (ctx name)
  (let ((v (cc-add-const ctx (property name 'name))))
    (cc-format 1 "is_longjmp_go(~A, ~A, ~A);"
               (property name 'jmpbuf)
               (property name 'longjmp-value)
               v)))

(define-instruction BLOCK-BEGIN (ctx name)
  (cc-format 1 "{")
  (let ((jmpbuf-var (gen-uniq ctx "jmpbuf_")))
    (push jmpbuf-var (context-jmpbuf-vars ctx))
    (cc-format 2 "if (is_setjmp(&~A) == 0) {" jmpbuf-var)
    (incf (dynamic *cc-indent-offset*))))

(define-instruction BLOCK-END (ctx name)
  (decf (dynamic *cc-indent-offset*))
  (cc-format 1 "}"))

(define-instruction RETURN-FROM (ctx name)
  (let ((v (cc-add-const ctx (property arg1 'name))))
    (cc-format 1 "is_longjmp_return_from(~A, ~A);"
               (property name 'jmpbuf)
               v)))

(define-instruction CATCH-BEGIN (ctx)
  (cc-format 1 "is_catch_begin();"))

(define-instruction CATCH-END (ctx)
  (cc-format 1 "is_catch_end();"))

(define-instruction THROW (ctx)
  (cc-format 1 "is_throw();"))

(define-instruction UNWIND-BEGIN (ctx function)
  (cc-format 1 "is_unwind_begin(~A);" (is-function-label function)))

(define-instruction UNWIND-END (ctx function)
  (cc-format 1 "is_unwind_end();"))

(define-instruction BEGIN (ctx)
  (cc-format 1 "{")
  (incf (dynamic *cc-indent-offset*)))

(define-instruction END (ctx)
  (decf (dynamic *cc-indent-offset*))
  (cc-format 1 "}"))

(define-instruction RETURN (ctx)
  (cc-format 1 "return;"))

(defun cc-instr (ctx instr)
  (let ((emit (property (instr-op instr) *instr-indicator*)))
    (if emit
        (funcall emit ctx instr)
      (error "unknow instruction: ~A" (instr-op instr)))))


(defglobal *output-file* "../runtime/OUTPUT.c")
;(defglobal *output-file* nil)

(defun is-compile-file (&rest filenames)
  (let ((ctx (create (class context)))
        (code nil))
    (dolist (filename filenames)
      (with-open-input-file (in filename)
                            (for ((x (read in nil) (read in nil)))
                                 ((null x))
                                 (setq code
                                       (genseq code
                                               (codegen ctx (pass1 x) nil)
                                               (gen 'POP))))))
    (if (null *output-file*)
        (format (standard-output) "~A~%" (cc-top ctx code))
      (with-open-output-file (out *output-file*)
                             (format out "~A~%" (cc-top ctx code))))
    t))

(defun is-compile (x)
  (let* ((ctx (create (class context)))
         (code (codegen ctx (pass1 x) nil)))
    (print-code code)
    (print-context ctx)
    (format (standard-output) "~A" (cc-top ctx code))
    t))

(defun make ()
  (is-compile-file "../lisp/predicate.lsp"
                   "../lisp/control.lsp"
                   "../lisp/list.lsp"
                   "../lisp/number.lsp"
                   "../lisp/read.lsp"))

(make)
