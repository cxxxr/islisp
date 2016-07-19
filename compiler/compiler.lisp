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

(defmacro push (obj place)
  `(setf ,place (cons ,obj ,place)))

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

(defun make-var (sym)
  (let ((gsym (gensym)))
    (set-property sym gsym 'name)
    gsym))

(defun pass1 (x env)
  (cond ((symbolp x)
         (pass1-refvar x env))
        ((not (consp x))
         (make-ast 'CONST x))
        (t
         (pass1-compound-form x env))))

(defun pass1-get-var (s env)
  (let ((v (assoc s env)))
    (if v
	(cdr v)
	nil)))

(defun pass1-refvar (s env)
  (let ((v (pass1-get-var s env)))
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
	 (check-lambda-list form (cadr form))
	 t)))

(defun lambda-rest-symbol-p (x)
  (or (eq x ':rest)
      (eq x '&rest)))

(defun check-lambda-list (form lambda-list)
  (unless (and (listp lambda-list)
               (for ((l lambda-list (cdr l)))
                    ((null l) t)
                 (let ((x (car l)))
                   (cond ((not (symbolp x))
                          (return nil))
                         ((lambda-rest-symbol-p x)
                          (unless (and (= 1 (length (cdr l)))
                                       (symbolp (cadr l))
                                       (not (lambda-rest-symbol-p (cadr l))))
                            (return nil)))))))
    (syntax-error "Illegal lambda list: ~A" form)))

(defun replace-list-with-alist (alist list)
  (mapcar (lambda (x)
	    (let ((v (assoc x alist)))
	      (if v (cdr v) x)))
          list))

(defun pass1-funcbody (form x env make-ast-function)
  (let ((lambda-list (car x)))
    (check-lambda-list form lambda-list)
    (let* ((vars (remove-if #'lambda-rest-symbol-p
                            lambda-list))
           (env1 (mapcar (lambda (v)
                           (cons v (make-var v)))
                         vars)))
      (funcall make-ast-function
               (replace-list-with-alist env1 lambda-list)
               (pass1 `(progn ,@(cdr x))
                      (append env1 env))))))

(defun dset-lambda-list (form lambda-form args)
  (let ((bindings nil))
    (for ((l (cadr lambda-form) (cdr l))
          (a args (cdr a)))
         ((or (null l) (null a))
          (when (or (and (null l) a)
                    (and l (null a)))
            (syntax-error "Invalid number of argnument: ~A" form)))
      (push (if (lambda-rest-symbol-p (car l))
                (progn
                  (push (list (cadr l) (cons 'list a)) bindings)
                  (return nil))
                (list (car l) (car a)))
            bindings))
    (nreverse bindings)))

(defun pass1-compound-form (x env)
  (case (car x)
        ((quote)
         (check-arg-count x 1 1)
         (make-ast 'CONST (second x)))
        ((setq)
         (check-arg-count x 2 2)
         (unless (symbolp (second x))
           (type-error x (second x) '<symbol>))
         (let ((var (pass1-get-var (second x) env))
               (val (pass1 (third x) env)))
           (if var
               (make-ast 'SET-LVAR var val)
             (make-ast 'SET-GVAR (second x) val))))
        ((let)
         (let* ((binds
                 (mapcar (lambda (b)
                           (unless (and (consp b)
                                        (= 2 (length b))
                                        (symbolp (car b)))
                             (syntax-error "Illegal let form ~A" x))
                           (list (make-var (cadr b))
                                 (pass1 (cadr b) env)))
                         (cadr x)))
                (body
                 (when (cddr x)
                   (pass1 `(progn ,@(cddr x))
                          (append (mapcar (lambda (x y)
                                            (cons (car x)
                                                  (car y)))
                                          (cadr x)
                                          binds)
                                  env)))))
           (make-ast 'LET binds body)))
        ((function)
         (check-arg-count x 1 1)
         (let ((arg (cadr x)))
           (cond ((symbolp arg)
                  (make-ast 'FUNCTION arg))
                 ((lambda-form-p arg)
                  arg)
                 (t
                  (syntax-error "Illegal form ~A" x)))))
        ((lambda)
         (check-arg-count x 1 -1)
         (pass1-funcbody x (cdr x) env
                         (lambda (lambda-list body)
                           (make-ast 'LAMBDA lambda-list body))))
        ((defun)
         (check-arg-count x 2 -1)
         (unless (symbolp (cadr x))
           (type-error x (cadr x) '<symbol>))
         (pass1-funcbody x (cddr x) env
                         (lambda (lambda-list body)
                           (make-ast 'DEFUN (cadr x) lambda-list body))))
        ((if)
         (check-arg-count x 2 3)
         (make-ast 'IF
                   (pass1 (second x) env)
                   (pass1 (third x) env)
                   (pass1 (fourth x) env)))
        ((progn)
         (make-ast 'PROGN
                   (mapcar (lambda (x) (pass1 x env))
                           (cdr x))))
        (t
         (cond ((lambda-form-p (car x))
                (let* ((lambda-form (car x))
                       (args (cdr x))
                       (bindings (dset-lambda-list x lambda-form args)))
                  (pass1 `(let ,bindings
                            ,@(cddr lambda-form))
                         env)))
               ((symbolp (car x))
                (make-ast 'CALL
                          (car x)
                          (mapcar (lambda (arg)
                                    (pass1 arg env))
                                  (cdr x))))
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
    :accessor is-function-code)
   (min
    :initarg min
    :accessor is-function-min)
   (max
    :initarg max
    :accessor is-function-max)))

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
    :accessor context-constant-list)))

(defun print-code (code)
  (dolist (instr code)
    (format (standard-output) "~A~%" instr)))

(defun print-context (ctx)
  (dolist (f (context-functions ctx))
    (format (standard-output)
            "~%~A(~A) ~A:~%"
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

(defun codegen-env-get (env sym)
  (let ((v (assoc sym env)))
    (if v
        (cdr v)
        nil)))

(defun codegen (ctx x env)
  (case (ast-op x)
    ((CONST)
     (gen 'CONST (ast-arg1 x)))
    ((FUNCTION)
     (gen 'FUNCTION (ast-arg1 x)))
    ((REF-GVAR)
     (gen 'GREF (ast-arg1 x)))
    ((SET-GVAR)
     (genseq (codegen ctx (ast-arg2 x) env)
             (gen 'GSET (ast-arg1 x))))
    ((REF-LVAR)
     (let ((var (codegen-env-get env (ast-arg1 x))))
       (if var
           (gen 'LREF var)
           (let ((var (codegen-add-heap-var ctx (ast-arg1 x))))
             (gen 'LREF var)))))
    ((SET-LVAR)
     (let ((var (codegen-env-get env (ast-arg1 x))))
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
    ((IF)
     (codegen-if ctx (ast-arg1 x) (ast-arg2 x) (ast-arg3 x) env))
    ((PROGN)
     (codegen-progn ctx (ast-arg1 x) env))
    ((CALL)
     (codegen-call ctx (ast-arg1 x) (ast-arg2 x) env))
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
        (genseq (mapcan (lambda (b)
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
                (gen 'NIP stack-push-count))))))

(defun codegen-lambda-list (lambda-list env1 num-args)
  (let ((peek-offset num-args)
        (code nil)
        (min 0)
        (max 0))
    (for ((l lambda-list (cdr l)))
         ((null l))
      (cond ((lambda-rest-symbol-p (car l))
             (setq code
                   (genseq code
                           (if (property (cadr l) 'heap-p)
                               (gen 'HEAP-VAR
                                    (codegen-env-get env1 (cadr l))
                                    (cadr l))
                               (gen 'LOCAL-VAR
                                    (codegen-env-get env1 (cadr l))
                                    (+ 1 (decf peek-offset))))))
             (setq max nil)
             (return nil))
            (t
             (setq code
                   (genseq code
                           (if (property (car l) 'heap-p)
                               (gen 'HEAP-VAR
                                    (codegen-env-get env1 (car l))
                                    (car l))
                               (gen 'LOCAL-VAR
                                    (codegen-env-get env1 (car l))
                                    (+ 1 (decf peek-offset))))))))
      (incf min)
      (incf max))
    (list code min max)))

(defun revert-lambda-list (lambda-list)
  (mapcar (lambda (x)
            (or (property x 'name) x))
          lambda-list))

(defun codegen-lambda-load-env (heap-vars)
  (let ((code nil))
    (dolist (e heap-vars)
      (setq code
            (genseq code
                    (gen 'HEAP-VAR
                         (cdr e)
                         (car e)))))
    code))

(defun codegen-filter-lambda-list-vars (lambda-list)
  (remove-if #'lambda-rest-symbol-p lambda-list))

(defun codegen-lambda-internal (ctx lambda-list body env)
  (let* ((arg-vars (codegen-filter-lambda-list-vars
                    lambda-list))
         (num-args (length arg-vars))
         (env1 (codegen-make-env1 ctx arg-vars))
         (prev-heap-vars (context-heap-vars ctx)))
    (let ((body-code (codegen ctx body env1)))
      (let* ((extend-env-code (codegen-extend-env env1))
             (load-env-code (codegen-lambda-load-env (context-heap-vars ctx))))
        (let* ((_vals (codegen-lambda-list lambda-list env1 num-args))
               (lambda-list-code (first _vals))
               (min (second _vals))
               (max (third _vals)))
          (let ((function
                 (create (class is-function)
                         'lambda-list (revert-lambda-list lambda-list)
                         'label (gen-uniq ctx "F")
                         'code (genseq (gen 'ARGS min max)
                                       extend-env-code
                                       lambda-list-code
                                       load-env-code
                                       body-code
                                       (when (/= 0 num-args) (gen 'NIP num-args))
                                       (gen 'RETURN))
                         'min min
                         'max max)))
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

(defun codegen-call (ctx func args env)
  (let ((code nil)
        (length 0))
    (dolist (arg args)
      (incf length)
      (setq code (genseq code (codegen ctx arg env))))
    (genseq code (gen 'CALL func length))))


(defun instr-op (instr)
  (first instr))

(defun instr-arg1 (instr)
  (second instr))

(defun instr-arg2 (instr)
  (third instr))

(defdynamic *cc-stream* nil)

(defun cc-format (indent string &rest args)
  (format (dynamic *cc-stream*) (create-string indent (convert 9 <character>)))
  (apply #'format (dynamic *cc-stream*) string args)
  (format (dynamic *cc-stream*) "~%"))

(defun cc-list-to-string (list)
  (let ((str ""))
    (for ((rest list (cdr rest)))
         ((null rest))
      (setq str
            (if (cdr rest)
                (string-append str (convert (car rest) <string>) ", ")
                (string-append str (convert (car rest) <string>)))))
    str))

(defun cc-add-const (ctx value)
  (dolist (elt (context-constant-list ctx)
               (let ((v (gen-uniq ctx "C_")))
                 (push (cons value v) (context-constant-list ctx))
                 v))
    (when (equal value (car elt))
      (return (cdr elt)))))

(defun cc-top (ctx)
  (let* ((body-str (cc-body ctx))
         (head-str (cc-head ctx)))
    (string-append head-str body-str)))

(defun cc-head (ctx)
  (dynamic-let ((*cc-stream* (create-string-output-stream)))
    (cc-format 0 "#include \"lisp.h\"")
    (dolist (c (context-constant-list ctx))
      (cc-format 0 "static ISObject ~A;" (cdr c)))
    (dolist (f (context-functions ctx))
      (cc-format 0 "static void ~A(int);" (is-function-label f)))
    (get-output-stream-string (dynamic *cc-stream*))))

(defun cc-body (ctx)
  (dynamic-let ((*cc-stream* (create-string-output-stream)))
    (dolist (f (context-functions ctx))
      (cc-function ctx f))
    (cc-loader ctx)
    (get-output-stream-string (dynamic *cc-stream*))))

(defun cc-loader (ctx)
  (cc-format 0 "static void loader(void)~%{")
  (cc-format 1 "is_gc_disable();")
  (dolist (c (context-constant-list ctx))
    (let ((var (cdr c))
          (value (car c)))
      (cc-loader-const ctx var value)))
  (dolist (f (context-functions ctx))
    (when (is-function-name f)
      (let ((var (cc-add-const ctx (is-function-name f))))
        (cc-format 1
                   "is_symbol_set_function(~A, is_make_user_function(~A));"
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
		      (let ((tmp (gen-uniq ctx "TMP_")))
			(push tmp tmp-vars)
			(setq tmp (f tmp (convert value <string>)))
			(cc-format 1 "~A = is_intern(&~A);" var tmp)
			var))
		     ((integerp value)
		      (cc-format 1 "~A = is_make_integer(~A);" var value)
		      var)
		     ((floatp value)
		      (cc-format 1 "~A = is_make_float(~A);" var value)
		      var)
		     ((stringp value)
		      (cc-format 1 "~A = is_make_string(\"~A\");" var value)
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
  (dolist (instr (is-function-code function))
    (cc-instr ctx function instr))
  (cc-format 0 "}"))

(defun cc-instr (ctx function instr)
  (case (instr-op instr)
    ((CONST)
     (let ((v (cc-add-const ctx (instr-arg1 instr))))
       (cc-format 1 "is_stack_push(~A);" v)))
    ((FUNCTION)
     (let ((v (cc-add-const ctx (instr-arg1 instr))))
       (cc-format 1 "is_stack_push(is_function(~A));" v)))
    ((GREF)
     (let ((v (cc-add-const ctx (instr-arg1 instr))))
       (cc-format 1 "is_stack_push(is_symbol_value(~A));" v)))
    ((GSET)
     (let ((v (cc-add-const ctx (instr-arg1 instr))))
       (cc-format 1 "is_setq(~A, is_stack_peek(1));" v)))
    ((CALL)
     (cc-format 1 "is_call(~A, ~A);"
                (cc-add-const ctx (instr-arg1 instr))
                (instr-arg2 instr)))
    ((ARGS)
     (let ((min (instr-arg1 instr))
           (max (instr-arg2 instr)))
       (cond ((null max)
              (cc-format 1 "if (argc < ~A) is_argc_error();" min)
              (cc-format 1 "is_stack_build_list(argc-~A);" min))
             ((= min max)
              (cc-format 1 "if (argc != ~A) is_argc_error();" min))
             (t
              (error "unsupported optional parameter")))))
    ((EXTEND-ENV)
     (let ((n (instr-arg1 instr))
           (peeks (instr-arg2 instr)))
       (cc-format 1
		  "is_env_extend(~A, ~A);"
		  n
		  (cc-list-to-string (reverse peeks)))))
    ((CLOSE)
     (cc-format 1 "is_stack_push(is_make_closure(~A));" (is-function-label (instr-arg1 instr))))
    ((HEAP-VAR)
     (cc-format 1 "ISObject *~A = is_env_get(~A);"
                (instr-arg1 instr)
                (property (instr-arg2 instr) 'env-offset)))
    ((LOCAL-VAR)
     (cc-format 1 "ISObject *~A = is_stack_peek_ptr(~A);"
                (instr-arg1 instr)
                (instr-arg2 instr)))
    ((LREF)
     (cc-format 1 "is_stack_push(*~A);" (instr-arg1 instr)))
    ((LSET)
     (cc-format 1 "*~A = is_stack_peek(1);" (instr-arg1 instr)))
    ((NIP)
     (cc-format 1 "is_stack_nip(~A);" (instr-arg1 instr)))
    ((POP)
     (cc-format 1 "is_stack_pop();"))
    ((JUMP-IF-FALSE)
     (cc-format 1 "if (is_stack_top_null()) goto ~A;" (instr-arg1 instr)))
    ((JUMP)
     (cc-format 1 "goto ~A;" (instr-arg1 instr)))
    ((LABEL)
     (cc-format 0 "~A:" (instr-arg1 instr)))
    ((RETURN)
     (cc-format 1 "return;"))
    (t
     (error "unknown instruction: ~A" (instr-op instr)))))



(defun is-compile-file (filename)
  (with-open-input-file (in filename)
    (let ((ctx (create (class context)))
          (code nil))
      (for ((x (read in nil) (read in nil)))
           ((null x))
        (setq code
              (genseq code
                      (codegen ctx (pass1 x nil) nil))))
      )))

(defun is-compile (x)
  (let* ((ctx (create (class context)))
         (code (codegen ctx (pass1 x nil) nil)))
    (print-code code)
    (print-context ctx)
    (format (standard-output) "~A" (cc-top ctx))
    t))
